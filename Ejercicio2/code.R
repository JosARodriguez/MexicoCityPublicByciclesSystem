# Instalamos las librerías necesarias ####

paquetes_instalados <- as.data.frame(installed.packages()[ , c(1, 3:4)])          
paquetes_instalados <- paquetes_instalados[is.na(paquetes_instalados$Priority), 1:2, drop = FALSE] 
rownames(paquetes_instalados) <- NULL                                              

paquetes_requeridos = c('dplyr','lubridate','ggplot2','forecast','zoo')

# Revisamos si los paquetes requeridos están en los paquetes instalados, y si no
# los instalamos para poder usarlos

for (i in paquetes_requeridos) {
  if (!(i %in% paquetes_instalados$Package)) {
    install.packages(i)
  }
}

#Cargamos los paquetes

library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo)

# Cargamos los datos, a estos datos ya se les había dado un preprocesamiento previo
# en el formato de fecha y en pasar los valores a númericos
datos = read.csv(paste0(getwd(),'/tipoCambio.csv'))

# Pasamos la fecha al formato de fecha
datos$Fecha = as.Date(datos$Fecha)
# Llenamos los valores nulos y renombramos la variable
datos$FX = na.locf(datos$Valor)
datos = datos %>% select(-Valor)


#Quitamos datos despues de noviembre 2021 y antes de 2012
datos_serie_de_tiempo = datos %>% dplyr::filter(Fecha>='2012-01-01',Fecha<='2021-11-30')

# Construimos la serie de tiempo tomando periodos de estacionalidad de 1,4 y 6 años
serie_de_tiempo = msts(datos_serie_de_tiempo$FX,seasonal.periods = c(365,365*4),start = decimal_date(as.POSIXct("2012-01-01")))

# Creamos grafica mostrando la descomposicion de la serie en factores de estacionalidad, tendencia y error
plot_serie_de_tiempo = autoplot(mstl(serie_de_tiempo))

# Creamos el modelo 
fcast_mstl <- serie_de_tiempo %>% stlf() %>% forecast() 

# Creamos el rango de predicciones
fechas_predicciones = seq(as.Date('2021-12-01'),to = as.Date('2022-05-31'), by = 'day')
predicciones = fcast_mstl$mean[1:182]

# Agregamos variable que representa valores reales
datos$Grupo = 'Valor'

# Creamos una tabla con las predicciones
preds = tibble(Fecha = fechas_predicciones,FX = predicciones,Grupo = 'Predicciones')

# Juntamos valores reales y predicciones para graficar resultados
datos_con_predicciones = rbind(datos,preds)

# Grafica de resultados
comparacion = ggplot(datos_con_predicciones,aes(Fecha,FX,colour = Grupo))+geom_point()+labs(title = 'Tipo de cambio peso/dólar')

# Comparamos numericamente valores reales y observados (de noviembre 2021 a marzo 2022)

datos_reales = datos %>% filter(Fecha>="2021-12-01")
datos_comparacion = left_join(datos_reales %>% select(-Grupo),preds %>% select(-Grupo,predicciones = FX))

comparacion_periodo_final = ggplot(datos_con_predicciones %>% filter(Fecha>"2021-11-30",Fecha<"2022-03-12"),
                                   aes(Fecha,FX,colour = Grupo))+geom_point()+labs(title = 'Tipo de cambio peso/dólar')

# Calculamos la diferencia absoluta y otras métricas relevantes
datos_comparacion$dif_absoluta = abs(datos_comparacion$FX-datos_comparacion$predicciones)
datos_comparacion$dif_absoluta_cuadrada = datos_comparacion$dif_absoluta*datos_comparacion$dif_absoluta
error_en_media_cuadrada = sum(datos_comparacion$dif_absoluta_cuadrada)/nrow(datos_comparacion)
rmse = sqrt(error_en_media_cuadrada)
