# Instalamos las librerías necesarias ####

paquetes_instalados <- as.data.frame(installed.packages()[ , c(1, 3:4)])          
paquetes_instalados <- paquetes_instalados[is.na(paquetes_instalados$Priority), 1:2, drop = FALSE] 
rownames(paquetes_instalados) <- NULL                                              
    
paquetes_requeridos = c('dplyr','lubridate','ggplot2','factoextra','cluster')

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
library(factoextra)
library(cluster)

# Cambiamos el directorio a la carpeta donde está este script para poder realizar
# las lecturas de datos, este paso asume que estamos en el directorio en que se
# encuentra la carpeta 'Ejercicio1'

#setwd(paste0(getwd(),'~/Ejercicio1'))

# Leemos los archivos de datos por mes ####
ecobici_agosto = read.csv(paste0(getwd(),'/datos/ecobici_agosto.csv'))
ecobici_septiembre = read.csv(paste0(getwd(),'/datos/ecobici_septiembre.csv'))
ecobici_octubre = read.csv(paste0(getwd(),'/datos/ecobici_octubre.csv'))

# Juntamos los 3 archivos en uno solo
ecobici = rbind(ecobici_agosto,ecobici_septiembre,ecobici_octubre)

# Consturimos nuevas variables de fecha y hora para cambiarlas de tipo 'string'
# a 'POSIXt' para poder recuperar las horas correspondientes y poder hacer agrupaciones
# de esta forma ####

# Cambiamos formato al formato estandar de fechas de R
ecobici = ecobici %>% mutate(fecha_arribo =as.Date(Fecha.Arribo, format = '%d/%m/%Y'),
                             fecha_retiro =as.Date(Fecha_Retiro, format = '%d/%m/%Y')) %>% 
  # Juntamos la fecha y la hora para tener la informacion en una variable de tipo 'POSIXt'
  mutate(date_time_arribo = as_datetime(paste0(fecha_arribo,Hora_Arribo)),
         date_time_retiro = as_datetime(paste0(fecha_retiro,Hora_Retiro))) %>% 
  # Recuperamos la hora,dia y mes de cada observación así como la duración de uso de las bicicletas
  mutate(hora_arribo = hour(date_time_arribo),
         hora_retiro = hour(date_time_retiro),
         dia_arribo = day(date_time_arribo),
         dia_retiro = day(date_time_retiro),
         mes_arribo = month(date_time_arribo),
         mes_retiro = month(date_time_retiro),
         duracion = round(as.numeric(date_time_arribo-date_time_retiro))) %>% 
  #Renombramos y seleccionamos las variables que vamos a utilizar
  select(genero = Genero_Usuario, edad = Edad_Usuario, bici = Bici,
         estacion_retiro = Ciclo_Estacion_Retiro, estacion_arribo = Ciclo_EstacionArribo,
         fecha_arribo, fecha_retiro, hora_arribo, hora_retiro, dia_arribo, dia_retiro,
         mes_arribo, mes_retiro, duracion) %>% 
  # Finalmente agregamos una variable booleana que indique si la estación de retiro fue
  # la misma que la estación de arribo, escogemos 1 y 0 en lugar de Verdadero y Falso
  # para poder tomar promedios sobre esta variable después
  mutate(misma_estacion = case_when(estacion_arribo==estacion_retiro ~ 1,
                   T ~ 0))


resumen_ecobici<-capture.output(summary(ecobici), file=NULL,append=FALSE)
resumen_ecobici <-as.data.frame(resumen_ecobici)


# Sección para quitar outliers
ecobici = ecobici %>% mutate(z_scores = abs((duracion-mean(duracion))/sd(duracion)))
ecobici_sin_outliers = ecobici %>% filter(z_scores<.5,estacion_retiro<475,estacion_arribo<475,
                                          fecha_arribo>='2021-07-30',
                                          fecha_retiro>='2021-07-30')

resumen_ecobici_sin_outliers<-capture.output(summary(ecobici_sin_outliers), file=NULL,append=FALSE)
resumen_ecobici_sin_outliers <-as.data.frame(resumen_ecobici_sin_outliers)

# Hacemos un resumen por hora de arribo y retiro para encontrar horas con mayor afluencia
arribos_por_hora = ecobici_sin_outliers %>% group_by(hora_arribo) %>%
  summarise(numero_de_arribos = n()) %>% 
  rename(hora = hora_arribo)

retiros_por_hora = ecobici_sin_outliers %>% group_by(hora_retiro) %>%
  summarise(numero_de_retiros = n()) %>% 
  rename(hora = hora_retiro)

usos_por_hora = left_join(arribos_por_hora,retiros_por_hora) %>% 
  mutate(usos = numero_de_arribos+numero_de_retiros)

usos_hora_plot = ggplot(usos_por_hora,aes(hora,usos))+geom_point()+labs(title = 'Afluencia Por Hora')

# Hacemos un resumen por estación para encontrar estaciones con mayor afluencia
arribos_por_estacion = ecobici_sin_outliers %>% group_by(estacion_arribo) %>%
  summarise(numero_de_arribos = n()) %>% 
  rename(estacion = estacion_arribo)

retiros_por_estacion = ecobici_sin_outliers %>% group_by(estacion_retiro) %>%
  summarise(numero_de_retiros = n()) %>% 
  rename(estacion = estacion_retiro)

usos_por_estacion = left_join(arribos_por_estacion,retiros_por_estacion) %>% 
  mutate(usos = numero_de_arribos+numero_de_retiros)


#### INICIA PREGUNTA 2 ####

# Introducimos una función para obtener la moda de un vector, ya que no hay
# una función en R que haga esto

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Normalizamos los datos
ecobici2 = ecobici %>% select(duracion)
df1 <- scale(ecobici2)
km <- kmeans(df1, centers = 2, nstart = 25)
ecobici2$cluster = km$cluster

kmeans_duracion_plot = ggplot(ecobici2,aes(duracion,duracion,colour = cluster))+geom_point()








# Encontramos las modas de los valores por estación
modas_ecobici = ecobici_sin_outliers %>% group_by(estacion = estacion_retiro) %>% 
  summarise(edad = mean(edad),
            hora_arribo = mean(hora_arribo),
            genero = moda(genero))

# Normalizamos los datos
df <- scale(modas_ecobici %>% select(hora_arribo,edad))

# Información sobre el número de grupos
elbow = fviz_nbclust(df, kmeans, method = "wss")+xlab('Número de grupos')+
  ylab('Inercia')+labs(title = 'Inercia por número de grupos')
gap_stat = clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
gap =fviz_gap_stat(gap_stat)+labs(title = 'Número óptimo de grupos')+
  xlab('Número de grupos')+ylab('Estadístico "gap"')

# Hacemos el entrenamiento reproducible
set.seed(1)

km1 <- kmeans(df, centers = 3, nstart = 25)
km2 <- kmeans(df, centers = 6, nstart = 25)

modas_ecobici$cluster1 = as.character(km1$cluster)
modas_ecobici$cluster2 = as.character(km2$cluster)


#Generamos gráficas de hora contra edad mostrando el grupo al que pertenece cada estación

# Gráfica con 3 grupos
tres_clusters = ggplot(modas_ecobici,aes(hora_arribo,edad,colour = cluster1))+geom_point()+
  labs(title = 'Agrupación de Estaciones', subtitle = 'tomando los retiros de las estaciones')+xlab('Media de hora de arribos')+
  ylab('Media de edad')

#Gráfica con 6 grupos
seis_clusters = ggplot(modas_ecobici,aes(hora_arribo,edad,colour = cluster2))+geom_point()+
  labs(title = 'Agrupación de Estaciones', subtitle = 'tomando los retiros de las estaciones')+xlab('Media de hora de arribos')+
  ylab('Media de edad')


#### Inicia pregunta 3


# Recuperamos cada arribo y cada retiro de cada estacion con su fecha
arribos_ecobici = ecobici_sin_outliers %>% select(estacion = estacion_arribo,
                                     fecha = fecha_arribo)

retiros_ecobici = ecobici_sin_outliers %>% select(estacion = estacion_retiro,
                                     fecha = fecha_retiro)

#Juntamos esta información y agrupamos por fecha y estacion para encontrar los 
# usos por estacion por dia

usos_por_estacion_por_dia = rbind(arribos_ecobici,retiros_ecobici) %>% 
  group_by(estacion,fecha) %>% summarise(usos = n()) %>% ungroup() %>% 
  group_by(estacion) %>% mutate(usos_relativos = usos/mean(usos))


usos_por_dia = rbind(arribos_ecobici,retiros_ecobici) %>% 
  group_by(fecha) %>% summarise(usos = n())



# Hacemos las regresiones lineales por estación y obtenemos la pendiente de cada modelo
modelos = usos_por_estacion_por_dia %>% group_by(estacion) %>% do(model = lm(usos ~ fecha, data = .))
modelos = modelos %>% mutate(pendiente = model$coefficients[2])

# Calculamos el promedio de usos por dia
tendencia_estacion = usos_por_estacion_por_dia %>% group_by(estacion) %>% 
  summarise(promedio_de_usos = mean(usos))

# Juntamos la información de promedios con la de los modelos
tendencia_estacion = left_join(tendencia_estacion,modelos)
tendencia_estacion = tendencia_estacion %>% mutate(pendiente_relativa = pendiente/promedio_de_usos)


# Encontramos 3 estaciones con mayor tendencia a la alta y menor a la baja
tendencia_alta = tendencia_estacion %>% slice_max(pendiente_relativa, n = 3) %>% 
  select(estacion,pendiente_relativa)

tendencia_baja = tendencia_estacion %>% slice_min(pendiente_relativa, n = 3) %>% 
  select(estacion,pendiente_relativa)

resumen_tendencia = rbind(tendencia_alta,tendencia_baja) %>%
  rename(`Indice de Tendencia` = pendiente_relativa) %>% 
  mutate(`Clasificación` = c('Alta 1', 'Alta 2','Alta 3','Baja 1','Baja 2', 'Baja 3'))

estaciones_tendencia = usos_por_estacion_por_dia %>% 
  filter(estacion %in% c(tendencia_alta$estacion,tendencia_baja$estacion)) %>% 
  mutate(estacion = case_when(estacion == 442 ~ 'Alta 1',
                              estacion == 208 ~ 'Alta 2',
                              estacion == 192 ~ 'Alta 3',
                              estacion == 193 ~ 'Baja 1',
                              estacion == 260 ~ 'Baja 2',
                              T ~ 'Baja 3'))



plot_tendencia = ggplot(estaciones_tendencia,aes(x = fecha,y = usos_relativos,colour = estacion))+
  geom_smooth(se = F)+xlab('Fecha')+ylab('Usos relativos')+labs(title = 'Comparación de tendencia')

tendencia_total = ggplot(usos_por_dia,aes(x = fecha,y = usos))+
  geom_smooth(se = F)+xlab('Fecha')+ylab('Usos relativos')+labs(title = 'Tendencia Total')
