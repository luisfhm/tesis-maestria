#Lectura y union de las tablas de infracciones
inf <- "data/infracciones_infracciones_transito"
year <- 2020:2023
bim <- 1:6

infrac <- data.frame()
for(i in year){
  for(j in bim){
    if(i ==2023 & j>2) break
    file <- paste0(inf,"_",i,"_b",j,".csv")
    df <- read.csv(file)
    if(i ==2020 | (i==2021 & j<3)){
      df <- df %>% mutate(categoria=motivacion)
    }
    if(j>1| i>2020) {
      columns <- intersect(names(df),names(infrac))
      df <- df %>% select(columns)
      infrac <- infrac %>% select(columns)
    }
    infrac <- rbind(infrac,df)
  }
}

rm(df)
gc()

#Convertir a formato de fecha
infrac$fecha_infraccion<-infrac$fecha_infraccion %>% as.Date() 

saveRDS(infrac,paste0(datadir,"Infracciones.RDS"))

rm(infrac)

#Lectura y union de las tablas de incidentes
anios<-c("2014_2015","2016_2018","2019_2021","2022_2023")
file<-"inViales_"

incidentes<-c()
for (i in anios) {
  df<-read.csv(file = paste0(datadir,file,i,".csv"))
  incidentes<-rbind(incidentes,df)
}
#Cambio de formato
incidentes$fecha_cierre<-as.Date(incidentes$fecha_cierre)
incidentes$fecha_creacion<-as.Date(incidentes$fecha_creacion)
#Eliminamos duplicados
incidentes<-distinct(incidentes)
saveRDS(incidentes,paste0(datadir,"Incidentes.RDS"))
rm(incidentes,df)
gc()
