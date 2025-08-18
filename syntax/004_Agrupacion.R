#Lectura shp (para mapas de la cdmx)
cdmx <- st_read(paste0(datadir,"colonias_iecm.shp"))

#Arreglando datos json (también para mapas pero tiene códigos postales)
cps<-from_json(paste0(datadir,"correos-postales.json"))
cps<-cps$features
cp<-data.frame(ID=cps$properties$d_cp)
for (i in 1:1215) {
  for (j in 1:length(cps$geometry$coordinates[[i]])) {
    cps$geometry$coordinates[[i]][[j]] <- as.data.frame(cps$geometry$coordinates[[i]][[j]])
  }
  cps$geometry$coordinates[[i]]<-do.call(rbind,cps$geometry$coordinates[[i]])
  cps$geometry$coordinates[[i]]<-cbind(id=rep(cp[i,],nrow(cps$geometry$coordinates[[i]])),
                                       cps$geometry$coordinates[[i]])
}
cps$geometry$coordinates <- lapply(cps$geometry$coordinates, function(df) {
  names(df) <- c("id", "X","Y")
  return(df)
})

cp <- do.call(rbind, cps$geometry$coordinates)
cp <- sfheaders::sf_multipolygon(
  obj = cp
  , multipolygon_id = "id"
  , x = "X"
  , y = "Y"
)
st_crs(cp) <- 4326
rm(cps)
saveRDS(cp,paste0(datadir,"codigos_postales.RDS"))
#Lectura de incidentes
incidentes<-readRDS(file = paste0(datadir,"Incidentes.RDS"))
incidentes<- incidentes %>% filter(year(fecha_creacion)>2019)

incidentes <- incidentes %>% filter(!is.na(colonia))
alcs <- data.frame(alcaldia=cdmx$NOMDT,col=cdmx$NOMUT)
incidentes<- incidentes %>% left_join(alcs,by=c("colonia"="col"))
incidentes <- incidentes %>% filter(!is.na(alcaldia)) #755,705x17
incidentes <- incidentes %>% mutate(moto=ifelse(incidente_c4=="Motociclista","Moto","No moto"))
#Identificando codigo postal
pnts <- incidentes %>% select(latitud,longitud)
cp2 <- st_transform(cp,crs = 2163)
pnts2 <- st_transform(st_as_sf(pnts,coords = c("longitud","latitud"),crs=4326),crs = 2163)
#Mapeamos intersección de colonia y polígono
int <- st_intersects(pnts2,cp2)
drop <- which(lengths(int)==0) #No se encontró la colonia 
int <- int[-drop]
change<-which(lengths(int)>1) #Se mapeo a más de una colonia
int<-lapply(int,min)
int <- unlist(int)
cps <- cp$id[int]
incidentes<-incidentes[-drop,]
incidentes <- cbind(incidentes,cps) #754,833x19
#Falta confirmar que los códigos postales pertenezcan a la alcaldia correcta
incidentes<- incidentes %>% mutate(alcaldia_coord=case_when(substr(cps,1,2)=="01"~"ALVARO OBREGON",
                                               substr(cps,1,2)=="02"~"AZCAPOTZALCO",
                                               substr(cps,1,2)=="03"~"BENITO JUAREZ",
                                               substr(cps,1,2)=="04"~"COYOACAN",
                                               substr(cps,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                                               substr(cps,1,2)=="06"~"CUAUHTEMOC",
                                               substr(cps,1,2)=="07"~"GUSTAVO A. MADERO",
                                               substr(cps,1,2)=="08"~"IZTACALCO",
                                               substr(cps,1,2)=="09"~"IZTAPALAPA",
                                               substr(cps,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                                               substr(cps,1,2)=="11"~"MIGUEL HIDALGO",
                                               substr(cps,1,2)=="12"~"MILPA ALTA",
                                               substr(cps,1,2)=="13"~"TLAHUAC",
                                               substr(cps,1,2)=="14"~"TLALPAN",
                                               substr(cps,1,2)=="15"~"VENUSTIANO CARRANZA",
                                               T~"XOCHIMILCO"))
incidentes <- incidentes %>% 
  mutate(ind_alcaldia=ifelse(alcaldia_coord==alcaldia,1,0)) %>% 
  filter(ind_alcaldia!=0) #672,463x21

saveRDS(incidentes,file = paste0(datadir,"Incidentes_corregida.RDS"))
incidentes <- readRDS(file = paste0(datadir,"Incidentes_corregida.RDS"))

#Para la base de infracciones guardamoms una base con codigos postales y colonias
cps<-incidentes %>% select(alcaldia,colonia,cps) %>% unique
saveRDS(cps,paste0(datadir,"Colonia-cp.RDS"))
#Ahora agrupamos por mes
incidentes_group <- incidentes %>%
  group_by(tiempo=format(fecha_creacion,"%Y-%m"),alcaldia,cps,moto) %>% 
  summarise(n=n()) %>% pivot_wider(names_from = moto,values_from = n,values_fill = 0)

incidentes_group<- incidentes_group %>%ungroup()%>% 
  mutate(total=rowSums(across(where(is.numeric)))) 
cols <- unique(incidentes_group$cps)
time <- unique(incidentes_group$tiempo)
all_combinations <- expand.grid(tiempo = time,cps=cols)
all_combinations<-all_combinations %>% 
  mutate(alcaldia=case_when(substr(cps,1,2)=="01"~"ALVARO OBREGON",
                                  substr(cps,1,2)=="02"~"AZCAPOTZALCO",
                                  substr(cps,1,2)=="03"~"BENITO JUAREZ",
                                  substr(cps,1,2)=="04"~"COYOACAN",
                                  substr(cps,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                                  substr(cps,1,2)=="06"~"CUAUHTEMOC",
                                  substr(cps,1,2)=="07"~"GUSTAVO A. MADERO",
                                  substr(cps,1,2)=="08"~"IZTACALCO",
                                  substr(cps,1,2)=="09"~"IZTAPALAPA",
                                  substr(cps,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                                  substr(cps,1,2)=="11"~"MIGUEL HIDALGO",
                                  substr(cps,1,2)=="12"~"MILPA ALTA",
                                  substr(cps,1,2)=="13"~"TLAHUAC",
                                  substr(cps,1,2)=="14"~"TLALPAN",
                                  substr(cps,1,2)=="15"~"VENUSTIANO CARRANZA",
                                  T~"XOCHIMILCO"))

# Merge with the original data frame to fill in missing values with zeros
incidentes_group <- merge(all_combinations, 
                          incidentes_group, by = c("tiempo","alcaldia" ,"cps"), all.x = TRUE)
incidentes_group[is.na(incidentes_group)] <- 0

incidentes_group$cps <- as.character(incidentes_group$cps) #50,740x6

rm(pnts,pnts2,cp,cp2,incidentes,cdmx,cps,drop,int,cols,time,all_combinations)

saveRDS(incidentes_group,file = paste0(datadir,"Incidentes_agrupada.RDS"))

#Agrupamos por semana
incidentes <- incidentes %>% mutate(week_num=strftime(fecha_creacion,format="%V"))
incidentes_group <- incidentes %>%
  group_by(tiempo=paste0(year(fecha_creacion),"_",week_num),alcaldia,cps,moto)%>% 
  summarise(n=n()) %>% pivot_wider(names_from = moto,values_from = n,values_fill = 0)

incidentes_group<- incidentes_group %>%ungroup()%>% 
  mutate(total=rowSums(across(where(is.numeric)))) 

cols <- unique(incidentes_group$cps)
time <- unique(incidentes_group$tiempo)
all_combinations <- expand.grid(tiempo = time,cps=cols)
all_combinations<-all_combinations %>% 
  mutate(alcaldia=case_when(substr(cps,1,2)=="01"~"ALVARO OBREGON",
                            substr(cps,1,2)=="02"~"AZCAPOTZALCO",
                            substr(cps,1,2)=="03"~"BENITO JUAREZ",
                            substr(cps,1,2)=="04"~"COYOACAN",
                            substr(cps,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                            substr(cps,1,2)=="06"~"CUAUHTEMOC",
                            substr(cps,1,2)=="07"~"GUSTAVO A. MADERO",
                            substr(cps,1,2)=="08"~"IZTACALCO",
                            substr(cps,1,2)=="09"~"IZTAPALAPA",
                            substr(cps,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                            substr(cps,1,2)=="11"~"MIGUEL HIDALGO",
                            substr(cps,1,2)=="12"~"MILPA ALTA",
                            substr(cps,1,2)=="13"~"TLAHUAC",
                            substr(cps,1,2)=="14"~"TLALPAN",
                            substr(cps,1,2)=="15"~"VENUSTIANO CARRANZA",
                            T~"XOCHIMILCO"))

# Merge with the original data frame to fill in missing values with zeros
incidentes_group <- merge(all_combinations, 
                          incidentes_group, by = c("tiempo","alcaldia" ,"cps"), all.x = TRUE)
incidentes_group[is.na(incidentes_group)] <- 0

incidentes_group$cps <- as.character(incidentes_group$cps) #224,200x6

rm(pnts,pnts2,cp,cp2,incidentes,cdmx,cps,drop,int,cols,time,all_combinations)

saveRDS(incidentes_group,file = paste0(datadir,"Incidentes_agrupada_semana.RDS"))


#Lectura infracciones
infracciones<-readRDS(file = paste0(datadir,"Infracciones.RDS")) #12,091768x14
infracciones<- infracciones %>% filter(!(is.na(latitud)&is.na(longitud)&is.na(colonia))) #11,921,013x14
#Antes de agrupar necesitamos arreglar el problema de las categorías y colonias 
infracciones_cats_ant <- infracciones %>% filter(fecha_infraccion<"2021-05-01") #4,304,797x14
infracciones_cats_desp <- infracciones %>% filter(fecha_infraccion>="2021-05-01") #7,616,216x14
rm(infracciones)
gc()
#Corregimos colonias quitando signos de puntuación espacios en blanco y pasando a mayúsculas
infracciones_cats_desp$colonia <- iconv(infracciones_cats_desp$colonia,from="UTF-8",to="ASCII//TRANSLIT") %>%
  toupper()
infracciones_cats_desp$colonia <- trimws(infracciones_cats_desp$colonia)
infracciones_cats_desp$colonia <- gsub("[[:punct:]]", "", infracciones_cats_desp$colonia)
infracciones_cats_desp <- infracciones_cats_desp %>% filter(colonia!="DESCONOCIDO")
infracciones_cats_desp$colonia <- gsub("\\s+", " ", infracciones_cats_desp$colonia)
infracciones_cats_desp$alcaldia <- iconv(infracciones_cats_desp$alcaldia,from="UTF-8",to="ASCII//TRANSLIT") %>%
  toupper()
# #Aún hay muchas inconsistencias, entonces obtenemos los nombres correctos y los comparamos con los incorrectos
# #con el paquete stringdist, con este método obtendrá el nombre correcto "más cercano"
# correct <- unique(infracciones_cats_ant$colonia)
# cols_common<- intersect(infracciones_cats_ant$colonia %>% unique,infracciones_cats_desp$colonia %>% unique)
# not_correct <- infracciones_cats_desp %>% filter(!(colonia%in%cols_common))
# 
# tic()
# corrected <- sapply(not_correct$colonia, function(name) {
#   closest_match <- stringdist(name, correct, method = "lv")  # Use Levenshtein distance for matching
#   if (min(closest_match,na.rm = T) <= 3) {  # If distance is less than or equal to 3, consider it a match
#     return(correct[which.min(closest_match)])
#   } else {
#     return(name)  # Return original name if no close match is found
#   }
# })
# toc() #90 min
# 
# not_correct$colonia <- corrected
# correct <- infracciones_cats_desp %>% filter((colonia%in%cols_common))
# infracciones_cats_desp <- rbind(correct,not_correct)
# cols_common<- intersect(infracciones_cats_ant$colonia %>% unique,infracciones_cats_desp$colonia %>% unique)
# 
# infracciones_cats_desp <- infracciones_cats_desp %>% filter((colonia%in%cols_common))



#Ahora corregimos el problema de las categorias
cats_ant<- infracciones_cats_ant$categoria %>% unique

#Categorías específicas de motos
cat_moto <- cats_ant[grepl("SE PROHÍBE A LOS CONDUCTORES DE MOTOCICLETAS",cats_ant, fixed = TRUE)]
cat_moto2 <-cats_ant[grepl("LOS CONDUCTORES DE MOTOCICLETAS DEBEN",cats_ant, fixed = TRUE)] 
cat_moto <- c(cat_moto,cat_moto2)
cat_moto_desp <- "Violaciones de motocicletas"

#Crear columna que indica si fue a moto o no
infracciones_cats_ant <- infracciones_cats_ant %>% 
  mutate(moto=ifelse(categoria%in%cat_moto,"Moto","No moto"))
infracciones_cats_desp <- infracciones_cats_desp %>%
  mutate(moto=ifelse(categoria==cat_moto_desp,"Moto","No moto"))

infracciones <- rbind(infracciones_cats_ant,infracciones_cats_desp) #11,486,959x15
rm(infracciones_cats_ant,infracciones_cats_desp)
gc()

#Cargamos la base anterior de colonias y cps
cps<-readRDS(paste0(datadir,"Colonia-cp.RDS"))
cps<-cps[!duplicated(cps$colonia),]
#Nueva columna de codigos postales pero uniendo por el nombre de colonia
infracciones<-infracciones %>% left_join(cps ,by=c("alcaldia","colonia"))
#Ahora mapeamos códigos postales
infracciones <- infracciones %>% 
  filter(nchar(codigo_postal)==4 | 
           nchar(codigo_postal)==5 |!is.na(latitud)|!is.na(longitud)|!is.na(cps)) #8,447,917x16
infracs_coord <- infracciones %>% filter(nchar(codigo_postal)!=4 & 
                                           nchar(codigo_postal)!=5&is.na(cps)) #210,495x16
infracs_cp <- infracciones %>% filter(nchar(codigo_postal)==4 | 
                                        nchar(codigo_postal)==5|!is.na(cps))
infracs_cp <- infracs_cp %>% mutate(codigo_postal=ifelse(nchar(codigo_postal)==4,
                                                         paste0("0",codigo_postal),
                                                         codigo_postal))
infracs_cp<-infracs_cp %>% 
  mutate(codigo_postal=ifelse(is.na(cps),ifelse(nchar(codigo_postal)!=5,
                                                NA,codigo_postal),
                              cps)) #8,221,245x16

infracs_cp<-infracs_cp %>% filter(codigo_postal%in%cp$id)#8,082,425x16

#Mapeamos intersección de colonia y polígono
pnts <- infracs_coord %>% select(longitud,latitud)
cp2 <- st_transform(cp,crs = 2163)
pnts2 <- st_transform(st_as_sf(pnts,coords = c("longitud","latitud"),crs=4326),crs = 2163)

int <- st_intersects(pnts2,cp2)
drop <- which(lengths(int)==0) #No se encontró la colonia 
infracs_coord <- infracs_coord[-drop,]
int <- int[-drop]
int <- unlist(int)
cps <- cp$id[int]
infracs_coord$codigo_postal <- cps
#Falta confirmar que los códigos postales pertenezcan a la alcaldia correcta
infracs_coord<- infracs_coord %>% 
  mutate(alcaldia_coord=case_when(substr(codigo_postal,1,2)=="01"~"ALVARO OBREGON",
                                  substr(codigo_postal,1,2)=="02"~"AZCAPOTZALCO",
                                  substr(codigo_postal,1,2)=="03"~"BENITO JUAREZ",
                                  substr(codigo_postal,1,2)=="04"~"COYOACAN",
                                  substr(codigo_postal,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                                  substr(codigo_postal,1,2)=="06"~"CUAUHTEMOC",
                                  substr(codigo_postal,1,2)=="07"~"GUSTAVO A. MADERO",
                                  substr(codigo_postal,1,2)=="08"~"IZTACALCO",
                                  substr(codigo_postal,1,2)=="09"~"IZTAPALAPA",
                                  substr(codigo_postal,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                                  substr(codigo_postal,1,2)=="11"~"MIGUEL HIDALGO",
                                  substr(codigo_postal,1,2)=="12"~"MILPA ALTA",
                                  substr(codigo_postal,1,2)=="13"~"TLAHUAC",
                                  substr(codigo_postal,1,2)=="14"~"TLALPAN",
                                  substr(codigo_postal,1,2)=="15"~"VENUSTIANO CARRANZA",
                                  T~"XOCHIMILCO"))
infracs_coord <- infracs_coord %>% 
  mutate(ind_alcaldia=ifelse(alcaldia_coord==alcaldia,1,0)) %>% 
  filter(ind_alcaldia!=0) #210,406x18


infracs_coord <- infracs_coord %>% select(-alcaldia_coord,-ind_alcaldia)

#Confirmamos lo mismo para las que si tenían codigo postal
infracs_cp <- infracs_cp %>%
  mutate(alcaldia_coord=case_when(substr(codigo_postal,1,2)=="01"~"ALVARO OBREGON",
                                  substr(codigo_postal,1,2)=="02"~"AZCAPOTZALCO",
                                  substr(codigo_postal,1,2)=="03"~"BENITO JUAREZ",
                                  substr(codigo_postal,1,2)=="04"~"COYOACAN",
                                  substr(codigo_postal,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                                  substr(codigo_postal,1,2)=="06"~"CUAUHTEMOC",
                                  substr(codigo_postal,1,2)=="07"~"GUSTAVO A. MADERO",
                                  substr(codigo_postal,1,2)=="08"~"IZTACALCO",
                                  substr(codigo_postal,1,2)=="09"~"IZTAPALAPA",
                                  substr(codigo_postal,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                                  substr(codigo_postal,1,2)=="11"~"MIGUEL HIDALGO",
                                  substr(codigo_postal,1,2)=="12"~"MILPA ALTA",
                                  substr(codigo_postal,1,2)=="13"~"TLAHUAC",
                                  substr(codigo_postal,1,2)=="14"~"TLALPAN",
                                  substr(codigo_postal,1,2)=="15"~"VENUSTIANO CARRANZA",
                                  T~"XOCHIMILCO"))



infracs_cp <- infracs_cp %>% 
  mutate(ind_alcaldia=ifelse(alcaldia_coord==alcaldia,1,0)) %>% 
  filter(ind_alcaldia!=0) #7,194,880x18

infracs_cp <- infracs_cp %>% select(-alcaldia_coord,-ind_alcaldia)

infracciones <- rbind(infracs_coord,infracs_cp)#7,405,286x16

saveRDS(infracciones,file = paste0(datadir,"Infracciones_corregida.RDS"))
infracciones <- readRDS(file = paste0(datadir,"Infracciones_corregida.RDS")) 


#Por último agrupamos por mes
infracciones_group <- infracciones %>%
  group_by(tiempo=format(fecha_infraccion,"%Y-%m"),codigo_postal,alcaldia,moto) %>% 
  summarise(n=n()) %>% pivot_wider(names_from = moto,values_from = n,values_fill = 0)

infracciones_group<- infracciones_group %>%ungroup()%>% 
  mutate(total=rowSums(across(where(is.numeric)))) 

cols <- unique(infracciones_group$codigo_postal)
time <- unique(infracciones_group$tiempo)
all_combinations <- expand.grid(tiempo = time,codigo_postal=cols)
all_combinations<-all_combinations %>% 
  mutate(alcaldia=case_when(substr(codigo_postal,1,2)=="01"~"ALVARO OBREGON",
                                  substr(codigo_postal,1,2)=="02"~"AZCAPOTZALCO",
                                  substr(codigo_postal,1,2)=="03"~"BENITO JUAREZ",
                                  substr(codigo_postal,1,2)=="04"~"COYOACAN",
                                  substr(codigo_postal,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                                  substr(codigo_postal,1,2)=="06"~"CUAUHTEMOC",
                                  substr(codigo_postal,1,2)=="07"~"GUSTAVO A. MADERO",
                                  substr(codigo_postal,1,2)=="08"~"IZTACALCO",
                                  substr(codigo_postal,1,2)=="09"~"IZTAPALAPA",
                                  substr(codigo_postal,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                                  substr(codigo_postal,1,2)=="11"~"MIGUEL HIDALGO",
                                  substr(codigo_postal,1,2)=="12"~"MILPA ALTA",
                                  substr(codigo_postal,1,2)=="13"~"TLAHUAC",
                                  substr(codigo_postal,1,2)=="14"~"TLALPAN",
                                  substr(codigo_postal,1,2)=="15"~"VENUSTIANO CARRANZA",
                                  T~"XOCHIMILCO"))

# Merge with the original data frame to fill in missing values with zeros
infracciones_group <- merge(all_combinations, infracciones_group, by = c("tiempo","alcaldia", "codigo_postal"), all.x = TRUE)
infracciones_group[is.na(infracciones_group)] <- 0

infracciones_group$codigo_postal <- as.character(infracciones_group$codigo_postal)

saveRDS(infracciones_group,file = paste0(datadir,"Infracciones_agrupada.RDS"))


#Por último agrupamos por semana
infracciones <- infracciones %>% mutate(week_num=strftime(fecha_infraccion, format = "%V"))
infracciones_group <- infracciones %>%
  group_by(tiempo=paste0(year(fecha_infraccion),"_",week_num),codigo_postal,alcaldia,moto) %>% 
  summarise(n=n()) %>% pivot_wider(names_from = moto,values_from = n,values_fill = 0)

infracciones_group<- infracciones_group %>%ungroup()%>% 
  mutate(total=rowSums(across(where(is.numeric)))) 

cols <- unique(infracciones_group$codigo_postal)
time <- unique(infracciones_group$tiempo)
all_combinations <- expand.grid(tiempo = time,codigo_postal=cols)
all_combinations<-all_combinations %>% 
  mutate(alcaldia=case_when(substr(codigo_postal,1,2)=="01"~"ALVARO OBREGON",
                            substr(codigo_postal,1,2)=="02"~"AZCAPOTZALCO",
                            substr(codigo_postal,1,2)=="03"~"BENITO JUAREZ",
                            substr(codigo_postal,1,2)=="04"~"COYOACAN",
                            substr(codigo_postal,1,2)=="05"~"CUAJIMALPA DE MORELOS",
                            substr(codigo_postal,1,2)=="06"~"CUAUHTEMOC",
                            substr(codigo_postal,1,2)=="07"~"GUSTAVO A. MADERO",
                            substr(codigo_postal,1,2)=="08"~"IZTACALCO",
                            substr(codigo_postal,1,2)=="09"~"IZTAPALAPA",
                            substr(codigo_postal,1,2)=="10"~"LA MAGDALENA CONTRERAS",
                            substr(codigo_postal,1,2)=="11"~"MIGUEL HIDALGO",
                            substr(codigo_postal,1,2)=="12"~"MILPA ALTA",
                            substr(codigo_postal,1,2)=="13"~"TLAHUAC",
                            substr(codigo_postal,1,2)=="14"~"TLALPAN",
                            substr(codigo_postal,1,2)=="15"~"VENUSTIANO CARRANZA",
                            T~"XOCHIMILCO"))

# Merge with the original data frame to fill in missing values with zeros
infracciones_group <- merge(all_combinations, infracciones_group,
                            by = c("tiempo","alcaldia", "codigo_postal"), all.x = TRUE)
infracciones_group[is.na(infracciones_group)] <- 0

infracciones_group$codigo_postal <- as.character(infracciones_group$codigo_postal)#170,405x6

saveRDS(infracciones_group,file = paste0(datadir,"Infracciones_agrupada_semana.RDS"))
