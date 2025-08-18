#Lectura bases
incidentes <-readRDS(file = paste0(datadir,"Incidentes_agrupada.RDS"))
infracciones <-readRDS(file = paste0(datadir,"Infracciones_agrupada.RDS"))

incidentes <- incidentes %>% rename(accid_moto=Moto,accid_no_moto=`No moto`,accid_total=total,codigo_postal=cps)
infracciones <- infracciones %>% rename(inf_moto=Moto,inf_no_moto=`No moto`,inf_total=total)

cols <- union(incidentes$codigo_postal,infracciones$codigo_postal)
alcs <- unique(incidentes$alcaldia)
time <- union(infracciones$tiempo,incidentes$tiempo)
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
  
vars <- intersect(names(incidentes),names(infracciones))

df <- all_combinations %>% merge(infracciones, by =vars , all.x = TRUE,all.y = TRUE,all = T)

df <- df %>% merge(incidentes,by = c("tiempo","alcaldia", "codigo_postal"), all = TRUE)
df$tiempo <- as.Date(paste0(df$tiempo,"-01"), format = "%Y-%m-%d")

df <- df %>% mutate(Post=ifelse(tiempo<"2022-04-01",0,1))
df[is.na(df)]<-0


antes <- df %>%filter(tiempo<="2021-04-01") %>% group_by(codigo_postal) %>%
  summarise(inf=sum(inf_total))
med_before <- median(antes$inf)
antes <- antes %>% mutate(T=ifelse(inf>med_before,1,0))

df<- df %>% left_join(antes %>% select(codigo_postal,T),by="codigo_postal")
df$T[is.na(df$T)] <- 0


saveRDS(df,file = paste0(datadir,"Incidentes-Infracciones.RDS"))

cps<-readRDS(paste0(datadir,"codigos_postales.RDS"))

trat<-unique(df %>% select(codigo_postal,T))

cps<-cps %>% left_join(trat,by=c("id" ="codigo_postal"))

trat1<-cps %>% filter(T==1)

ggplot() +
  geom_sf(data = cps, fill = 'gray') +
  geom_sf(data = trat1, fill = 'red') +ggtitle("Códigos postales marcados con el instrumento")
# 
# #Por semana
# #Lectura bases
# incidentes <-readRDS(file = paste0(datadir,"Incidentes_agrupada_semana.RDS"))
# infracciones <-readRDS(file = paste0(datadir,"Infracciones_agrupada_semana.RDS"))
# 
# incidentes <- incidentes %>% rename(accid_moto=Moto,accid_no_moto=`No moto`,accid_total=total,codigo_postal=cps)
# infracciones <- infracciones %>% rename(inf_moto=Moto,inf_no_moto=`No moto`,inf_total=total)
# 
# cols <- union(incidentes$codigo_postal,infracciones$codigo_postal)
# alcs <- unique(incidentes$alcaldia)
# time <- union(infracciones$tiempo,incidentes$tiempo)
# all_combinations <- expand.grid(tiempo = time,codigo_postal=cols)
# all_combinations<-all_combinations %>% 
#   mutate(alcaldia=case_when(substr(codigo_postal,1,2)=="01"~"ALVARO OBREGON",
#                             substr(codigo_postal,1,2)=="02"~"AZCAPOTZALCO",
#                             substr(codigo_postal,1,2)=="03"~"BENITO JUAREZ",
#                             substr(codigo_postal,1,2)=="04"~"COYOACAN",
#                             substr(codigo_postal,1,2)=="05"~"CUAJIMALPA DE MORELOS",
#                             substr(codigo_postal,1,2)=="06"~"CUAUHTEMOC",
#                             substr(codigo_postal,1,2)=="07"~"GUSTAVO A. MADERO",
#                             substr(codigo_postal,1,2)=="08"~"IZTACALCO",
#                             substr(codigo_postal,1,2)=="09"~"IZTAPALAPA",
#                             substr(codigo_postal,1,2)=="10"~"LA MAGDALENA CONTRERAS",
#                             substr(codigo_postal,1,2)=="11"~"MIGUEL HIDALGO",
#                             substr(codigo_postal,1,2)=="12"~"MILPA ALTA",
#                             substr(codigo_postal,1,2)=="13"~"TLAHUAC",
#                             substr(codigo_postal,1,2)=="14"~"TLALPAN",
#                             substr(codigo_postal,1,2)=="15"~"VENUSTIANO CARRANZA",
#                             T~"XOCHIMILCO"))
# 
# vars <- intersect(names(incidentes),names(infracciones))
# 
# df <- all_combinations %>% merge(infracciones, by =vars , all.x = TRUE,all.y = TRUE,all = T)
# 
# df <- df %>% merge(incidentes,by = c("tiempo","alcaldia", "codigo_postal"), all = TRUE)
# 
# df <- df %>% mutate(anyo=as.numeric(substr(tiempo,1,4)),semana=as.numeric(substr(tiempo,6,7)))
# 
# df <- df %>% 
#   mutate(tiempo=ISOweek2date(paste0(anyo,"-","W",
#                                     ifelse(nchar(semana)==1,paste0("0",semana),
#                                            semana),"-",1)))
# 
# df <- df %>% mutate(Post=ifelse(tiempo<"2022-04-01",0,1))
# df[is.na(df)]<-0
# 
# 
# antes <- df %>%filter(tiempo<"2022-04-01") %>% group_by(codigo_postal) %>% 
#   summarise(inf=sum(inf_total))
# med_before <- median(antes$inf)
# antes <- antes %>% mutate(T=ifelse(inf>med_before,1,0))
# 
# df<- df %>% left_join(antes %>% select(codigo_postal,T),by="codigo_postal")
# df$T[is.na(df$T)] <- 0
# 
# 
# saveRDS(df,file = paste0(datadir,"Incidentes-Infracciones_semana.RDS"))
