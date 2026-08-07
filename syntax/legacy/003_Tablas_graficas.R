#Lectura bases
incidentes <-readRDS(file = paste0(datadir,"Incidentes_agrupada.RDS"))
infracciones <-readRDS(file = paste0(datadir,"Infracciones_agrupada.RDS"))

incidentes <- incidentes %>% rename(accid_moto=Moto,accid_no_moto=`No moto`)
infracciones <- infracciones %>% rename(inf_moto=Moto,inf_no_moto=`No moto`)


#Tablas
incidentes %>% group_by(alcaldia) %>% summarise(Accidentes=n()) %>% 
  kable(format = "latex",booktabs=T) %>% kable_styling(position = "center")

#Gráficas
incidentes$tiempo<- as.Date(paste0(incidentes$tiempo,"-01"), format = "%Y-%m-%d")

ggplot(incidentes, aes(x = tiempo, y = total)) +
  geom_bar(stat = "identity", aes(fill = "Total")) +
  geom_bar(stat = "identity", aes(y = accid_no_moto, fill = "Otros accidentes")) +
  geom_bar(stat = "identity", aes(y = accid_moto, fill = "Accidentes de motocicleta")) +
  scale_fill_manual(values = c("Total" = "darkgreen", "Accidentes de motocicleta" = "darkblue", "Otros accidentes" = "darkred")) +
  labs(title = "Accidentes",
       x = "Tiempo",
       y = "Número de accidentes",
       fill = "Tipo de accidentes") +
  theme_minimal()


ggplot(incidentes, aes(x = tiempo, y = accid_moto)) +
  geom_bar(stat = "identity",fill="steelblue")+theme_minimal()+
  labs(title = "Accidentes de motocicleta",
       x="Tiempo",
       y="Número de accidentes")
