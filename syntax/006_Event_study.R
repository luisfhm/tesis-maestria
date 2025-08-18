df <-readRDS(file = paste0(datadir,"Incidentes-Infracciones.RDS"))

event<-"2022-04-01" 

# df <- df %>% group_by(colonia) %>% 
#   mutate(event_time=tiempo[Post>0][1])
df<-df %>% group_by(codigo_postal) %>% 
  mutate(event_time=ifelse(T>0,event,NA))
c<-1
df<-df %>% mutate(linf=log(inf_total+c),lacc=log(accid_total+c),
                  PostT=Post*T)

df$event_time[df$T==0] <-NA


df<- df %>% group_by(codigo_postal) %>% 
  mutate(time_to_event=ifelse(T==1,
                              time_length(difftime(tiempo, event_time),
                                          "months") %>% round(),0),
         mes=month(tiempo))



# for(i in 1:12){
#   df[paste0("dummy",i)]<-ifelse(df$mes==i&T==1,1,0)
#   df[paste0("dummy",i+12)]<-ifelse(df$mes==i&T==0,1,0)
#   
# }
# 

df<-df %>% filter(time_to_event>=-11 &time_to_event<13)
saveRDS(df,paste0(datadir,"Base_final.RDS"))


#Infracciones
plot<- feols(log(inf_moto+1)~i(time_to_event,T,ref=-1)|
               codigo_postal+tiempo+alcaldia,
             cluster = "codigo_postal",data = df)

iplot(plot,xlab = "Meses desde el tratamiento",
      ylab = "Valor estimado (95% IC)",
      main = "Event Study Sobre Infracciones Totales")



#ggplot
coefs <- c(plot$coefficients[1:which(names(plot$coefficients)=="time_to_event::0:T")-1],0,
           plot$coefficients[which(names(plot$coefficients)=="time_to_event::0:T"):length(plot$coefficients)])
se <- c(plot$se[1:which(names(plot$coefficients)=="time_to_event::0:T")-1],
        0,plot$se[which(names(plot$coefficients)=="time_to_event::0:T"):length(plot$se)])

datos_did <- data.frame(coeficientes=coefs,ses=se,
                        time=c(-length(which(unique(sort(df$time_to_event))<0)):length(which(unique(sort(df$time_to_event))>0))),
                        type=rep(1:3,c(length(which(unique(sort(df$time_to_event))<0))-1,1,length(which(unique(sort(df$time_to_event)+1)>0)))))
datos_did$time <- factor(datos_did$time)
colors=c("black","blue","red")

ggplot(data=datos_did,mapping=aes(y=coeficientes, x=time))+
  geom_point(aes(colour=factor(type)), size=2)+
  geom_errorbar(aes(ymin=(coeficientes-1.96*ses),
                    ymax=(coeficientes+1.96*ses),colour=factor(type)),
                width=0.2)+
  geom_hline(yintercept=0,linetype="solid",color="grey",2)+
  geom_vline(xintercept=11,
  linetype="dashed", color="red",2)+theme_bw()+
  ylab("Valor estimado (95% IC)")+xlab("Meses desde el tratamiento")+
  scale_color_manual(name="Periodo",values = colors)+
  theme(legend.position = "none")+
  ggtitle("Event Study sobre Infracciones de Motos")+
  theme(axis.text.x  = element_text(size = 5))

#Accidentes
plot<- feols(log(accid_moto+1)~i(time_to_event,T,ref=-1)|
               codigo_postal+tiempo,
             cluster = "codigo_postal",data = df)

iplot(plot,xlab = "Meses desde el tratamiento",
      ylab = "Valor estimado (95% IC)",
      main = "Event Study Sobre Accidentes Totales")

  


#ggplot
coefs <- c(plot$coefficients[1:which(names(plot$coefficients)=="time_to_event::0:T")-1],0,
           plot$coefficients[which(names(plot$coefficients)=="time_to_event::0:T"):length(plot$coefficients)])
se <- c(plot$se[1:which(names(plot$coefficients)=="time_to_event::0:T")-1],
        0,plot$se[which(names(plot$coefficients)=="time_to_event::0:T"):length(plot$se)])

datos_did <- data.frame(coeficientes=coefs,ses=se,
                        time=c(-length(which(unique(sort(df$time_to_event))<0)):length(which(unique(sort(df$time_to_event))>0))),
                        type=rep(1:3,c(length(which(unique(sort(df$time_to_event))<0))-1,1,length(which(unique(sort(df$time_to_event)+1)>0)))))
datos_did$time <- factor(datos_did$time)
colors=c("black","blue","red")

ggplot(data=datos_did,mapping=aes(y=coeficientes, x=time))+
  geom_point(aes(colour=factor(type)), size=2)+
  geom_errorbar(aes(ymin=(coeficientes-1.96*ses),
                    ymax=(coeficientes+1.96*ses),colour=factor(type)),
                width=0.2)+
  geom_hline(yintercept=0,linetype="solid",color="grey",2)+
  geom_vline(xintercept=11,
             linetype="dashed", color="red",2)+theme_bw()+
  ylab("Valor estimado (95% IC)")+xlab("Meses desde el tratamiento")+
  scale_color_manual(name="Periodo",values = colors)+
  theme(legend.position = "none")+
  ggtitle("Event Study sobre Accidentes de Motos")+
  theme(axis.text.x  = element_text(size = 5))

