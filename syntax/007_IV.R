df <-readRDS(file = paste0(datadir,"Base_final.RDS"))


fs<-felm(log(inf_moto+1)~PostT|codigo_postal+tiempo|0|codigo_postal,data = df)
rf<-felm(log(accid_moto+1)~PostT|codigo_postal+tiempo|0|codigo_postal,data = df)

df$yhat<-fs$fitted.values

#ss<-felm(lacc~yhat|codigo_postal+tiempo|0|codigo_postal,data = df)
reg_iv<-felm(log(accid_moto+1)~1|codigo_postal+tiempo|(log(inf_moto+1)~PostT)|codigo_postal,data = df)

stargazer(fs,rf,reg_iv,type = "latex",column.labels=c("Primera etapa","Forma Reducida"
                        ,'IV'),covariate.labels = c("Instrumento","Estimador IV",
                                                                  "Estimador IV"),
                     title = "Estimación IV",dep.var.labels.include = F,header = F,
                     omit.stat = c("ser","adj.rsq"),table.placement = "H")

first<-summary(fs)
stargazer(fs,type = "text",
          add.lines = list(c("F",round(first$P.fstat["F"],3))),
          font.size = "scriptsize")
