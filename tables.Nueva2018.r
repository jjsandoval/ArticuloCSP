#install.packages("tables")
require(tables)
setwd("~/Documentos/ModelosR_pc/ModeloFinal")
tab.pred.nb.inf.cm2<-read.table("tab.pred.bn.inf.cm3.csv", header=T, sep=",", stringsAsFactors=F)
#
tab.pred.nb.inf.cm2$Edad[tab.pred.nb.inf.cm2$Edad == 1] <- "65-69"
tab.pred.nb.inf.cm2$Edad[tab.pred.nb.inf.cm2$Edad == 2] <- "70-74"
tab.pred.nb.inf.cm2$Edad[tab.pred.nb.inf.cm2$Edad == 3] <- "75-79"
tab.pred.nb.inf.cm2$Edad[tab.pred.nb.inf.cm2$Edad == 4] <- "80-84"
tab.pred.nb.inf.cm2$Edad[tab.pred.nb.inf.cm2$Edad == 5] <- "85+"
#
tab.pred.nb.inf.cm2$Esc[tab.pred.nb.inf.cm2$Esc == 1] <- "0-3"
tab.pred.nb.inf.cm2$Esc[tab.pred.nb.inf.cm2$Esc == 2] <- "4-7"
tab.pred.nb.inf.cm2$Esc[tab.pred.nb.inf.cm2$Esc == 3] <- "8-11"
tab.pred.nb.inf.cm2$Esc[tab.pred.nb.inf.cm2$Esc == 4] <- "12+"
#
tab.pred.nb.inf.cm2$Sex[tab.pred.nb.inf.cm2$Sex==1]<-"Mujer"
tab.pred.nb.inf.cm2$Sex[tab.pred.nb.inf.cm2$Sex==2]<-"Hombre" 
#
# 2009
ano1<-subset(tab.pred.nb.inf.cm2, Año==1, select = c("Esc", "Edad","Sex","Median", 
                                                          "Lower95", "Upper95"))
t1<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano1);t1
latex(t1)
#
# Alzheimer
ano1.a<-ano1[4:6]*.72
ano1.a2<-cbind("Esc"=ano1$Esc, "Edad"=ano1$Edad, "Sex"=ano1$Sex,ano1.a)
t.ano1a<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano1.a2); t.ano1a
latex(t.ano1a)

## 2010
ano2<-subset(tab.pred.nb.inf.cm2, Año==2, select = c("Esc", "Edad","Sex","Median", 
                                                     "Lower95", "Upper95"))
t2<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano2);t2
latex(t2)
#
# Alzheimer
ano2.a<-ano2[4:6]*.72
ano2.a2<-cbind("Esc"=ano2$Esc, "Edad"=ano2$Edad, "Sex"=ano2$Sex,ano2.a)
t.ano2a<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano2.a2); t.ano2a
latex(t.ano2a)

## 2011
ano3<-subset(tab.pred.nb.inf.cm2, Año==3, select = c("Esc", "Edad","Sex","Median", 
                                                     "Lower95", "Upper95"))
t3<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano3);t3
latex(t3)
#
# Alzheimer
ano3.a<-ano3[4:6]*.72
ano3.a2<-cbind("Esc"=ano3$Esc, "Edad"=ano3$Edad, "Sex"=ano3$Sex,ano3.a)
##### Educacion vs Edad
t.ano3a<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano3.a2); t.ano3a
latex(t.ano3a)

################################# Edad & Educacion ############################################
## 2011
t22<-tabular(Factor(Edad)*(Factor(Esc, "Educación")+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano3);t22
latex(t22)

ano3.a<-ano3[4:6]*.72
ano3.a2<-cbind("Esc"=ano3$Esc, "Edad"=ano3$Edad, "Sex"=ano3$Sex, ano3.a)
t.ano3a<-tabular(Factor(Edad)*(Factor(Esc, "Educación")+1)+1
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano3.a2); t.ano3a
latex(t.ano3a)

## 2012
ano4<-subset(tab.pred.nb.inf.cm2, Año==4, select = c("Esc", "Edad","Sex","Median", 
                                                     "Lower95", "Upper95"))
t4<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano4);t4
latex(t4)
#
# Alzheimer
ano4.a<-ano4[4:6]*.72
ano4.a2<-cbind("Esc"=ano4$Esc, "Edad"=ano4$Edad, "Sex"=ano4$Sex,ano4.a)
t.ano4a<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano4.a2); t.ano4a
latex(t.ano4a)

## 2013
ano5<-subset(tab.pred.nb.inf.cm2, Año==5, select = c("Esc", "Edad","Sex","Median", 
                                                     "Lower95", "Upper95"))
t5<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)
            ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano5);t5
latex(t5)
#
# Alzheimer
ano5.a<-ano5[4:6]*.72
ano5.a2<-cbind("Esc"=ano5$Esc, "Edad"=ano5$Edad, "Sex"=ano5$Sex, ano5.a)
t.ano5a<-tabular(Factor(Esc, "Educación")*(Factor(Edad)+1)+1
                 ~Factor(Sex, "Sexo")*(Median+Lower95+Upper95)*(median),ano5.a2); t.ano5a
latex(t.ano5a)

