setwd("~/Documentos/ModelosR_pc/ModeloFinal")
tab.pred.nb.inf.cm<-read.table("tab.pred.bn.inf.cm3.csv", header=T, sep=",", stringsAsFactors=F)
require(ggplot2)
require(reshape2)
#
# tabla Año 2013
# Demencia
ano5.sex1<-subset(tab.pred.nb.inf.cm, Año==5 & Sex==1, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano5.sex2<-subset(tab.pred.nb.inf.cm, Año==5 & Sex==2, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano5<-cbind(ano5.sex1, ano5.sex2[3:5])
ano5$Edad[ano5$Edad == 1] <- "65-69"
ano5$Edad[ano5$Edad == 2] <- "70-74"
ano5$Edad[ano5$Edad == 3] <- "75-79"
ano5$Edad[ano5$Edad == 4] <- "80-84"
ano5$Edad[ano5$Edad == 5] <- "85+"
#

colnames(ano5)<-c(c("Edad","Esc","Hombre", "LIh", "LSh","Mujer", "LIm", "LSm"))
# Alzheimer
ano5.a<-ano5[3:5]*.72
ano5.a2<-cbind("Edad"=ano5$Edad, "Esc"=ano5$Esc, ano5.a)
ano5.a2 <- ano5.a2[order(ano5.a2$Edad),] 
lano5.a2<-log(ano5.a2[3:5])
lano5.a2<-cbind("Edad"=ano5.a2$Edad, "Esc"=ano5.a2$Esc, lano5.a2) 
mlano5.a2<-melt(lano5.a2)
mlano5.a2<-subset(mlano5.a2, variable!="Esc")
x<-rep(1:4, 15)
mlano5.a2<-cbind(x, mlano5.a2)
################################# GRAFICOS HOMBRES  ###########################################
gg1<-ggplot(mlano5.a2, aes(x=x, y=value))+ 
  geom_point(size = 1, colour = "gray80", fill = "white")+
  geom_smooth(aes(linetype=Edad), se = T, colour= "gray40", method = "loess", size=.6)+
 ylab("Ln (Tasa x 100.000 hab,)")+xlab("Años de Educación \n a.")+  theme_bw()+
  #ggtitle("Tasas Específicas de Mortalidad Atribuible al Alzheimer. \n Hombres Brasil, Año 2013")+
  scale_x_continuous(labels = c("0-3","4-7", "8-11","12+"))+ylim(c(2.2,7.8))
  #theme(plot.title = element_text(hjust = 0.5))
gg1
###################################### GRAFICO MUJERES #######################################
# tabla ano 2013
# Demencia
ano5a.sex1<-subset(tab.pred.nb.inf.cm, Año==5 & Sex==1, select = c("Esc","Edad","Median", 
                                                                   "Lower95", "Upper95"))
ano5a.sex2<-subset(tab.pred.nb.inf.cm, Año==5 & Sex==2, select = c("Esc","Edad","Median", 
                                                                   "Lower95", "Upper95"))
ano5a<-cbind(ano5a.sex1, ano5a.sex2[3:5])
ano5a$Esc[ano5a$Esc == 1] <- "0-3"
ano5a$Esc[ano5a$Esc == 2] <- "4-7"
ano5a$Esc[ano5a$Esc == 3] <- "8-11"
ano5a$Esc[ano5a$Esc == 4] <- "12+"
#
colnames(ano5a)<-c(c("Esc","Edad","Hombre", "LIh", "LSh","Mujer", "LIm", "LSm"))
# Alzheimer
ano5a.a<-ano5a[3:8]*.72
ano5a.a2<-cbind("Esc"=ano5a$Esc,"Edad"=ano5a$Edad, ano5a.a)
ano5a.a2 <- ano5a.a2[order(ano5a.a2$Esc),] 
lano5a.a2<-log(ano5a.a2[3:8])
lano5a.a2<-cbind("Educación"=ano5a$Esc,"Edad"=ano5a$Edad, lano5a.a2)
mlano5a.a2<-melt(lano5a.a2)
mlano5a.a2<-subset(mlano5a.a2, variable!="Edad")
Edad<-rep(1:5, 24)
mlano5a.a2<-cbind(Edad, mlano5a.a2)
gg2<-ggplot(mlano5a.a2, aes(x=Edad, y=value))+ 
  geom_point(size = 1, colour = "gray80", fill = "white")+
  geom_smooth(aes(linetype=Educación), se = T, colour= "gray40", method = "loess", size=.6)+
  ylab("Ln (Tasa x 100.000 hab.)")+xlab("Grupos de Edad \n b.")+  theme_bw()+
  #ggtitle("Tasas Específicas de Mortalidad Atribuibles al Alzheimer, \n Mujeres Brasil Año 2013")+
  scale_x_continuous(labels = c("65-69","70-74","75-79","80-84", "85+")) +
  coord_cartesian(ylim = c(2.8,7.2), expand = T)
  
gg2

multiplot(gg1,gg2, cols = 2)
