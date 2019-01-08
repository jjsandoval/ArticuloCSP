setwd("~/Documentos/PhD_Demografia/ModelosR/ModeloFinal")
tab.pred.nb.inf.cm<-read.table("tab.pred.bn.inf.cm3.csv", header=T, sep=",", stringsAsFactors=F)
#
# tabla Año 2011
# Demencia
ano3.sex1<-subset(tab.pred.nb.inf.cm, Año==3 & Sex==1, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano3.sex2<-subset(tab.pred.nb.inf.cm, Año==3 & Sex==2, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano3<-cbind(ano3.sex1, ano3.sex2[3:5])
ano3$Edad[ano3$Edad == 1] <- "65-69"
ano3$Edad[ano3$Edad == 2] <- "70-74"
ano3$Edad[ano3$Edad == 3] <- "75-79"
ano3$Edad[ano3$Edad == 4] <- "80-84"
ano3$Edad[ano3$Edad == 5] <- "85+"
#
ano3$Esc[ano3$Esc == 1] <- "0"
ano3$Esc[ano3$Esc == 2] <- "1-3"
ano3$Esc[ano3$Esc == 3] <- "4-7"
ano3$Esc[ano3$Esc == 4] <- "8-11"
ano3$Esc[ano3$Esc == 5] <- "12+"
#
colnames(ano3)<-c(c("Edad","Esc","Hombre", "LIh", "LSh","Mujer", "LIm", "LSm"))
# Alzheimer
ano3.a<-ano3[3:8]*.72
ano3.a2<-cbind("Edad"=ano3$Edad, "Esc"=ano3$Esc, ano3.a)
ano3.a2 <- ano3.a2[order(ano3.a2$Edad),] 
lano3.a2<-log(ano3.a2[3:8])
################################# GRAFICOS ##############################################
setwd("~/Documentos/PhD_Demografia/TesisLatex/figure")
# grafico Alzheimer Año 2011
pdf("alz10e2.pdf", paper= "a4r", width=14, height=8.5) # guardar plot PDF
grank<-range(lano3.a2$Hombre[1:5], lano3.a2$LIh[1:5], lano3.a2$LSh[1:5],
             lano3.a2$Hombre[6:10], lano3.a2$LIh[6:10], lano3.a2$LSh[6:10],
             lano3.a2$Hombre[11:15], lano3.a2$LIh[11:15], lano3.a2$LSh[11:15],
             lano3.a2$Hombre[16:20], lano3.a2$LIh[16:20], lano3.a2$LSh[16:20],
             lano3.a2$Hombre[21:25], lano3.a2$LIh[21:25], lano3.a2$LSh[21:25])

plot(lano3.a2$Hombre[1:5],type='o',axes=FALSE, ann=FALSE,ylim=grank,lwd=2)
axis(1,at=1:5,lab=ano3.a2$Esc[1:5])
axis(2, las=1,at=NULL)
box()
lines(lano3.a2$LSh[1:5], type="l", lty=3,lwd=2, col="gray50")
lines(lano3.a2$LIh[1:5], type="l", lty=3, lwd=2, col="gray50")
lines(lano3.a2$Hombre[6:10], type="o", pch=22, lty=2, lwd=2)
lines(lano3.a2$LSh[6:10], type="l", lty=3,lwd=2, col="gray50")
lines(lano3.a2$LIh[6:10], type="l", lty=3, lwd=2, col="gray50")
lines(lano3.a2$Hombre[11:15], type="o", pch=22, lty=4, lwd=2)
lines(lano3.a2$LSh[11:15], type="l", lty=3,lwd=2, col="gray50")
lines(lano3.a2$LIh[11:15], type="l", lty=3, lwd=2, col="gray50")
lines(lano3.a2$Hombre[16:20], type="o", pch=22, lty=5, lwd=2)
lines(lano3.a2$LSh[16:20], type="l", lty=3,lwd=2, col="gray50")
lines(lano3.a2$LIh[16:20], type="l", lty=3, lwd=2, col="gray50")
lines(lano3.a2$Hombre[21:25], type="o", pch=22, lty=6, lwd=2)
lines(lano3.a2$LSh[21:25], type="l", lty=3,lwd=2, col="gray50")
lines(lano3.a2$LIh[21:25], type="l", lty=3, lwd=2, col="gray50")
title(main="TEM atribuible al Alzheimer en hombres \n Brasil, 2011", col.main="black", font.main=2)
title(xlab="Nivel de estudios (Años)", col.lab=rgb(0,0.1,0),font.main=1)
title(ylab="Logaritmo (x 100.000)", col.lab=rgb(0,0.1,0),font.main=1)
legend("topright", title="Grupos de Edad", c(paste(c("65-69", "70-74", "75-79","80-84","85+","ICR95%"))), 
       cex=0.7, col=c(1,1,1,1,1, "gray50"), lty=c(1,2,4:6,3),lwd=2, bty="n", 
       y.intersp=0.8, x.intersp=2.0, merge = TRUE,  ncol=2)
dev.off()
###################################################################################3
# tabla ano 2011
# Demencia
ano3a.sex1<-subset(tab.pred.nb.inf.cm, Año==3 & Sex==1, select = c("Esc","Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano3a.sex2<-subset(tab.pred.nb.inf.cm, Año==3 & Sex==2, select = c("Esc","Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano3a<-cbind(ano3a.sex1, ano3a.sex2[3:5])
ano3a$Edad[ano3a$Edad == 1] <- "65-69"
ano3a$Edad[ano3a$Edad == 2] <- "70-74"
ano3a$Edad[ano3a$Edad == 3] <- "75-79"
ano3a$Edad[ano3a$Edad == 4] <- "80-84"
ano3a$Edad[ano3a$Edad == 5] <- "85+"
#
ano3a$Esc[ano3a$Esc == 1] <- "0"
ano3a$Esc[ano3a$Esc == 2] <- "1-3"
ano3a$Esc[ano3a$Esc == 3] <- "4-7"
ano3a$Esc[ano3a$Esc == 4] <- "8-11"
ano3a$Esc[ano3a$Esc == 5] <- "12+"
#
colnames(ano3a)<-c(c("Esc","Edad","Hombre", "LIh", "LSh","Mujer", "LIm", "LSm"))
# Alzheimer
ano3a.a<-ano3a[3:8]*.72
ano3a.a2<-cbind("Esc"=ano3a$Esc,"Edad"=ano3a$Edad, ano3a.a)
ano3a.a2 <- ano3a.a2[order(ano3a.a2$Esc),] 
lano3a.a2<-log(ano3a.a2[3:8])
################################# GRAFICOS ##############################################
# grafico Alzheimer Año 2011
pdf("alz10d2.pdf", paper= "a4r", width=14, height=8.5) # guardar plot PDF
grank<-range(lano3a.a2$Mujer[1:5], 
             lano3a.a2$Mujer[6:10], 
             lano3a.a2$Mujer[11:15], 
             lano3a.a2$Mujer[16:20], 
             lano3a.a2$Mujer[21:25])

plot(lano3a.a2$Mujer[1:5],type='o',axes=FALSE, ann=FALSE,ylim=grank,lwd=2)
axis(1,at=1:5,lab=ano3a.a2$Edad[1:5])
axis(2, las=1,at=NULL)
box()
lines(lano3a.a2$Mujer[6:10], type="o", pch=22, lty=2, lwd=2)
lines(lano3a.a2$Mujer[11:15], type="o", pch=22, lty=4, lwd=2)
lines(lano3a.a2$Mujer[16:20], type="o", pch=22, lty=5, lwd=2)
lines(lano3a.a2$Mujer[21:25], type="o", pch=22, lty=6, lwd=2)
title(main="TEM atribuible al Alzheimer en Mujeres \n Brasil, 2011", col.main="black", font.main=2)
title(xlab="Grupos de edad (Años)", col.lab=rgb(0,0.1,0),font.main=1)
title(ylab="Logaritmo(x 100.000)", col.lab=rgb(0,0.1,0),font.main=1)
legend("bottomright", title="Nivel de Escolaridad", c(paste(c(" 00", "1-3", "4-7","8-11","12+"))), 
       cex=.8, col=c(1,1,1,1,1), lty=c(1:2,4:6),lwd=2, bty="n", 
       y.intersp=.7, x.intersp=1.5, merge = TRUE,  ncol=2)
dev.off()
#######
pdf("alz10d3.pdf", paper= "a4r", width=14, height=8.5) # guardar plot PDF
grank<-range(ano3a.a2$Mujer[1:5], ano3a.a2$LIm[1:5], ano3a.a2$LSm[1:5],
             ano3a.a2$Mujer[6:10], ano3a.a2$LIm[6:10], ano3a.a2$LSm[6:10],
             ano3a.a2$Mujer[11:15], ano3a.a2$LIm[11:15], ano3a.a2$LSm[11:15],
             ano3a.a2$Mujer[16:20], ano3a.a2$LIm[16:20], ano3a.a2$LSm[16:20],
             ano3a.a2$Mujer[21:25], ano3a.a2$LIm[21:25], ano3a.a2$LSm[21:25])
# OJO VERIFICAR SI TIENEN LOGARITMOS ESTAS TASAS
plot(ano3a.a2$Mujer[1:5],type='o',axes=FALSE, ann=FALSE,ylim=grank,lwd=2)
axis(1,at=1:5,lab=ano3a.a2$Edad[1:5])
axis(2, las=1,at=NULL)
box()
lines(ano3a.a2$LSm[1:5], type="l", lty=3,lwd=2, col="gray50")
lines(ano3a.a2$LIm[1:5], type="l", lty=3, lwd=2, col="gray50")
lines(ano3a.a2$Mujer[6:10], type="o", pch=22, lty=2, lwd=2)
lines(ano3a.a2$LSm[6:10], type="l", lty=3,lwd=2, col="gray50")
lines(ano3a.a2$LIm[6:10], type="l", lty=3, lwd=2, col="gray50")
lines(ano3a.a2$Mujer[11:15], type="o", pch=22, lty=3, lwd=2)
lines(ano3a.a2$LSm[11:15], type="l", lty=3,lwd=2, col="gray50")
lines(ano3a.a2$LIm[11:15], type="l", lty=3, lwd=2, col="gray50")
lines(ano3a.a2$Mujer[16:20], type="o", pch=22, lty=4, lwd=2)
lines(ano3a.a2$LSm[16:20], type="l", lty=3,lwd=2, col="gray50")
lines(ano3a.a2$LIm[16:20], type="l", lty=3, lwd=2, col="gray50")
lines(ano3a.a2$Mujer[21:25], type="o", pch=22, lty=5, lwd=2)
lines(ano3a.a2$LSm[21:25], type="l", lty=3,lwd=2, col="gray50")
lines(ano3a.a2$LIm[21:25], type="l", lty=3, lwd=2, col="gray50")
title(main="Tasas de Mortalidad Mujeres(log)-Año 2011", col.main="black", font.main=2)
title(xlab="Grupos de Edad (Años)", col.lab=rgb(0,0.1,0),font.main=1)
title(ylab="tasa x 100.000", col.lab=rgb(0,0.1,0),font.main=1)
legend("topleft", title="Nivel de Escolaridad", c(paste(c(" 00", "1-3", "4-7","8-11","12+","ICR95%"))), 
       cex=0.6, col=c(1,1,1,1,1, "gray50"), lty=c(1:5,3),lwd=2, bty="n",
       y.intersp=0.8, x.intersp=2.0, merge = TRUE,  ncol=2)
dev.off()
