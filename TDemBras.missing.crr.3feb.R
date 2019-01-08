###########################################################################################
setwd("/home/juan/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
################################### TABLA DE POISSON ################################
# 1.CONSTRUCCION DE TABLA SIN MISSING
tpoisCR.CRR.SM<-read.table("tpoisCR.CRR.SM.csv", header=T, sep=",", stringsAsFactors=F)
# Proyecciones poblacion y frecuencia de demencia
# Correccion al censo 2010
tpois2010c.sm<-subset(tpoisCR.CRR.SM, ANO==2)
ttasas2010.sm<-tpois2010c.sm
ttasas2010.sm$Pob<-round(TPoBrc2010b.sm$Freq*exp((0)*rt),0)
n1<-sum(BR65t[6,3]+BR65t[6,4])
n2<-sum(ttasas2010.sm$Pob)
ttasas2010.sm$Pob.cr<-round((n1/n2)*ttasas2010.sm$Pob,0)
ttasas2010.sm$Pob<-NULL
#
# Frecuencia de demencia y proyeccion 2009 
tpois2009c.sm<-subset(tpoisCR.CRR.SM, ANO==1)
ttasas2009.sm<-tpois2009c.sm
ttasas2009.sm$Pob.cr<-round(ttasas2010.sm$Pob.cr*exp((-1)*rt),0)
write.csv(ttasas2009.sm, "t.tasas2009.cr.sm.csv") # salvar tabla de tasas Brasil 2009
#
# Frecuencia de demencia y proyeccion 2010 
write.csv(ttasas2010.sm, "t.tasas2010.cr.sm.csv") # salvar tabla de tasas Brasil 2010
#
# Frecuencia de demencia y proyeccion 2011
tpois2011c.sm<-subset(tpoisCR.CRR.SM, ANO==3)
ttasas2011.sm<-tpois2011c.sm
ttasas2011.sm$Pob.cr<-round(ttasas2010.sm$Pob.cr*exp((1)*rt),0)
write.csv(ttasas2011.sm, "t.tasas2011.cr.sm.csv") # salvar tabla de tasas Brasil 2011
#
# Frecuencia de demencia y proyeccion 2012
tpois2012c.sm<-subset(tpoisCR.CRR.SM, ANO==4)
ttasas2012.sm<-tpois2012c.sm
ttasas2012.sm$Pob.cr<-round(ttasas2010.sm$Pob.cr*exp((2)*rt),0)
write.csv(ttasas2012.sm, "t.tasas2012.cr.sm.csv") # salvar tabla de tasas Brasil 2012
#
# Frecuencia de demencia y proyeccion 2013
tpois2013c.sm<-subset(tpoisCR.CRR.SM, ANO==5)
ttasas2013.sm<-tpois2013c.sm
ttasas2013.sm$Pob.cr<-round(ttasas2010.sm$Pob.cr*exp((3)*rt),0)
write.csv(ttasas2013.sm, "t.tasas2013.cr.sm.csv") # salvar tabla de tasas Brasil 2013
#
# Union de las bases de años 2009-2013
setwd("/home/juan/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
ttasas2009.sm<-read.table("t.tasas2009.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2010.sm<-read.table("t.tasas2010.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2011.sm<-read.table("t.tasas2011.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2012.sm<-read.table("t.tasas2012.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2013.sm<-read.table("t.tasas2013.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas0913.sm<-rbind(ttasas2009.sm, ttasas2010.sm, ttasas2011.sm, ttasas2012.sm, ttasas2013.sm)
####################3Pruebas de confiabilidad de los datos 2010 ###########################
N<-nrow(tpois2010c.sm);N
M<-sample(1:N,1); M
tpois2010c.sm[M,]
TPoBrc2010.sm[M,]
write.csv(ttasas0913.sm, "t.tasas0913.cr.sm.csv") # salvar tabla de tasas Brasil 2009-2013
#######################################################################################
# 2. CONSTRUCCION DE TABLA COMN MISSING
tpoisCR.CRR.CM<-read.table("tpoisCR.CRR.CM.csv", header=T, sep=",", stringsAsFactors=F)
# Proyecciones poblacion y frecuencia de demencia
# Correccion al censo 2010
tpois2010c.cm<-subset(tpoisCR.CRR.CM, ANO==2)
ttasas2010.cm<-tpois2010c.cm
ttasas2010.cm$Pob<-round(TPoBrc2010b.cm$Freq*exp((0)*rt),0)
n1<-sum(BR65t[6,3]+BR65t[6,4])
n2<-sum(ttasas2010.cm$Pob)
ttasas2010.cm$Pob.cr<-round((n1/n2)*ttasas2010.cm$Pob,0)
ttasas2010.cm$Pob<-NULL
#
# Frecuencia de demencia y proyeccion 2009 
tpois2009c.cm<-subset(tpoisCR.CRR.CM, ANO==1)
ttasas2009.cm<-tpois2009c.cm
ttasas2009.cm$Pob.cr<-round(ttasas2010.cm$Pob.cr*exp((-1)*rt),0)
write.csv(ttasas2009.cm, "t.tasas2009.cr.cm.csv") # salvar tabla de tasas Brasil 2009
#
# Frecuencia de demencia y proyeccion 2010 
write.csv(ttasas2010.cm, "t.tasas2010.cr.cm.csv") # salvar tabla de tasas Brasil 2010
#
# Frecuencia de demencia y proyeccion 2011
tpois2011c.cm<-subset(tpoisCR.CRR.CM, ANO==3)
ttasas2011.cm<-tpois2011c.cm
ttasas2011.cm$Pob.cr<-round(ttasas2010.cm$Pob.cr*exp((1)*rt),0)
write.csv(ttasas2011.cm, "t.tasas2011.cr.cm.csv") # salvar tabla de tasas Brasil 2011
#
# Frecuencia de demencia y proyeccion 2012
tpois2012c.cm<-subset(tpoisCR.CRR.CM, ANO==4)
ttasas2012.cm<-tpois2012c.cm
ttasas2012.cm$Pob.cr<-round(ttasas2010.cm$Pob.cr*exp((2)*rt),0)
write.csv(ttasas2012.cm, "t.tasas2012.cr.cm.csv") # salvar tabla de tasas Brasil 2012
#
# Frecuencia de demencia y proyeccion 2013
tpois2013c.cm<-subset(tpoisCR.CRR.CM, ANO==5)
ttasas2013.cm<-tpois2013c.cm
ttasas2013.cm$Pob.cr<-round(ttasas2010.cm$Pob.cr*exp((3)*rt),0)
write.csv(ttasas2013.cm, "t.tasas2013.cr.cm.csv") # salvar tabla de tasas Brasil 2013
#
# Union de las bases de años 2009-2013
setwd("/home/juan/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
ttasas2009.cm<-read.table("t.tasas2009.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2010.cm<-read.table("t.tasas2010.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2011.cm<-read.table("t.tasas2011.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2012.cm<-read.table("t.tasas2012.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas2013.cm<-read.table("t.tasas2013.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas0913.cm<-rbind(ttasas2009.cm, ttasas2010.cm, ttasas2011.cm, ttasas2012.cm, ttasas2013.cm)
####################3Pruebas de confiabilidad de los datos 2010 ###########################
N<-nrow(tpois2010c.cm);N
M<-sample(1:N,1); M
tpois2010c.cm[M,]
TPoBrc2010.cm[M,]
write.csv(ttasas0913.cm, "t.tasas0913.cr.cm.csv") # salvar tabla de tasas Brasil 2009-2013
#######################################################################################

############################ MODELOS CLASICOS DE REGRESION ################################
setwd("/home/juan/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
#
# ANALISIS PARA DATOS AGREGADOS SIN Y CON MISSINGS 
ttasas0913.cr.sm<-read.table("t.tasas0913.cr.sm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas0913.cr.cm<-read.table("t.tasas0913.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
#ttasas0913.cr.cm[1]<-NULL
#names(ttasas0913.cr.sm)[5] <- "FREC"
#names(ttasas0913.cr.cm)[5] <- "FREC"
#ttasas0913.cr.cm[1]<-NULL
# Cambio de referencia en el factor
#tab.pois.m$ESC<- C(tab.pois.m$ESC, contr.treatment, base=3)
#write.csv(ttasas0913.cr.sm, "t.tasas0913.cr.sm.csv") # salvar tabla de tasas Brasil 2009-2013
#write.csv(ttasas0913.cr.cm, "t.tasas0913.cr.cm.csv") # salvar tabla de tasas Brasil 2009-2013
#
# 1. Modelo clasico poisson OFFSET sin missing
m1.sm = glm(FREC ~ factor(SEXO)+factor(ED5)+factor(ANO)+factor(ESC)+ 
              factor(SEXO)*factor(ED5)+ factor(SEXO)*factor(ESC)+
              offset (log(Pob.cr)), family = poisson(link = 'log'), data = ttasas0913.cr.sm)  
summary(m1.sm)
yhat1.sm <- predict(m1.sm)
plot(log(ttasas0913.cr.sm$FREC),yhat1.sm)
abline(lsfit(log(ttasas0913.cr.sm$FREC),yhat1.sm), col="blue")
plot(m1.sm)

# 2. Modelo clasico poisson OFFSET con missing imputados
m1.cm = glm(FREC ~ factor(SEXO)+factor(ED5)+factor(ANO)+factor(ESC)+ 
              factor(SEXO):factor(ED5)+ factor(SEXO):factor(ESC)+
              offset(log(Pob.cr)), family = poisson(link = 'log'), data = ttasas0913.cr.cm)  
summary(m1.cm)
yhat1.cm <- predict(m1.cm)
plot(log(ttasas0913.cr.cm$FREC),yhat1.cm)
abline(lsfit(log(ttasas0913.cr.cm$FREC),yhat1.cm),  col="blue")
op <- par(mfrow = c(2, 2))
plot(m1.cm)

# MODELO CLASICO BINOMIAL NEGATIVO
# 1. sin missing
require(MASS)
m3.sm <- glm.nb(FREC ~ factor(SEXO)+factor(ED5)+factor(ANO)+factor(ESC)+
                  factor(SEXO):factor(ED5)+factor(SEXO):factor(ESC)+
                  offset(log(Pob.cr)), data = ttasas0913.cr.sm)
summary(m3.sm)
yhat3.sm <- predict(m3.sm)
plot(log(ttasas0913.cr.sm$FREC),yhat3.sm)
abline(lsfit(log(ttasas0913.cr.sm$FREC),yhat3.sm),  col="red")
(est <- cbind(Estimate = coef(m3.sm), confint(m3.sm))) # problemas con missing
exp(est)
op <- par(mfrow = c(2, 2))
plot(m3.sm)
# 1. con missing imputados
m3.cm <- glm.nb(FREC ~ factor(SEXO)+factor(ED5)+factor(ANO)+factor(ESC)+ 
                  factor(SEXO):factor(ED5)+factor(SEXO):factor(ESC)+
                  offset(log(Pob.cr)), data = ttasas0913.cr.cm)
summary(m3.cm)
yhat3.cm <- predict(m3.cm)
plot(log(ttasas0913.cr.cm$FREC),yhat3.cm)
abline(lsfit(log(ttasas0913.cr.cm$FREC),yhat3.cm), col="red", lwd=2, lty=2)
(est <- cbind(Estimate = coef(m3.cm), confint(m3.cm))) # problemas con missing
exp(est)
op <- par(mfrow = c(2, 2))
plot(m3.cm)
#
rm(list = setdiff(ls(), lsf.str())) # removes all objects except for functions
