################################# MODELO DE POISSON ################################
# Muestra aleatoria ponderada de la base de datos
setwd("~/Documentos/PhD_Demografia/ModelosR/ModeloFinal")
ttasas0913.cr.inf.cm<-read.table("t.tasas0913.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas0913.cr.inf.cm$SEXO[ttasas0913.cr.inf.cm$SEXO==1]<-2 # Hombre
ttasas0913.cr.inf.cm$SEXO[ttasas0913.cr.inf.cm$SEXO==0]<-1 # Mujer
set.seed(2^29-1) # Semilla primo de mersene
#install.packages("runjags")
require(runjags)
require(reshape2)
library(ggplot2)
#install.packages("mcmcplots")
library(mcmcplots)
###################### MODELO BAYESIANO BINOMIAL NEGATIVO EN JAGS ######################
nbinomial.infb<-
"model{
# N observations
for (i in 1:N){
NUM[i] ~ dnegbin(p[i],r)
p[i] <- r/(r+lambda[i])
log(lambda[i])<-mu[i]
#Modelo
mu[i]<- log(E[i]) + b0 + SEX[SEXO[i]] + ED[ED5[i]] + ES[ESC[i]] + Ano[ANO[i]]
}
# Parameters a priori
ED[1]<-0 # Categoria referencia Edad 60-69
ES[5]<-0 #  0 anos de estudo
SEX[1]<-0 #  0 sexo mujer
Ano[1]<-0 # ref. 2009
b0~dnorm(0.0, 1.0E-6)
SEX[2]~dnorm(-0.01,76.95)
ED[2]~dnorm(0.833,50.3)
ED[3]~dnorm(1.56,50.3)
ED[4]~dnorm(2.21,52.5)
ED[5]~dnorm(2.59,11.9)
Ano[2]~dnorm(0, 1.0E-6)
Ano[3]~dnorm(0, 1.0E-6)
Ano[4]~dnorm(0, 1.0E-6)
Ano[5]~dnorm(0, 1.0E-6)
ES[1]~dnorm(1.09, 15.67)
ES[2]~dnorm(0.95, 15.67)
ES[3]~dnorm(0.84, 141.7)
ES[4]~dnorm(0.37, 141.7)
# Hyperparametros de la priori
r ~ dgamma(0.01,0.01)
a<-1/r
# Calculo de tasas estimadas(x100.000) (*****)
for (i in 1:N){Est0[i]<-lambda[i]} # valores ajustados
# Residual Pearson
for (i in 1:N){rchi[i]<-(NUM[i]-lambda[i])/sqrt(lambda[i]+(lambda[i])^2/r)}
desvio2<-inprod(rchi,rchi)
# BIC
# sum(loggam(NUM[]+r)-loggam(r)-loggam(NUM[]+1))
logLikBN<-sum(mu[]*log(p[])+r*log(1-p[]))
BIC<--2*logLikBN+p1*log(N) # comparar solo modelos de una misma distr.
LLP<--2*logLikBN
}"
#
N<-nrow(ttasas0913.cr.inf.cm)
p1<- 14
#
datosbn.inf.cm <- list(N=N, p1=p1, NUM=ttasas0913.cr.inf.cm$FREC, SEXO=ttasas0913.cr.inf.cm$SEXO, ED5=ttasas0913.cr.inf.cm$ED5, 
              ESC=ttasas0913.cr.inf.cm$ESC, ANO=ttasas0913.cr.inf.cm$ANO, E=ttasas0913.cr.inf.cm$Pob.cr)     
#
inits <- list(list(b0=rnorm(1), SEX=c(NA,rnorm(1)), ED=c(NA, rnorm(4)), ES=c(rnorm(4), NA),
                Ano=c(NA, rnorm(4)), r=runif(1,0,50)),
              list(b0=rnorm(1), SEX=c(NA,rnorm(1)), ED=c(NA, rnorm(4)), ES=c(rnorm(4), NA),
                   Ano=c(NA, rnorm(4)), r=runif(1,0,50)),
              list(b0=rnorm(1), SEX=c(NA,rnorm(1)), ED=c(NA, rnorm(4)), ES=c(rnorm(4), NA),
                   Ano=c(NA, rnorm(4)), r=runif(1,0,50)))
#
nbin.inf.cm<- run.jags(model=nbinomial.infb, 
                       monitor=c("b0","SEX", "ED", "ES", "Ano", "desvio2","r", "BIC", "LLP"), 
                       data = datosbn.inf.cm, inits = inits, n.chains = 3, sample = 1000, 
                       burnin = 1000, adapt=1000, thin = 10)
Est0.bn<-extend.jags(nbin.inf.cm, add.monitor="Est0", sample = 1000, thin = 10, 
                    drop.monitor=c("b0","SEX", "ED", "ES", "Ano","desvio2","r", "BIC", "LLP"))
desvio2.bn<-extend.jags(nbin.inf.cm, add.monitor="rchi", sample = 1000, thin = 10, 
                      drop.monitor=c("b0","SEX", "ED", "ES", "Ano","desvio2","r", "BIC", "LLP"))
dic.bn.cm<-extract(nbin.inf.cm, "dic", force.resample = FALSE, n.iter=1000, thin=3)
#
###################......RESULTADOS....###################################
sink("result.bn.6jun.inf.cm.txt") # guardar resultados
print(nbin.inf.cm)
dic.bn.cm
nbin.inf.cm$summary
nbin.inf.cm$HPD
sink() # dejar de guardar
Est.bn.HPD<-summary(Est0.bn)
tab.pred.bn.inf.cm<-cbind("Año"=datosbn.inf.cm$ANO, "Edad"=datosbn.inf.cm$ED5, "Sex"=datosbn.inf.cm$SEXO, 
                   "Esc"=datosbn.inf.cm$ESC, "Freq"=datosbn.inf.cm$NUM, "Pob"=datosbn.inf.cm$E, 
                   round(Est.bn.HPD[,1:3],3))
desvio2.bn.HPD<-summary(desvio2.bn)
tab.desvio2.bn.inf.cm<-cbind("Año"=datosbn.inf.cm$ANO, "Edad"=datosbn.inf.cm$ED5, "Sex"=datosbn.inf.cm$SEXO, 
                          "Esc"=datosbn.inf.cm$ESC, "Freq"=datosbn.inf.cm$NUM, "Pob"=datosbn.inf.cm$E, 
                          round(desvio2.bn.HPD[,1:3],2))
tab.pred.bn.inf.cm<-data.frame(tab.pred.bn.inf.cm)
tab.desvio2.bn.inf.cm<-data.frame(tab.desvio2.bn.inf.cm)
options(scipen=999)
# Guardar graficos
plot(tab.pred.bn.inf.cm$Freq,tab.pred.bn.inf.cm$Median, main = "Model NB con Missing Imputado")
abline(lsfit(tab.pred.bn.inf.cm$Freq,tab.pred.bn.inf.cm$Median), col="red", lty=2)
pdf("Post-MCMCnbin-Global26marz.pdf",  paper="a4r", width=11.69, height=8.27)
plot(nbin.inf.cm, plot.type = c("trace", "density", "histogram", "autocorr"), layout=c(4,4))
dev.off()
# valores preditos
pdf("Post-MCMCnbin-Global26ene.pdf", paper="a4r", width=9, height=7)
plot(Est0.bn, plot.type = c("trace", "density", "histogram", "autocorr"), layout=c(4,4))
dev.off()
pdf("Post-MCMCnbin_cadF1-26ene.pdf")
plot(nbin.inf.cm$mcmc[[1]])
dev.off()
#
pdf("Post-MCMCnbin_cadF2-26ene.pdf")
plot(nbin.inf.cm$mcmc[[2]])
dev.off()
#
#install.packages("mcmcplots")
library(mcmcplots)
pdf("ost_cater_ED.pdf.pdf")
caterplot(nbin.inf.cm, parms="ED", horizontal = T,
          labels = c("65-69", "70-74", "75-79","80-84","85+"),collapse = F,
          cex.labels = 1)
dev.off()
pdf("Post_cater_ES.pdf")
caterplot(nbin.inf.cm, parms="ES",horizontal = T, 
          labels = c("Nulo", "1-3", "8-11","4-7","12+"),collapse = F,cex.labels = 1)
dev.off()
pdf("Post-MCMCPo_cater1-ANO-26ene.pdf")
caterplot(nbin.inf.cm, parms="Ano")
dev.off()
######
denplot(nbin.inf.cm, parms="ED")
denplot(nbin.inf.cm, parms="ES",collapse=TRUE, greek=TRUE, ci=0.95)
denplot(nbin.inf.cm, parms="Ano")
#
########### GRAFICOS VALIDACION MODELO ###############
# ajuste validacion del modelo
setwd("~/PhD_Demografia/TesePhD/TesisLatex/figure")
pdf("mfinal.pdf", paper="a4r", width=9, height=7)
op <- par(mfrow = c(1, 2))
plot(tab.pred.bn.inf.cm$Median, tab.desvio2.bn.inf.cm$Median, 
     xlab = "Mediana Posterior Estimada", ylab = "Desvio Residual Bayesiano", 
     ylim=c(-3,3), main = "Modelo Final con Missing Imputados")
abline(h = 0, col = "red", lty=2)
plot(1:250, tab.desvio2.bn.inf.cm$Median, xlab = "Tiempo",
     ylab = "Desvio Residual Bayesiano",  ylim=c(-3,3), main = "Modelo Final con Missing Imputados")
abline(h = 0, col = "red", lty=2)
dev.off()
#
pdf("NBinf.pdf", paper="a4r", width=14, height=8)
op <- par(mfrow = c(1, 2))
plot(tab.pred.bn.inf.sm$Freq,tab.pred.bn.inf.sm$Median, main = "Modelo Final sin Missing",
      xlab = "Valor Observado", ylim=c(0,2500), xlim=c(0,2500),
     ylab = "Mediana Posterior Estimada")
abline(lsfit(datosbn.inf.sm$NUM,tab.pred.bn.inf.sm$Median), col="red", lty=2)
#
plot(tab.pred.bn.inf.cm$ts,tab.pred.bn.inf.cm$Median, main = "Modelo Final con Missing Imputados",
      xlab = "Valor Observado", ylim=c(0,2500), xlim=c(0,2500),
     ylab = "Mediana Posterior Estimada")
abline(lsfit(tab.pred.bn.inf.cm$Freq,tab.pred.bn.inf.cm$Median), col="red", lty=2)
dev.off()
#################################
# Graficas MCMC individuales
nbin.inf.cm.out <- combine.mcmc(nbin.inf.cm, vars=c("b0","SEX", "ED", "ES", "Ano"), 
                                thin=10, return.samples = 1000)
# Datos
nbin.inf.cm.out1<-data.frame(nbin.inf.cm.out)
#write.csv(nbin.inf.cm.out1, "nbin.inf.cm.out1.csv")
#
setwd("~/PhD_Demografia/TesePhD/TesisLatex/figure")
## SEXO ###
pdf("postSex.pdf", paper="a4r", width=9, height=7)
sex.bn <- cbind("ap"=rnorm(1000, -0.01, 0.114), nbin.inf.cm.out1[3])
sex.bn <-data.frame(sex.bn)
sex.bn.m <-melt(sex.bn)
p<-ggplot(sex.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Sexo(H):",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
# GRUPOS DE EDAD
## edad2 ###
# prior
ed2.bn <- cbind("ap"=rnorm(1000, 0.833, 0.140999), nbin.inf.cm.out1[5])
ed2.bn <-data.frame(ed2.bn)
ed2.bn.m <-melt(ed2.bn)
pdf("postEd2.pdf", paper="a4r", width=9, height=7)
p<-ggplot(ed2.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Edad(70-74 años): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()

## edad3 ###
# prior
ed3.bn <- cbind("ap"=rnorm(1000, 1.56,0.140999), nbin.inf.cm.out1[6])
ed3.bn <-data.frame(ed3.bn)
ed3.bn.m <-melt(ed3.bn)
pdf("posted3.pdf", paper="a4r", width=9, height=7)
p<-ggplot(ed3.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Edad(75-79 años): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
#
## edad4 ###
# prior
ed4.bn <- cbind("ap"=rnorm(1000, 2.21, 0.1380131), nbin.inf.cm.out1[7])
ed4.bn <-data.frame(ed4.bn)
ed4.bn.m <-melt(ed4.bn)
pdf("posted4.pdf", paper="a4r", width=9, height=7)
p<-ggplot(ed4.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Edad(80-84 años): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
## edad5 ###
# prior
ed5.bn <- cbind("ap"=rnorm(1000, 2.59, 0.2898855), nbin.inf.cm.out1[8])
ed5.bn <-data.frame(ed5.bn)
ed5.bn.m <-melt(ed5.bn)
pdf("posted5.pdf", paper="a4r", width=9, height=7)
p<-ggplot(ed5.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Edad(85+ años): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
#
## escolaridad 1 ###
pdf("postEsc1.pdf", paper="a4r", width=9, height=7)
es1.bn <- cbind("ap"=rnorm(1000, 1.09, 0.2526187), nbin.inf.cm.out1[9])
es1.bn <-data.frame(es1.bn)
es1.bn.m <-melt(es1.bn)
p<-ggplot(es1.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Escolaridad (0 anos): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
#
## escolaridad 2 ###
es2.bn <- cbind("ap"=rnorm(1000, 0.84, 0.2526187), nbin.inf.cm.out1[10])
es2.bn <-data.frame(es2.bn)
es2.bn.m <-melt(es2.bn)
pdf("postEsc2.pdf", paper="a4r", width=9, height=7)
p<-ggplot(es2.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Escolaridad (1-3 anos): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
#
## escolaridad 3 ###
es3.bn <- cbind("ap"=rnorm(1000, 0.80, 0.08400692), nbin.inf.cm.out1[11])
es3.bn <-data.frame(es3.bn)
es3.bn.m <-melt(es3.bn)
pdf("postEsc3.pdf", paper="a4r", width=9, height=7)
p<-ggplot(es3.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Escolaridad (4-7 anos): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
#
## escolaridad 4 ###
es4.bn <- cbind("ap"=rnorm(1000, 0.37, 0.08400692), nbin.inf.cm.out1[12])
es4.bn <-data.frame(es4.bn)
es4.bn.m <-melt(es4.bn)
pdf("postEsc4.pdf", paper="a4r", width=9, height=7)
p<-ggplot(es4.bn.m, aes(x = value, group=variable, fill=variable)) +  theme_classic()+
  geom_density(alpha=0.5, size=.5)+ 
  scale_fill_grey(name="Escolaridad (8-11 anos): ",labels=c("Dist. Apriori", "Dist. A posteriori"))
p+theme(text=element_text(size=14), legend.position="bottom",legend.text=element_text(size=14))+ 
  ylab("Densidad(%)")+ xlab("Valores del Parámetro")
dev.off()
rm(list = setdiff(ls(), lsf.str()))
