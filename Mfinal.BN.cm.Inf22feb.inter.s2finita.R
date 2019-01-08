################################# MODELO DE POISSON ################################
# Muestra aleatoria ponderada de la base de datos
setwd("/home/juan/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
ttasas0913.cr.inf.cm<-read.table("t.tasas0913.cr.cm.csv", header=T, sep=",", stringsAsFactors=F)
ttasas0913.cr.inf.cm$SEXO[ttasas0913.cr.inf.cm$SEXO==1]<-2 # Hombre
ttasas0913.cr.inf.cm$SEXO[ttasas0913.cr.inf.cm$SEXO==0]<-1 # Mujer
# Semilla primo de mersene
set.seed(2^29-1) 
require(runjags)
require(rjags)
require(reshape2)
library(ggplot2)
library(mcmcplots)
###################### MODELO BAYESIANO BINOMIAL NEGATIVO EN JAGS ######################
nbinomial4.int<-
"model{
# N observations
for (i in 1:N){
NUM[i] ~ dnegbin(p[i],r)
p[i] <- r/(r+lambda[i])
log(lambda[i])<-mu[i]
#Modelo
mu[i]<- log(E[i]) + b0 + SEX*SEXO[i] + ED[ED5[i]] + ES[ESC[i]] + Ano[ANO[i]] +
SEX.ED[ED5[i]]*(SEXO[i])+SEX.ES[ESC[i]]*(SEXO[i])
}
# Parameters a priori
SEX.ED[1]<-0 # Categoria referencia Edad 60-69 * sexo
SEX.ES[5]<-0 # Categoria referencia Edad 60-69 * sexo
ED[1]<-0 # Categoria referencia Edad 60-69
ES[5]<-0 #  0 anos de estudo
#SEX[1]<-0 #  0 sexo mujer
Ano[1]<-0 # ref. 2009
# Distribuciones a priori
b0~dnorm(0.0, 1.0E-6)
for (k in 2:5){SEX.ED[k]~dnorm(0.0, 1.0E-6)}
for (k in 1:4){SEX.ES[k]~dnorm(0.0, 1.0E-6)}
SEX~dnorm(-0.01,76.95)
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
# Calculo de medias estimadas(x100.000) (*****)
for (i in 1:N){Est[i]<-lambda[i]} # valores ajustados
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
p1<- 24
#
datosbn.cm <- list(N=N, p1=p1, NUM=ttasas0913.cr.inf.cm$FREC, SEXO=ttasas0913.cr.inf.cm$SEXO, ED5=ttasas0913.cr.inf.cm$ED5, 
              ESC=ttasas0913.cr.inf.cm$ESC, ANO=ttasas0913.cr.inf.cm$ANO, E=ttasas0913.cr.inf.cm$Pob.cr)     
#
inits <- list(list(b0=rnorm(1), SEX=rnorm(1), ED=c(NA, rnorm(4)), SEX.ED=c(NA, rnorm(4)), 
                   ES=c(rnorm(4),NA), SEX.ES=c(rnorm(4),NA), Ano=c(NA, rnorm(4))),
              list(b0=rnorm(1), SEX=rnorm(1), ED=c(NA, rnorm(4)), SEX.ED=c(NA, rnorm(4)), 
                   ES=c(rnorm(4),NA), SEX.ES=c(rnorm(4),NA), Ano=c(NA, rnorm(4))),
              list(b0=rnorm(1), SEX=rnorm(1), ED=c(NA, rnorm(4)), SEX.ED=c(NA, rnorm(4)), 
                   ES=c(rnorm(4),NA), SEX.ES=c(rnorm(4),NA), Ano=c(NA, rnorm(4))))
#
nbin.cm<- run.jags(model=nbinomial4.int, 
               monitor=c("b0","SEX", "ED", "ES", "Ano", "SEX.ED","SEX.ES","desvio2", 
                         "BIC", "LLP", "r"), data = datosbn.cm, inits = inits, 
               n.chains = 3, sample = 1000, burnin = 1000, adapt=1000, thin = 10)
Est.nb<-extend.jags(nbin.cm, add.monitor="Est", sample = 1000, thin = 10, 
                  drop.monitor=c("b0","SEX", "ED", "ES", "Ano", "SEX.ED","SEX.ES",
                                 "desvio2", "BIC", "LLP", "r"))
desvio2.nb<-extend.jags(nbin.cm, add.monitor="rchi", sample = 1000, thin = 3, 
                    drop.monitor=c("b0","SEX", "ED", "ES", "Ano", "SEX.ED","SEX.ES",
                                   "desvio2", "BIC", "LLP", "r"))
dic.nb.cm<-extract(nbin.cm, "dic", force.resample = FALSE, n.iter=1000, thin=3)
#....RESULTADOS....###################################
sink("result.bn.6jun.int.cm.txt") # guardar resultados
print(nbin.cm)
dic.nb.cm
nbin.cm$summary
nbin.cm$HPD
sink() # dejar de guardar
tab.pred.bn<-cbind("Año"=datosbn.cm$ANO, "Edad"=datosbn.cm$ED5, "Sex"=datosbn.cm$SEXO, 
                     "Esc"=datosbn.cm$ESC, "Freq"=datosbn.cm$NUM, "Pob"=datosbn.cm$E, 
                   round(Est.nb$HPD,1))
tab.desvio2.bn<-cbind("Año"=datosbn.cm$ANO, "Edad"=datosbn.cm$ED5, "Sex"=datosbn.cm$SEXO, 
                   "Esc"=datosbn.cm$ESC, "Freq"=datosbn.cm$NUM, "Pob"=datosbn.cm$E, 
                   round(desvio2.nb$HPD,1))
tab.pred.bn<-data.frame(tab.pred.bn)
tab.desvio2.bn<-data.frame(tab.desvio2.bn)
#
# Guardar graficos
plot(datosbn.cm$NUM,tab.pred.bn$Median, main = "Model NB sin Missing")
abline(lsfit(datosbn.cm$NUM,tab.pred.bn$Median), col="red", lty=2)
pdf("Post-MCMCnbin-Global22feb.pdf", paper="a4r", width=9, height=7)
plot(nbin.cm, plot.type = c("trace", "ecdf", "histogram", "key", "autocorr"), layout=c(4,4))
dev.off()
#
pdf("Post-MCMCnbin-Global22feb.pdf", paper="a4r", width=9, height=7)
plot(Est.nb, plot.type = c("trace", "ecdf", "histogram", "key", "autocorr"), layout=c(4,4))
dev.off()
pdf("Post-MCMCnbin_cadF1-22feb.pdf")
plot(nbin.cm$mcmc[[1]])
dev.off()
#
pdf("Post-MCMCnbin_cadF2-22feb.pdf")
plot(nbin.cm$mcmc[[2]])
dev.off()
#
#install.packages("mcmcplots")
library(mcmcplots)
pdf("Post-MCMCPo_caterIdade-22feb.pdf")
caterplot(nbin.cm, parms="ED")
dev.off()
pdf("Post-MCMCPo_cater-ESC2-22feb.pdf")
caterplot(nbin.cm, parms="ES")
dev.off()
pdf("Post-MCMCPo_cater1-ANO-22feb.pdf")
caterplot(nbin.cm, parms="Ano")
dev.off()

########### GRAFICOS VALIDACION MODELO ###############
# ajuste validacion del modelo
setwd("~/PhD_Demografia/TesePhD/TesisLatex/figure")
pdf("m5.pdf", paper="a4r", width=9, height=7)
op <- par(mfrow = c(2, 1))
plot(tab.pred.bn$Median, tab.desvio2.bn$Median, xlab = "Valores Medianos Ajustados",
     ylab = "Residual Bayesiano Estandarizado", ylim=c(-4,4))
abline(h = 0, col = "red", lty=2)
plot(1:250, tab.desvio2.bn$Median, xlab = "Tiempo",
     ylab = "Residual Bayesiano Estandarizado",  ylim=c(-4,4))
abline(h = 0, col = "red", lty=2)
dev.off()

rm(list = setdiff(ls(), lsf.str()))
