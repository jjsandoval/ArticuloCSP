setwd("~/PhD_Demografia/TesePhD/ModelosR/ModeloFinal")
#write.csv(tab.pred.bn.inf.cm, "tab.pred.bn.inf.cm.csv")
#write.csv(tab.pred.bn.NOinf.cm, "tab.pred.bn.NOinf.cm.csv")
tab.pred.nb.inf.cm<-read.table("tab.pred.bn.inf.cm.csv", header=T, sep=",", stringsAsFactors=F)
tab.pred.nb.NOinf.cm<-read.table("tab.pred.bn.NOinf.cm.csv", header=T, sep=",", stringsAsFactors=F)
tab.pred.sm<-read.table("tab.pred.sm.csv", header=T, sep=",", stringsAsFactors=F) # sin corregir nada
#tab.pred.sm$Pob.cr<-ttasas0913.cr.sm$Pob.cr
tab.pred.cm<-read.table("tab.pred.cm.csv", header=T, sep=",", stringsAsFactors=F)
#tab.pred.cm$Pob.cr<-ttasas0913.cr.cm$Pob.cr
#tpoisCRb.sm<-tpoisCRb.sm[order(tpoisCRb.sm$ANO),] # ordenar
#write.csv(tab.pred.sm,"tab.pred.sm.csv") 
#write.csv(tab.pred.cm,"tab.pred.cm.csv") 
#
# media geometrica
g.mean<-function(x){
  exp(mean(log(x)))
}
#
# tasa bruta estimada SM y CM
tab.pred.sm$tsb<-round((tab.pred.sm$FREC/tab.pred.sm$Pob.cr)*100000,3) # 1
tab.pred.cm$tsb<-round((tab.pred.cm$Freq/tab.pred.cm$Pob.cr)*100000,3) # 2
tab.pred.nb.NOinf.cm$tse<-round((tab.pred.nb.NOinf.cm$Median/tab.pred.nb.NOinf.cm$Pob)*100000,3) # 3
tab.pred.nb.inf.cm$tse<-round((tab.pred.nb.inf.cm$Median/tab.pred.nb.inf.cm$Pob)*100000,3) # 4

# INICIO
# tasas brutas y estimadas por SEXO (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, tab.pred.sm$SEXO, median) # tasa bruta obs corr SM # 1
tapply(tab.pred.cm$tsb, tab.pred.cm$Sex, median) # tasa bruta obs corr CM # 2
tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Sex, median) # modelo CM no informativa # 3
tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Sex, median) # modelo CM informativa # 4

# ALZHEIMER
# tasas brutas y estimadas por SEXO (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, tab.pred.sm$SEXO, median)*.72 # tasa bruta SM
tapply(tab.pred.cm$tsb, tab.pred.nb.inf.cm$Sex, median)*.72 # tasa bruta CM
tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Sex, median)*.72 # modelo CM no informativa
tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Sex, median)*.72 # modelo CM informativa

# li
# tasas brutas y estimadas por SEXO (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, tab.pred.sm$Sex, median)*.68 # tasa bruta SM
tapply(tab.pred.cm$tsb, tab.pred.nb.inf.cm$Sex, median)*.68 # tasa bruta CM
tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Sex, median)*.68 # modelo CM no informativa
tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Sex, median)*.68 # modelo CM informativa

#ls
# tasas brutas y estimadas por SEXO (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, tab.pred.sm$Sex, median)*.76 # tasa bruta SM
tapply(tab.pred.cm$tsb, tab.pred.nb.inf.cm$Sex, median)*.76 # tasa bruta CM
tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Sex, median)*.76 # modelo CM no informativa
tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Sex, median)*.76 # modelo CM informativa
#
# tasas brutas y estimadas por EDAD (todos los datos ya fueron corregidos x subregistros)
round(tapply(tab.pred.sm$tsb, tab.pred.sm$ED5, median),1) # tasa bruta SM # 1
round(tapply(tab.pred.cm$tsb, tab.pred.cm$Edad, median),1) # tasa bruta CM # 2
round(tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Edad, median),1) # modelo CM no informativa # 3
round(tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Edad, median),1) # modelo CM informativa # 4
# ALZHEIMER
# tasas brutas y estimadas por EDAD (todos los datos ya fueron corregidos x subregistros)
round(tapply(tab.pred.sm$tsb, tab.pred.sm$ED5, median)*.72,1) # tasa bruta SM # 1
round(tapply(tab.pred.cm$tsb, tab.pred.cm$Edad, median)*.72,1) # tasa bruta CM # 2
round(tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Edad, median)*.72,1) # modelo CM no informativa # 3
round(tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Edad, median)*.72,1) # modelo CM informativa # 4
#
# tasas brutas y estimadas por ESCOLARIDAD (todos los datos ya fueron corregidos x subregistros)
round(tapply(tab.pred.sm$tsb, tab.pred.sm$ESC, median),1) # tasa bruta SM # 1
round(tapply(tab.pred.cm$tsb, tab.pred.cm$Esc, median),1) # tasa bruta CM # 2
round(tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Esc, median),1) # modelo CM no informativa # 3
round(tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Esc, median),1) # modelo CM informativa # 4
# alzheimer ESC
# tasas brutas y estimadas por ESCOLARIDAD (todos los datos ya fueron corregidos x subregistros)
round(tapply(tab.pred.sm$tsb, tab.pred.sm$ESC, median)*.72,1) # tasa bruta SM # 1
round(tapply(tab.pred.cm$tsb, tab.pred.cm$Esc, median)*.72,1) # tasa bruta CM # 2
round(tapply(tab.pred.nb.NOinf.cm$tse, tab.pred.nb.NOinf.cm$Esc, median)*.72,1) # modelo CM no informativa # 3
round(tapply(tab.pred.nb.inf.cm$tse, tab.pred.nb.inf.cm$Esc, median)*.72,1) # modelo CM informativa # 4

# tasas brutas y estimadas por Edad y Sexo. (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, list(tab.pred.sm$Sex, tab.pred.sm$Edad), median) # tasa bruta SM
tapply(tab.pred.cm$tsb, list(tab.pred.cm$Sex, tab.pred.cm$Edad), median) # tasa bruta CM
tapply(tab.pred.nb.NOinf.cm$tse, list(tab.pred.nb.NOinf.cm$Sex, tab.pred.nb.NOinf.cm$Edad), median) # modelo CM no informativa
tapply(tab.pred.nb.inf.cm$tse, list(tab.pred.nb.inf.cm$Sex, tab.pred.nb.inf.cm$Edad), median) # modelo CM informativa

# tasas brutas y estimadas por Edad, Sexo y Año. 
# (todos los datos ya fueron corregidos x subregistros)
tapply(tab.pred.sm$tsb, list(tab.pred.sm$Edad, tab.pred.sm$Sex, tab.pred.sm$Año), median) # tasa bruta SM
tapply(tab.pred.cm$tsb, list(tab.pred.cm$Edad, tab.pred.cm$Sex, tab.pred.cm$Año), median) # tasa bruta CM
tapply(tab.pred.nb.NOinf.cm$tse, list(tab.pred.nb.NOinf.cm$Edad, tab.pred.nb.NOinf.cm$Sex, tab.pred.cm$Año), median) # modelo CM no informativa
tapply(tab.pred.nb.inf.cm$tse, list(tab.pred.nb.inf.cm$Edad, tab.pred.nb.inf.cm$Sex, tab.pred.nb.inf.cm$Año), median) # modelo CM informativa

#######################################################################################
require(xtable)
write.csv(tab.pred.bn.inf.cm2,"tab.pred.bn.inf.cm2.csv") 
tab.pred.nb.inf.cm2<-read.table("tab.pred.bn.inf.cm2.csv", header=T, sep=",", stringsAsFactors=F)

# tabla AÑO 2009
# Demencia
ano1.sex1<-subset(tab.pred.nb.inf.cm2, Año==1 & Sex==1, select = c("Esc", "Edad","Median", 
                                                    "Lower95", "Upper95"))
ano1.sex2<-subset(tab.pred.nb.inf.cm2, Año==1 & Sex==2, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano1<-cbind(ano1.sex1, ano1.sex2[3:5])
ano1$Edad[ano1$Edad == 1] <- "65-69"
ano1$Edad[ano1$Edad == 2] <- "70-74"
ano1$Edad[ano1$Edad == 3] <- "75-79"
ano1$Edad[ano1$Edad == 4] <- "80-84"
ano1$Edad[ano1$Edad == 5] <- "85+"
xtable(ano1, caption = "Estimaciones de las tasas de mortalidad(x 100.000 hab.) para el año
2009 por Nivel de escolaridad, edad y sexo a partir de los resultados  del 
modelo bayesiano de Regresion Binomial Negativa con información a priori vía Meta-análisis
Intervalos de credibilidad HPD al 95%", digits=2)
# Alzheimer
ano1.a<-ano1[3:8]*.72
ano1.a2<-cbind("Esc"=ano1$Esc, "Edad"=ano1$Edad,ano1.a)
#
xtable(ano1.a2, caption = "Estimaciones de las tasas de mortalidad(x 100.000 hab.) para el año
2009 por Nivel de escolaridad, edad y sexo a partir de los resultados  del 
modelo bayesiano de Regresion Binomial Negativa con información a priori vía Meta-análisis
Intervalos de credibilidad HPD al 95%", digits=2)
#
# tabla AÑO 2010
# Demencia
ano2.sex1<-subset(tab.pred.nb.inf.cm2, Año==2 & Sex==1, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano2.sex2<-subset(tab.pred.nb.inf.cm2, Año==2 & Sex==2, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano2<-cbind(ano2.sex1, ano2.sex2[3:5])
ano2$Edad[ano2$Edad == 1] <- "65-69"
ano2$Edad[ano2$Edad == 2] <- "70-74"
ano2$Edad[ano2$Edad == 3] <- "75-79"
ano2$Edad[ano2$Edad == 4] <- "80-84"
ano2$Edad[ano2$Edad == 5] <- "85+"
#
ano2$Esc[ano2$Esc == 1] <- "0"
ano2$Esc[ano2$Esc == 2] <- "1-3"
ano2$Esc[ano2$Esc == 3] <- "4-7"
ano2$Esc[ano2$Esc == 4] <- "8-11"
ano2$Esc[ano2$Esc == 5] <- "12+"
print(xtable(ano2, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,) 
             por Demencia para el año 2010 por Nivel de escolaridad, edad y sexo a 
             partir de los resultados  del modelo bayesiano de Regresión Binomial 
             Negativa con información a priori vía Meta-análisis. Intervalos de 
             credibilidad HPD al 95%"), caption.placement="top")
# Alzheimer
ano2.a<-ano2[3:8]*.72
ano2.a2<-cbind("Esc"=ano2$Esc, "Edad"=ano2$Edad,ano2.a)
#
print(xtable(ano2.a2, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,)
             por Demencia debido a la enfermedad de Alzheimer para el año 2009 por Nivel
             de escolaridad, edad y sexo a partir de los resultados  del modelo 
             bayesiano de Regresión Binomial Negativa con información a priori vía 
             Meta-análisis. Intervalos de credibilidad HPD al 95%", digits=2), 
      caption.placement="top")
#
# tabla AÑO 2011, año mediano para publicacion
# Demencia
ano3.sex1<-subset(tab.pred.nb.inf.cm2, Año==3 & Sex==1, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano3.sex2<-subset(tab.pred.nb.inf.cm2, Año==3 & Sex==2, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano3<-cbind(ano3.sex1, ano3.sex2[3:5])
ano3$Edad[ano3$Edad == 1] <- "65-69"
ano3$Edad[ano3$Edad == 2] <- "70-74"
ano3$Edad[ano3$Edad == 3] <- "75-79"
ano3$Edad[ano3$Edad == 4] <- "80-84"
ano3$Edad[ano3$Edad == 5] <- "85+"
print(xtable(ano3, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,) 
             por Demencia para el año 2011 por Nivel de escolaridad, edad y sexo a 
             partir de los resultados  del modelo bayesiano de Regresión Binomial 
             Negativa con información a priori vía Meta-análisis. Intervalos de 
             credibilidad HPD al 95%"), caption.placement="top")
# Alzheimer
ano3.a<-ano3[3:8]*.72
ano3.a2<-cbind("Esc"=ano3$Esc, "Edad"=ano3$Edad,ano3.a)
#
print(xtable(ano3.a2, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,)
             por Demencia debido a la enfermedad de Alzheimer para el año 2011 por Nivel
             de escolaridad, edad y sexo a partir de los resultados  del modelo 
             bayesiano de Regresión Binomial Negativa con información a priori vía 
             Meta-análisis. Intervalos de credibilidad HPD al 95%", digits=2), 
      caption.placement="top")
#
# tabla AÑO 2012
# Demencia
ano4.sex1<-subset(tab.pred.nb.inf.cm2, Año==4 & Sex==1, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano4.sex2<-subset(tab.pred.nb.inf.cm2, Año==4 & Sex==2, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano4<-cbind(ano4.sex1, ano4.sex2[3:5])
ano4$Edad[ano4$Edad == 1] <- "65-69"
ano4$Edad[ano4$Edad == 2] <- "70-74"
ano4$Edad[ano4$Edad == 3] <- "75-79"
ano4$Edad[ano4$Edad == 4] <- "80-84"
ano4$Edad[ano4$Edad == 5] <- "85+"
print(xtable(ano4, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,) 
             por Demencia para el año 2012 por Nivel de escolaridad, edad y sexo a 
             partir de los resultados  del modelo bayesiano de Regresión Binomial 
             Negativa con información a priori vía Meta-análisis. Intervalos de 
             credibilidad HPD al 95%"), caption.placement="top")
# Alzheimer
ano4.a<-ano4[3:8]*.72
ano4.a2<-cbind("Esc"=ano4$Esc, "Edad"=ano4$Edad,ano4.a)
#
print(xtable(ano4.a2, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,)
             por Demencia debido a la enfermedad de Alzheimer para el año 2012 por Nivel
             de escolaridad, edad y sexo a partir de los resultados  del modelo 
             bayesiano de Regresión Binomial Negativa con información a priori vía 
             Meta-análisis. Intervalos de credibilidad HPD al 95%", digits=2), 
      caption.placement="top")
#
# tabla AÑO 2013
# Demencia
ano5.sex1<-subset(tab.pred.nb.inf.cm2, Año==5 & Sex==1, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano5.sex2<-subset(tab.pred.nb.inf.cm2, Año==5 & Sex==2, select = c("Esc", "Edad","Median", 
                                                                  "Lower95", "Upper95"))
ano5<-cbind(ano5.sex1, ano5.sex2[3:5])
ano5$Edad[ano5$Edad == 1] <- "65-69"
ano5$Edad[ano5$Edad == 2] <- "70-74"
ano5$Edad[ano5$Edad == 3] <- "75-79"
ano5$Edad[ano5$Edad == 4] <- "80-84"
ano5$Edad[ano5$Edad == 5] <- "85+"
xtable(ano5, caption = "Estimaciones de las tasas de mortalidad(x 100.000 hab.) para el año
2013 por Nivel de escolaridad, edad y sexo a partir de los resultados  del 
modelo bayesiano de Regresion Binomial Negativa con información a priori vía Meta-análisis
Intervalos de credibilidad HPD al 95%", digits=2)
# Alzheimer
ano5.a<-ano5[3:8]*.72
ano5.a2<-cbind("Esc"=ano5$Esc, "Edad"=ano5$Edad,ano5.a)
#
xtable(ano5.a2, caption = "Estimaciones de las tasas de mortalidad(x 100.000 hab.) para el año
2013 por Nivel de escolaridad, edad y sexo a partir de los resultados  del 
modelo bayesiano de Regresion Binomial Negativa con información a priori vía Meta-análisis
Intervalos de credibilidad HPD al 95%", digits=2)

# Año 2013 edad y escolaridad por sexo
# tabla AÑO 2013
# Demencia
ano5e.sex1<-subset(tab.pred.nb.inf.cm2, Año==5 & Sex==1, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano5e.sex2<-subset(tab.pred.nb.inf.cm2, Año==5 & Sex==2, select = c("Edad","Esc","Median", 
                                                                  "Lower95", "Upper95"))
ano5e<-cbind(ano5e.sex1, ano5e.sex2[3:5])
ano5e <- ano5e[order(ano5e$Edad),] 
ano5e$Edad[ano5e$Edad == 1] <- "65-69"
ano5e$Edad[ano5e$Edad == 2] <- "70-74"
ano5e$Edad[ano5e$Edad == 3] <- "75-79"
ano5e$Edad[ano5e$Edad == 4] <- "80-84"
ano5e$Edad[ano5e$Edad == 5] <- "85+"
#
ano5e$Esc[ano5e$Esc == 1] <- "Cero"
ano5e$Esc[ano5e$Esc == 2] <- "1-3"
ano5e$Esc[ano5e$Esc == 3] <- "4-7"
ano5e$Esc[ano5e$Esc == 4] <- "8-11"
ano5e$Esc[ano5e$Esc == 5] <- "12+"
#
xtable(ano5e, caption = "{Estimaciones de las tasas de mortalidad(x 100,000 hab,)
 por todo tipo de {\bf Demencia} para el año {\bf  2013} por grupo de edad, sexo y para cada Nivel de escolaridad a partir de los resultados del modelo 
 bayesiano de Regresión Binomial Negativa con información a priori vía 
 Meta-análisis, Intervalos de credibilidad HPD al 95%", digits=2)
# Alzheimer
ano5e.a<-ano5e[3:8]*.72
ano5e.a2<-cbind("Edad"=ano5e$Edad,"Esc"=ano5e$Esc,ano5e.a)
#
xtable(ano5e.a2, caption = "Estimaciones de las tasas de mortalidad(x 100,000 hab,)
 por todo tipo de {\bf Demencia} para el año {\bf  2013} por grupo de edad, sexo y para cada Nivel de escolaridad a partir de los resultados del modelo 
 bayesiano de Regresión Binomial Negativa con información a priori vía 
 Meta-análisis, Intervalos de credibilidad HPD al 95%", digits=2)



