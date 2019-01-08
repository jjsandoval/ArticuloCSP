betas<- read.table(text="
b0      -8.840220e+00   -8.66408665   -8.4928069
SEX[2]  -2.919895e-03    0.09948202    0.1851545
ED[2]    6.908776e-01    0.83962143    0.9822860
ED[3]    1.841307e+00    1.98997811    2.1400748
ED[4]    2.733541e+00    2.88787647    3.0335919
ED[5]    4.013697e+00    4.16880970    4.3376676
ES[1]    4.598468e-01    0.58789539    0.7203761
ES[2]    3.597355e-02    0.17333091    0.3100446
ES[3]    2.346944e-01    0.33974565    0.4485562
Ano[2]  -8.902703e-02    0.07749105    0.2415497
Ano[3]  -1.141380e-01    0.05069761    0.2159275
Ano[4]  -1.077047e-01    0.05651019    0.2261933
Ano[5]  -9.060010e-02    0.07013935    0.2357004
", header=F)
colnames(betas)<-c("Parámetros","ICR95%li","Beta","ICR95%ls")
lbetas<-exp(betas[2:4])
colnames(lbetas)<-c("ICr95%li", "RTM", "ICr95%ls")
lbetas$beta<-betas$Beta
lbetas$parametros<-betas$Parámetros
lbetas<-lbetas[c(5,4,2,1,3)]
lbetas[1,3:5]<-lbetas[1,3:5]*100000 # tasas de beta0
#install.packages("xtable")
require(xtable)
tabpar<-xtable(lbetas, caption = "Estimaciones de los parámetros del modelo de regresión binomial negativo
bayesiano y razones de tasas de mortalidad con intervalos de credibilidad del 95%", digits=3)
print(tabpar, caption.placement = 'top', floating=F, NA.string = "NA")

#################### RIESGO ATRIBUIBLE POBLACIONAL ###########################3
# TODAS LAS DEMENCIAS
Pe<-0.0799
betas2<-exp(betas[2:13,2:4])
#betas2[1,]<-1/betas2[1,c(3,2,1)]
#betas2[8,]<-1/betas2[8,c(3,2,1)]
betas2<-betas2[c(2,1,3)]
RAPi<-(Pe*(betas2$`ICR95%li`-1))/(1+Pe*(betas2$`ICR95%li`-1))*100
RAPm<-(Pe*(betas2$Beta-1))/(1+Pe*(betas2$Beta-1))*100
RAPs<-(Pe*(betas2$`ICR95%ls`-1))/(1+Pe*(betas2$`ICR95%ls`-1))*100
# RaPs
params<-c("Sexo(H)", "70-74 años", "75-79 años", "80-84 años", "85+ años",
          "0-3 años", "4-7 años", "8-11 años", "2010", "2011", "2012", "2013")
t.Rap<-cbind(params, betas2, RAPm, RAPi, RAPs)
#
require(xtable)
t.Rap2<-xtable(t.Rap, caption = "Estimaciones del porcentaje de riesgo atribuible 
poblacional (RaP) desde las razones de tasas de mortalidad (RTM). Modelo bayesiano 
               de regresión binomial negativa con información a priori vía 
               meta-análisis. Brasil 2009--2013.", digits=2,
               table.placement ="")
print(t.Rap2, caption.placement = "top")

# DEMENCIA DEBIDO AL ALZHEIMER
Pe2<-0.057528 # prevalencia de Alzheimer de acuerdo a p1=0.0799 y p2=0.72 
RAPia<-(Pe2*(betas2$`ICR95%li`-1))/(1+Pe2*(betas2$`ICR95%li`-1))*100
RAPma<-(Pe2*(betas2$Beta-1))/(1+Pe2*(betas2$Beta-1))*100
RAPsa<-(Pe2*(betas2$`ICR95%ls`-1))/(1+Pe2*(betas2$`ICR95%ls`-1))*100
# RaPs
t.Rap.a<-cbind(t.Rap, RAPma, RAPia, RAPsa)
t.Rap.a<-t.Rap.a[c(1,2,5:10)]
#
require(xtable)
t.Rap2a<-xtable(t.Rap.a, caption = "Estimaciones del porcentaje de riesgo atribuible 
poblacional (RaP) desde las razones de tasas de mortalidad (RTM) para la Demencia y 
Demencia debido a la enfermeadad de Alzheimer. Modelo bayesiano de regresión binomial 
negativa con información a priori vía meta-análisis. Brasil 2009--2013.", digits=2,
               table.placement ="")
print(t.Rap2a, caption.placement = "top")

