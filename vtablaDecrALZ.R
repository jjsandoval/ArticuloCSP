# Females 2010, i cause = Demencia by all causes combinates (corrigido x queiroz,2012)
t<- read.table(text="
i  x nDxi nDx nNx
1 65 181  10711 660091
2 70 344  13860 519301
3 75 819  16101 387509
4 80 1458 18621 274664
5 85 4071 32786 218282", header = T)
#
# TABLA DE VIDA MASTER
# n
n<-c()
for (i in 1:nrow(t)){
n[i]<-t$x[i+1]-t$x[i]
}
# nmx
t$nmx<-round(t$nDx/t$nNx,5)
# nax
t$nax<-round(c(rep(2.5, max(t$i)-1), 6.539),2)
# nqx
t$nqx<-(n*t$nmx)/(1+(n-t$nax)*t$nmx)
t$nqx[max(t$i)]<-1
t$nqx<-round(t$nqx,5)
# npx
t$npx<-1-t$nqx # Pr. sobreviver entre x e x+n
# lx
t$lx<-c(100000, rep(NA,max(t$i)-1))
for(i in 2:nrow(t)){
   t$lx[i]<-t$lx[i-1]*t$npx[i-1]
}
t$lx<-round(t$lx,0)
# ndx
t$ndx<-c()
for(i in 1: nrow(t)){
  t$ndx[i]<-t$lx[i]-t$lx[i+1]
}
t$ndx[max(t$i)]<-round(t$lx[max(t$i)])
# nLx
t$nLx<-c()
for (i in 1:nrow(t)){
  t$nLx[i]<-n[i]*t$lx[i+1]+t$nax[i]*t$ndx[i]
}
t$nLx[max(t$i)]<-round(t$lx[max(t$i)]/t$nmx[max(t$i)])
t$nLx<-round(t$nLx)
# Tx
t$Tx<-round(rev(cumsum(rev(t$nLx))))
# ex(0)
t$ex<-round(t$Tx/t$lx,3)
t
#
#########################... PROCESSO DE DECREMENTEOS MULTIPLOS.... ###########################
#######################....PRIMERA PARTE DE LA TABLA DE DECREMENTOS....########################
# pagina 77 Preston
#nmxi
# nmxi
nmxi<-round(t$nDxi/t$nNx,5)
# nqxi
t$nqxi<-round(t$nqx*(t$nDxi/t$nDx),5)
# ndxi
t$ndxi<-round(t$nqxi*t$lx)
#lxi
t$lxi<-round(rev(cumsum(rev(t$ndxi))))
# i: muertes por neoplasmas
tabla1<-cbind("Edad"=t$x, "nDx"=t$nDx, "nDx^i"=t$nDxi, "lx"=t$lx, "nqx"=t$nqx,
              "nqx^i"=t$nqxi, "ndx^i"=t$ndxi,"lx^i"=t$lxi) 
tabla1
#######################....SEGUNDA PARTE DE LA TABLA DE DECREMENTOS....########################
# R^-1
t$Rmi<-round((t$nDx-t$nDxi)/t$nDx,5)
# npxi^-1
t$npxim<-round((t$npx)^(t$Rmi),5)
#
# lx^-1
t$lxim<-c(100000, rep(NA,max(t$i)-1))
for(i in 2:nrow(t)){
  t$lxim[i]<-t$lxim[i-1]*t$npxim[i-1]
}
t$lxim<-round(t$lxim,0)
#
# naxi-1
t$naxim<-rep(2.5,max(t$i))
for (i in 2:nrow(t)){
  t$naxim[i]<-((-5/24)*t$ndxi[i-1] + 2.5*t$ndxi[i] + (5/24)*t$ndxi[i+1])/t$ndxi[i]
}
#t$naxim[min(t$i)]<-0.045+2.684*t$nmx[1]
#t$naxim[min(t$i)+1]<-1.651-2.816*t$nmx[1]
#t$naxim[min(t$i)+2]<-2.5
t$naxim[max(t$i)]<-t$ex[max(t$i)]/t$Rmi[max(t$i)]
#
# nLxim
t$nLxim<-c()
for (i in 1:nrow(t)){
  t$nLxim[i]<-n[i]*t$lxim[i+1]+t$naxim[i]*(t$ndx[i]-t$ndxi[i])
}
t$nLxim[max(t$i)]<-t$lxim[max(t$i)]/(t$nmx[max(t$i)]-nmxi[max(t$i)])
t$nLxim<-round(t$nLxim)
# Txim
t$Txim<-round(rev(cumsum(rev(t$nLxim))))
# exi(0)
t$exim<-round(t$Txim/t$lxim,3)
#
# -i: sin muertes por neoplasmas
tabla2<-cbind("Edad"=t$x,"R^-1"=t$Rmi, "lx"=t$lx, "npx"=t$npx, "nax"=t$nax, "ex0"=t$ex,"npx^-1"=t$npxim,
              "lx^-1"=t$lxim, "nax^-1"=t$naxim, "ex^-1"=t$exim) 
tabla2

