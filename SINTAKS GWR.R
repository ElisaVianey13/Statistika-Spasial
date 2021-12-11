DATA2=read.table(file.choose(),header=TRUE)
summary(DATA2)
a<-lm(formula=Y~X1+X2+X3+X4+X5+X6,data=DATA2)
summary(a)
## Uji F (Melihat signifikansi keseluruhan model)
anova(a)

#confident Interval (Uji Parsial)
confint.lm(a, level=0.95)
prediksi<-predict(a)
prediksi

##uji asumsi klasik model OLS
#uji multikolinearitas
library(car)
vif(a)

#uji normalitas residual
resid<-abs(a$residuals)
res=a$residual
ks.test(res,"pnorm",mean(res),sd(res),alternative=c("two.sided"))

#uji heterogenitas spasial
library(lmtest)
bptest(lm(a$residuals~X1+X2+X3+X4+X5+X6, data=DATA2))

#uji otokorelasi spasial
dwtest(lm(a$residuals~X1+X2+X3+X4+X5+X6, data=DATA2))

#Mencari bandwidth optimal (adaptive bandwidth)
library(spgwr)
b <- gwr.sel(Y~X1+X2+X3+X4+X5+X6,coords=cbind(DATA2$U,DATA2$V),data=DATA2, adapt=TRUE,gweight=gwr.Gauss)
b
#Estimasi Parameter
gwr1 <- gwr(Y~X1+X2+X3+X4+X5+X6,coords=cbind(DATA2$U,DATA2$V),data=DATA2, adapt=b,hatmatrix=TRUE,gweight=gwr.Gauss)
gwr1
anova(gwr1)

#Menampilkan t hitung
t_X1=gwr1$SDF$X1/gwr1$SDF$X1_se
t_X1
t_X2=gwr1$SDF$X2/gwr1$SDF$X2_se
t_X2
t_X3=gwr1$SDF$X3/gwr1$SDF$X3_se
t_X3
t_X4=gwr1$SDF$X4/gwr1$SDF$X4_se
t_X4
t_X5=gwr1$SDF$X5/gwr1$SDF$X5_se
t_X5
t_X6=gwr1$SDF$X6/gwr1$SDF$X6_se
t_X6

#Membaca Output
gwr1$SDF$"(Intercept)"
gwr1$SDF$X1
gwr1$SDF$X2
gwr1$SDF$X3
gwr1$SDF$X4
gwr1$SDF$X5
gwr1$SDF$X6

#Uji Kecocokan Model
BFC02.gwr.test(gwr1)

#menampilkan r-square lokal
gwr1.R2=gwr1$SDF$localR2
gwr1.R2

#Evaluasi hasil prediksi dan data observasi menggunakan grafik
require (ggplot2)
plot(DATA2$Y, type="l", col="black")
lines(gwr1$SDF$pred, type="l", col="red")
lines(prediksi, type="l", col="blue") 
legend("topright",c("Observasi","Prediksi OLS","Prediksi GWR"),
col=c("black","blue","red"), lwd=3)

##Perbandingan Korelasi Antar Prediksi dengan Observasi
obs<-DATA2$Y
gwr_pred<-gwr1$SDF$pred
gwr_pred
cor(prediksi,obs)^2
cor(gwr_pred,obs)^2
