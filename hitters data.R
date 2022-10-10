                                  ### DSM-5007 ARASİNAV ###
library(ISLR)
library(psych)
library(corrplot)
library(olsrr)
library(Metrics)
library(ModelMetrics)
library(truncgof)
library(nortest)
library(lmtest)
library("caret")
library("ModelMetrics")
library("dplyr")
library("car")
Hitters<-data.frame(Hitters)
Hitters<-Hitters[-c(1,2,3,4,5,9,10,14,17,18)]
Hitters=na.omit(Hitters) #Kayip gozlemler temizlendi

??Hitters
sapply(Hitters,class) #2 kategorik bağimsiz degisken 7 numerik bagimsiz degisken

Hitters<-data.matrix(Hitters)#Kategorik degiskenlere numerik degerler atanmıstir. (NewLeague_A<-1 N<-2   Divison_E<-1, W<-2)
Hitters<-data.frame(Hitters)
head(Hitters)


###Test-Train###
smp_size <- floor(0.70 * nrow(Hitters)) # Verilerin 0.7'si egitim 0.3'u test verisi olarak ayrıldı
set.seed(2021900143) 
train_ind <- sample(nrow(Hitters), size = smp_size, replace = FALSE)
train1 <- Hitters[train_ind, ]
test <- Hitters[-train_ind, ]

#Descriptive Statistics

pairs.panels(train)

pairs(train, pch = 19, col='red', lower.panel = NULL) 
corrplot(cor(train))

hist(train$Salary, freq = F)
lines(density(train$Salary),col="red",lwd=2)

hist(train$CWalks, freq = F)
lines(density(train$CWalks),col="red",lwd=2)

hist(train$CAtBat, freq = F)
lines(density(train$CAtBat),col="red",lwd=2)

hist(train$CRuns,freq=F)
lines(density(train$CRuns),col="red",lwd=2)

hist(train$CRBI,freq=F)
lines(density(train$CRBI),col="red",lwd=2)

#Numeric olarak algılanan kategorik degiskenler tekrar factor tipine cevrilmistir.
Hitters$NewLeague<-as.factor(Hitters$NewLeague)
Hitters$Division<-as.factor(Hitters$Division)
summary(Hitters)

 
 ### Coklu Regresyon Modeli ###

model<- lm(Salary~Walks+Years+CAtBat+CRuns+CRBI+CWalks+Division+PutOuts+NewLeague,data=train1)
summary(model) 
#H0:ßi=0 (lineer ilişki yoktur.)
#H1:ßi=!0 (en az bir (ßi) katsayı anlamlıdır. Bagimli degiskeni etkiler.)
#İntercept,Years,CatBat, CRBI ve NewLeague katsyilari anlamsiz cıkmistir.  
#R^2 ve adj. R^2 degerleri birbirine yakin.

qf(0.95,9,174) #F degeri tablo degerinden buyuk ve p degeri 0.05'ten cok kucuk oldugu icin H0 reddedilir.
                #En az bir ßi anlamlidir.Model gecerlidir.
qt(0.975,174)

## Multicollinearity ##

vif(model_full) #CatBat, CRuns, degiskenlerinin Vif degerleri yuksek oldugu 
#icin degiskenler arasında coklu dogrusal baglantı sorunu oldugunu soyleyebiliriz (Kritik vif degeri 10 alinmistir)

## Full Modelin Hata ve Varyans Grafikleri
par(mfrow=c(2,2))
plot(model_full)

#ALTERNATIF MODELLER

a1<-ols_step_all_possible(model_full)
plot(a1)
a1[c(466,502,382,383,256),]

a2<-ols_step_both_p(model_full,pent = 0.05,prem=0.1)
plot(a2) #5değişkenli model
a2

a3<-ols_step_backward_p(model_full) #modelden cikanlari soyler
a3

a4<-ols_step_forward_p(model_full) 
a4

#Uygulanan algoritmalar sonucu secilen 256, 382, 383,466 ve 502. modellerin test verisi uzerindeki sonuclarına bakilmistir. 
 

#3 degisken modelden cikarildi 382.model
model_1<-lm(Salary~Walks+CRuns+CRBI+CWalks+Division+PutOuts,data=train)
summary(model_1) #intercept ve CRBI anlamsiz
qf(.95,6,177) #p<0.05 ve  Ftablo<F degeri  Model gecerli.


#2 degisken modelden cikarildi 466.model
model_2<-lm(Salary~Walks+Years+CRuns+CRBI+CWalks +Division+PutOuts,data=train)
summary(model_2) ##intercept, Years ve CRBI anlamsiz
qf(.95,7,176) # Model gecerli

 #502. model 1 degisken cıkarildi
model_3<-lm(Salary~Walks+Years+CAtBat+CRuns+CRBI+CWalks+Division+PutOuts,data=train)
summary(model_3) #intercept, CatBat, CRBI ve Years katsayilari anlamsiz 
qf(0.95,8,175) # Model gecerli


#256.model
model_4<-lm(Salary~Walks+CRuns+CWalks+Division+PutOuts,data=train)
summary(model_4)#butun katsayilar anlamli

#383.model
model_5<-lm(Salary~Walks+Years+CRuns+CWalks+Division+PutOuts,data=train)
summary(model_5)#2jatsayi anlamsiz
#Model karsilastirmasi icin modellerin test verisindeki performanslarına bakilir

predictions1=predict(model_1,test)
predictions2=predict(model_2,test)
predictions3=predict(model_3,test)
predictions4=predict(model_4,test)
predictions5=predict(model_5,test)

RMSE1 = RMSE(predictions1, test$Salary) 
RMSE2 = RMSE(predictions2, test$Salary)
RMSE3 = RMSE(predictions3, test$Salary)#en kucuk deger
RMSE4=RMSE(predictions4,test$Salary)
RMSE5=RMSE(predictions5,test$Salary)
cbind(RMSE1,RMSE2,RMSE3,RMSE4,RMSE5)


#mean absolute error hesaplamak için
install.packages("ModelMetrics") 
library("ModelMetrics")
mae1=mae(predictions1, test$Salary)#En kucuk deger
mae2=mae(predictions2, test$Salary) 
mae3=mae(predictions3, test$Salary)
mae4=mae(predictions4,test$Salary)
mae5=mae(predictions5,test$Salary)

cbind(mae1,mae2,mae3,mae4,mae5)

#model_1 olası en iyi model secilmistir. Model_3 de çoklu bağlantı sorununa sebep olan CAtBat değişkeni vardır

summary(model_1)
vif(model_1) #CRuns 


#Modelin iyileşmesine yonelik calismalar

par(mfrow=c(2,2))
plot(model_1)
hist(model_1$residuals)

#Modelin hata dagilimi egrisel oldugu icin polinom regresyon modeli dusunulur.Modele karesel bir degiken eklenir.


model_1.q<-lm(Salary~Walks+CRuns+I(CRuns^2)
            +CRBI+CWalks+Division+PutOuts,data=train)
summary(model_1.q)#butun katsayilar anlamli
vif(model_1.q)

model_1.q_CWalks<-lm(Salary~Walks+CRuns+I(CWalks^2)+
                   +CRBI+CWalks+Division+PutOuts,data=train)
summary(model_1.q_CWalks)
vif(model_1.q_CWalks) #vif degerleri buyuk 

model_1.q_CRBI<-lm(Salary~Walks+CRuns+I(CRBI^2)
                   +CRBI+CWalks+Division+PutOuts,data=train)
summary(model_1.q_CRBI)#butun katsayilar anlamli
vif(model_1.q_CRBI)

par(mfrow=c(2,2))
plot(model_1.q) #CRuns^2 eklendi

par(mfrow=c(1,2))
hist(model_1$residuals,freq=F)
lines(density(model_1.q$residuals),col="red")
hist(model_1.q$residuals,freq=F)
lines(density(model_1.q$residuals),col="red")

par(mfrow=c(1,2))
plot(model_1.q$fitted.values,model_1.q$residuals)
abline(h=0,col="red")

## Normallik Testleri

v.test(model_1.q$residuals, "pnorm",fit=list(mean=0,sd=sd(model_1.q$residuals)))

cvm.test(model_1.q$residuals)
ks.test(model_1.q$residuals, "pnorm",fit=list(mean=0,sd=sd(model_1.q$residuals)))
shapiro.test(model_1.q$residuals)

#Ho hipotezi reddedildi dagilim normal degil

# Breusch-Pagan Test####

bptest(model_1.q) #varyans sabit değil h0 reddedildi

#Aykiri ve etkin gozlemler


hat <- hatvalues(model_1.q)
res<- model_1.q$residuals

length(which(hat>2*mean(hat)))  #17 kaldirac noktasi

mse=sum(res^2)/model_1.q$df

st.res=res/(mse^0.5)
stud.res=res/((mse*(1-hat))^0.5)

sum(st.res-stud.res)

plot(hat,st.res,xlab = "Hat Values", ylab = "Standardized Residuals")
abline(h=c(-3,3),v=2*mean(hat))


hatvalues(model_1.q)
hatvalues(model_1.q)>2*mean(hatvalues(model_1.q))
which(hatvalues(model_1.q)>2*mean(hatvalues(model_1.q))) #17 kaldirac degeri


par(mfrow=c(2,2))
plot(model_1.q)

par(mfrow=c(1,1))
st.res=model_1.q$residuals/sd(model_1.q$residuals)
plot(hatvalues(model_1.q),st.res)
abline(h=c(-2,2),v=2*mean(hatvalues(model_1.q)))
identify(hatvalues(model_1.q),st.res) 

train[c(which(st.res<(-3)),
       which(st.res>(3))),]

c(which(st.res<(-3)),which(st.res>(3)))
length(c(which(st.res<(-3)),which(st.res>(3)))) #aykiri gozlem 4 tane

ols_plot_cooksd_chart(model_1.q)
summary(model_1.q)
clean_train<-train[-c(14,50,54,166),]
summary(model_1.q)
model.q_cl<-lm(Salary ~ Walks + CRuns + I(CRuns^2) + CRBI + CWalks + Division+
                PutOuts, data =clean_train)

summary(model.q_cl)
vif(model.q_cl)


library(truncgof)
v.test(model.q_cl$residuals, "pnorm",fit=list(mean=0,sd=sd(model.q_cl$residuals)))
shapiro.test(model.q_cl$residuals) 
cvm.test(model.q_cl$residuals)



par(mfrow=c(2,2))
plot(model3_cl.q)
bptest(model.q_cl) #Varyanslar sabit. ho reddedildi



model.q_cl1<-lm(sqrt(Salary) ~ Walks + CRuns + I(CRuns^2) + CRBI + CWalks + Division+
                 PutOuts, data =train)
summary(model.q_cl1)
bptest(model.q_cl1)



#yeni gozlem degeri
b<-data.frame(Walks=c(1.4343e+00,2,15),CRuns=c(0.332,1,5),CRBI=c(4.343422e-02,0.01,1),CWalks=c(-15,-2,2)
              ,Division=c(-150,5,30),PutOuts=c(0.1,0.3,0.7))
predict(model.q_cl,newdata = b)
predictnewconf=predict(model.q_cl,b,interval="confidence")
predictnewpred=predict(model.q_cl,b,interval="prediction")
predicted_data4 <- data.frame(b,predictnewconf,predictnewpred)
 library(rmarkdown)

