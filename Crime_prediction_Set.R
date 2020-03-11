setwd("F://Rsoftware/DataSet's/Crime/SET")
getwd()
library(ggplot2)
library(dplyr)
library(caret)
library(statsr)
library(gridExtra)
library(GGally)
library(ggthemes)
library(lattice)
library(plotly)

crimeData<-read.csv("01_District_wise_crimes_committed_IPC_2001_2012.csv")
crimeData

max_rape<-max(crimeData$RAPE)
max_murder<-max(crimeData$MURDER)
plot(crimeData$RAPE,crimeData$MURDER,type = "p",xlim = c(0,max_rape),ylim = c(0,max_murder),main = "MURDER vs RAPE",xlab = "Rape",ylab = "Murder",col=c("red","blue"))

counts<-table(crimeData$YEAR)
barplot(counts,main = "Crime per Year",xlab = "Years",ylab = "Count",col = c("red","green","blue","yellow","black","orange","violet","skyblue"))

sum_rape<-sum(crimeData$RAPE)
sum_murder<-sum(crimeData$MURDER)
sum_KABOWC<-sum(crimeData$KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS)
sum_dacoity<-sum(crimeData$DACOITY)
sum_CBHAHR<-sum(crimeData$CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES)
sum_robbery<-sum(crimeData$ROBBERY)
sum_KAA<-sum(crimeData$KIDNAPPING...ABDUCTION)
sum_roits<-sum(crimeData$RIOTS)
slice<-c(sum_rape,sum_murder,sum_KABOWC,sum_dacoity,sum_CBHAHR,sum_robbery,sum_KAA,sum_roits)
lbls<-c("RAPE","MURDER","KABOWC","DACOITY","CBHAHR","ROBBERY","KAA","ROITS")
pie(slice,labels = lbls,col = rainbow(length(lbls)),main = "Pie Chart of different crimes during 2001-2012")

p_genreimdb <- ggplot(crimeData, aes(x=factor(STATE.UT), y=ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY)) +
geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("State wise crime on women") + geom_hline(yintercept =median(crimeData$ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY, na.rm = TRUE), col = "royalblue",lwd = 1)
p_genreimdb <- ggplot(crimeData, aes(x=factor(STATE.UT), y=RAPE)) +
geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("State wise rape data") + geom_hline(yintercept =median(crimeData$RAPE, na.rm = TRUE), col = "royalblue",lwd = 1)

plot(crimeData$MURDER,crimeData$ATTEMPT.TO.MURDER, main="Murder vs Attempt to murder", xlab = "MURDER", ylab="ATTEMPT TO MURDER",col=c("red","blue"))
abline(lm(crimeData$ATTEMPT.TO.MURDER~crimeData$MURDER),col = "royalblue",lwd = 1)

d1 <- ggplot(data = crimeData, aes(y = MURDER, x = ATTEMPT.TO.MURDER,colour=COUNTERFIETING )) + geom_point()
d2 <- ggplot(data = crimeData, aes(y = RAPE, x = CUSTODIAL.RAPE, colour=COUNTERFIETING)) + geom_point()
grid.arrange(d1, d2, nrow = 1, ncol = 2)

d3<-ggplot(data = crimeData,aes(y=KIDNAPPING...ABDUCTION,x=KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS,colour=COUNTERFIETING))+geom_point()
d4<-ggplot(data = crimeData,aes(y=DACOITY,x=PREPARATION.AND.ASSEMBLY.FOR.DACOITY,colour=COUNTERFIETING))+geom_point()
grid.arrange(d3,d4, nrow = 1, ncol = 2)

cloud(RAPE~DOWRY.DEATHS * INSULT.TO.MODESTY.OF.WOMEN,data=crimeData,col=c("red","blue","green"),pch=17,main="3D Scatter Plot to show the occurance of crime against women")

p_genreimdb <- ggplot(crimeData, aes(x=factor(YEAR), y=TOTAL.IPC.CRIMES)) +
geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("Year wise total crimes") + geom_hline(yintercept =median(crimeData$TOTAL.IPC.CRIMES, na.rm = TRUE), col = "royalblue",lwd = 1)

bar1<-crimeData
bar1<-bar1[bar1$YEAR==2007,]
bar1<-bar1[bar1$STATE.UT=="WEST BENGAL",]
bar1<-bar1[bar1$DISTRICT!="",]
dat<-data.frame(DICTRICT=bar1$DISTRICT,values=as.numeric(bar1$THEFT))
ggplot(data=dat,aes(x=DICTRICT,y=bar1$THEFT))+geom_bar(stat = "identity")+ggtitle("ANDHRA PRADESH 2010 MURDER DATA ")

model <- lm(MURDER ~ RAPE, data=crimeData) #funny thing, if you will not use "data=" you will not be able to get predict() to work properly
par(mfrow=c(2,2)) #combine plots to 2x2 table
hist(model$residuals, main="residuals")
qqnorm(model$residuals)
qqline(model$residuals)
plot(model$residuals ~ model$fitted)
summary(model)

#Prediction-1 
crime1 <- data.frame(title ="MURDER", RAPE = 50)
predict(model, crime1, interval = "prediction", level = 0.95)
