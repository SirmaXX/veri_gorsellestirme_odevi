library(corrplot)
library(tseries)
library(skedastic)
library(lmtest)
library(caret)
library(ggplot2)
library(corrplot)
library(GGally)
library(gapminder)

#verisetini tanıtalım.
file<-"/home/deniz/Masaüstü/veri_gorsellestirme_odevi/datasets/lung_cancer_examples.csv"
data<-read.csv(file)
str(data)


lungdata<-data.frame(data[,c(3:7)])
str(lungdata)

x <- lungdata[,c(1:4)]
y <- lungdata[,5]
str(x)
str(y)

#correlation plot
correlations <- cor(x)
corrplot(correlations, method="square")

#pairs
pairs(lungdata, col=y)


#koleogram
ggpairs(lungdata, aes(colour = unique(Result ), alpha = 0.4))
ggcorr(lungdata[, c(1:4)]) +labs(title="Bütün değişkenlerin bulunduğu Koreologram")


#logistic regression

glm.fit <- glm(Result ~ Age  + Smokes+ AreaQ +Alkhol, data =lungdata, family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.pred <- ifelse(glm.probs < 0.5, 0, 1)
mean(glm.pred == y)


#multiple linear regression

lung.lm<-lm(Result ~ Age  + Smokes+ AreaQ +Alkhol, data =lungdata)
summary(lung.lm)

resid1<-resid(lung.lm)
res<-resid(glm.fit)

plot(y , res)
abline(0,0)
sum(res)


#artıkların histogramı
hist(res)

#normallik testi için kolmogrov-smirnov testi
ks.test(res, "pnorm", mean=mean(res), sd=sd(res))


jarque.bera.test(res)
# değişen varyans testleri


#breusch_pagan testi
breusch_pagan(lung.lm)

#white test
white_lm(lung.lm)

#goldfeld_quandt testi
goldfeld_quandt(lung.lm)

#bartlett testi
bartlett.test(list(y,x))


#https://www.tutorialspoint.com/r/r_logistic_regression.htm
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://www.scribbr.com/statistics/linear-regression-in-r/




