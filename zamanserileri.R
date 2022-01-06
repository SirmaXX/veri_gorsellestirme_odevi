install.packages("priceR")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("lubridate")
library(ggplot2)
library(priceR)
library(tidyverse)
library(dplyr)
library(reshape)
library(reshape2)
library(lubridate)


## EREGLI HISSE SENEDI VS USDTRY KARSILASTIRMA

edata<-read.csv(file.choose(),header = T,sep=",")
USDTRY <- historical_exchange_rates("USD",
                                    to ="TRY",
                                    start_date="2010-01-01",
                                    end_date = "2020-03-19")
colnames(USDTRY)[2]="usd_to_try"
#haftanýn  günlerini çektik usdtry paritesi için 
fusdtry<-USDTRY%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta günleri içinden pazartesi ve cuma olan çalýþma günlerini çektik 
f2usdtry<-USDTRY%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))
  
# tarih aralýðýný saptadýk
range(edata$Date)
#"2010-01-01" "2020-03-19"

#sütun isimlerini öðrendik
names(edata)
#çýkarmak istediðimiz sütün isimlerini aþaðýda yazarak çýkarttýk
df = subset(edata, select = -c(Adj.Close,High,Low,Open,Volume) )

#istenmeyen sütünlardan temizlenmiþ verisetini erdata deðiþkenine atadýk.


erdata<-df

#Date formatýna çevirme 
erdata<-erdata%>%
  mutate(Date=as.Date(erdata$Date))
typeof(erdata$Date)

#left join iþlemi ile aralarýnda bulunan tarih farklarýný eledik ve zaman aralýklarý eþitlendi


matching<-left_join(f2usdtry,erdata, by = c("date" = "Date"))
names(matching)



matching2= subset(matching, select = -c(weekday) )
colnames(matching2)<- c("date","usd_try","eregli")
names(matching2)



melted_matchings2<-melt(matching2,id=c("date"))

ggplot(melted_matchings2,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý Ve Eregli Demir Çelik Fiyat Deðiþimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlýklar",
                     labels=c("USDTRY","EREGLI"), 
                     values=c("green4", "firebrick1","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend çizgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari düzenledik


## Yanlýþ Görselleþtirme türü y 0 dan baþlamýyor

ggplot(melted_matchings2,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý Ve Eregli Demir Çelik Hisse  
                         Fiyatý  Deðiþimi",x = "Yýllar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlýklar",
                     labels=c("USDTRY","EREGLI"), 
                     values=c("green4", "firebrick1","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T)+ # grafige trend çizgisi yerlestirdik
  expand_limits(y=c(10,15)) 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------







# ISDEMIR USDTRY PARÝTESÝ  FÝYAT KARÞILAÞTIRMASI



isdmr<-read.csv("D:\\Yedek\\VG PROJE\\ISDMR.csv")

range(isdmr$Date)
#"2016-03-28" "2020-03-19"

USDTRY2 <- historical_exchange_rates("USD",
                                    to ="TRY",
                                    start_date="2016-03-28",
                                    end_date = "2020-03-19")

colnames(USDTRY2)[2]="usd_to_try"


#haftanýn  günlerini çektik usdtry paritesi için 
utry<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta günleri içinden pazartesi ve cuma olan çalýþma günlerini çektik 
u2try<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#sütun isimlerini öðrendik
names(isdmr)
#çýkarmak istediðimiz sütün isimlerini aþaðýda yazarak çýkarttýk
#istenmeyen sütünlardan temizlenmiþ verisetini isdmr2 deðiþkenine atadýk.
isdmr2= subset(isdmr, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date formatýna çevirme 
isdmr2<-isdmr2%>%
  mutate(Date=as.Date(isdmr2$Date))
typeof(isdmr2$Date)

#left join iþlemi ile aralarýnda bulunan tarih farklarýný eledik ve zaman aralýklarý eþitlendi


isdmrmatch<-left_join(u2try,isdmr2, by = c("date" = "Date"))
names(isdmrmatch)



isdmrmatch2= subset(isdmrmatch, select = -c(weekday) )
colnames(isdmrmatch2)<- c("date","usd_try","isdmr")
names(isdmrmatch2)



melted_matchings3<-melt(isdmrmatch2,id=c("date"))




# Zaman serisi Doðru görselleþtirme
ggplot(melted_matchings3,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý Ve Iskenderun Demir Çelik Hisse Fiyatý Deðiþimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlýklar",
                     labels=c("USDTRY","ISDMR"), 
                     values=c("green4","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend çizgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari düzenledik





#Zaman Serisi Yanlýþ görselleþtirme

ggplot(melted_matchings3,aes(x=date,y=value)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý Ve Iskenderun Demir Çelik Fiyat Deðiþimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  expand_limits(y=c(8,15))  # y ekseninin eksende yer alan sayilari düzenledik











#---------------------------------------------------------------------------------------------------





# SISECAM VS USDTRY PARÝTESÝ KARSILASTIRMA


sise<-read.csv("D:\\Yedek\\VG PROJE\\SISE.csv")

range(sise$Date)
#"2016-03-28" "2020-03-19"

USDTRY3 <- historical_exchange_rates("USD",
                                     to ="TRY",
                                     start_date="2010-01-01",
                                     end_date = "2020-03-19")

colnames(USDTRY3)[2]="usd_to_try"


#haftanýn  günlerini çektik usdtry paritesi için 
stry-USDTRY3%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta günleri içinden pazartesi ve cuma olan çalýþma günlerini çektik 
s2try<-USDTRY3%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#sütun isimlerini öðrendik
names(sise)
#çýkarmak istediðimiz sütün isimlerini aþaðýda yazarak çýkarttýk
#istenmeyen sütünlardan temizlenmiþ verisetini sise2 deðiþkenine atadýk.
sise2= subset(sise, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date formatýna çevirme 
sise2<-sise2%>%
  mutate(Date=as.Date(sise2$Date))
typeof(sise2$Date)

#left join iþlemi ile aralarýnda bulunan tarih farklarýný eledik ve zaman aralýklarý eþitlendi


sisematch<-left_join(s2try,sise2, by = c("date" = "Date"))
names(sisematch)



sisematch2= subset(sisematch, select = -c(weekday) )
colnames(sisematch2)<- c("date","usd_try","sise")
names(sisematch2)



melted_matchings4<-melt(sisematch2,id=c("date"))




# Zaman serisi Doðru görselleþtirme
ggplot(melted_matchings4,aes(x=date,y=value,color=variable)) +
  geom_line(alpha=0.8,position = "dodge") + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý Ve ÞiþeCam Hisse Senedi Fiyat Deðiþimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlýklar",
                     labels=c("USDTRY","SISE"), 
                     values=c("green4","black")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend çizgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari düzenledik

#-----------------------------------------------------------------------------------------------------------------------------------------------






# AKSA VS USDTRY PARÝTESÝ




aksa<-read.csv("D:\\Yedek\\VG PROJE\\AKSA.csv")

range(aksa$Date)
#"2010-01-01" "2020-03-19"

USDTRY4 <- historical_exchange_rates("USD",
                                     to ="TRY",
                                     start_date="2010-01-01",
                                     end_date = "2020-03-19")

colnames(USDTRY2)[2]="usd_to_try"


#haftanýn  günlerini çektik usdtry paritesi için 
utry4<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta günleri içinden pazartesi ve cuma olan çalýþma günlerini çektik 
utry4<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#sütun isimlerini öðrendik
names(aksa)
#çýkarmak istediðimiz sütün isimlerini aþaðýda yazarak çýkarttýk
#istenmeyen sütünlardan temizlenmiþ verisetini isdmr2 deðiþkenine atadýk.
aksa2= subset(aksa, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date formatýna çevirme 
aksa2<-aksa2%>%
  mutate(Date=as.Date(aksa2$Date))
typeof(aksa2$Date)

#left join iþlemi ile aralarýnda bulunan tarih farklarýný eledik ve zaman aralýklarý eþitlendi


aksamatch<-left_join(utry4,aksa2, by = c("date" = "Date"))
names(aksamatch)



aksamatch= subset(aksamatch, select = -c(weekday) )
colnames(aksamatch)<- c("date","usd_try","isdmr")
names(aksamatch)



melted_matchings4<-melt(aksamatch,id=c("date"))




# Zaman serisi Doðru görselleþtirme
ggplot(melted_matchings4,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yýllara Göre Dolar Türk Lirasý ve Aksa Akrilik Hisse Fiyatý Deðiþimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlýklar",
                     labels=c("USD/TRY","AKSA"), 
                     values=c("green4","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T,size=0.4)+ # grafige trend çizgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari düzenledik



