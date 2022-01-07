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
#haftanın  günlerini çektik usdtry paritesi için 
fusdtry<-USDTRY%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta günleri içinden pazartesi ve cuma olan çalışma günlerini .ektik 
f2usdtry<-USDTRY%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))
  
# tarih aralığını saptadık
range(edata$Date)
#"2010-01-01" "2020-03-19"

#sütun isimlerini öğrendik
names(edata)
#çıkarmak istediğimiz sütun isimlerini aşağıda yazarak çıkarttık
df = subset(edata, select = -c(Adj.Close,High,Low,Open,Volume) )

#istenmeyen sütunlardan temizlemek verisetini erdata değişkenine atadık.


erdata<-df

#Date formatına çevirme 
erdata<-erdata%>%
  mutate(Date=as.Date(erdata$Date))
typeof(erdata$Date)

#left join işlemi ile aralarında bulunan tarih farklarını eledik ve zaman aralıkları işlendi


matching<-left_join(f2usdtry,erdata, by = c("date" = "Date"))
names(matching)



matching2= subset(matching, select = -c(weekday) )
colnames(matching2)<- c("date","usd_try","eregli")
names(matching2)



melted_matchings2<-melt(matching2,id=c("date"))

ggplot(melted_matchings2,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini belirledik
  theme_bw()+
  labs(title="Yıllara Göre Dolar Türk Lirası Ve Eregli Demir Çelik Fiyat Değişimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlıklar",
                     labels=c("USDTRY","EREGLI"), 
                     values=c("green4", "firebrick1","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini düzenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend ?izgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari düzenledik


## Yanl?? G?rselle?tirme t?r? y 0 dan ba?lam?yor

ggplot(melted_matchings2,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yıllara Göre Dolar Türk Lirası Ve Eregli Demir Çelik Hisse  
                         Fiyatı  Değişimi",x = "Y?llar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlıklar",
                     labels=c("USDTRY","EREGLI"), 
                     values=c("green4", "firebrick1","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini d?zenledik
  geom_smooth(se = T)+ # grafige trend ?izgisi yerlestirdik
  expand_limits(y=c(10,15)) 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------







# ISDEMIR USDTRY PAR?TES?  F?YAT KAR?ILA?TIRMASI



isdmr<-read.csv("/home/deniz/Masaüstü/veri_gorsellestirme_odevi/datasets/ISDMR.csv")

range(isdmr$Date)
#"2016-03-28" "2020-03-19"

USDTRY2 <- historical_exchange_rates("USD",
                                    to ="TRY",
                                    start_date="2016-03-28",
                                    end_date = "2020-03-19")

colnames(USDTRY2)[2]="usd_to_try"


#haftan?n  g?nlerini ?ektik usdtry paritesi i?in 
utry<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta g?nleri i?inden pazartesi ve cuma olan ?al??ma g?nlerini ?ektik 
u2try<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#s?tun isimlerini ??rendik
names(isdmr)
#??karmak istedi?imiz s?t?n isimlerini a?a??da yazarak ??kartt?k
#istenmeyen s?t?nlardan temizlenmi? verisetini isdmr2 de?i?kenine atad?k.
isdmr2= subset(isdmr, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date format?na ?evirme 
isdmr2<-isdmr2%>%
  mutate(Date=as.Date(isdmr2$Date))
typeof(isdmr2$Date)

#left join i?lemi ile aralar?nda bulunan tarih farklar?n? eledik ve zaman aral?klar? e?itlendi


isdmrmatch<-left_join(u2try,isdmr2, by = c("date" = "Date"))
names(isdmrmatch)



isdmrmatch2= subset(isdmrmatch, select = -c(weekday) )
colnames(isdmrmatch2)<- c("date","usd_try","isdmr")
names(isdmrmatch2)



melted_matchings3<-melt(isdmrmatch2,id=c("date"))




# Zaman serisi Do?ru g?rselle?tirme
ggplot(melted_matchings3,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yıllara Göre Dolar Türk Lirası Ve  Iskenderun Demir Çelik Hisse Fiyatı Değişimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlıklar",
                     labels=c("USDTRY","ISDMR"), 
                     values=c("green4","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini d?zenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend ?izgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari d?zenledik





#Zaman Serisi Yanl?? g?rselle?tirme

ggplot(melted_matchings3,aes(x=date,y=value)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yıllara göre Dolar Türk Lirası Ve Iskenderun Demir çelik Fiyat Değişimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  expand_limits(y=c(8,15))  # y ekseninin eksende yer alan sayilari d?zenledik











#---------------------------------------------------------------------------------------------------





# SISECAM VS USDTRY PAR?TES? KARSILASTIRMA


sise<-read.csv("/home/deniz/Masaüstü/veri_gorsellestirme_odevi/datasets/SISE.csv")

range(sise$Date)
#"2016-03-28" "2020-03-19"

USDTRY3 <- historical_exchange_rates("USD",
                                     to ="TRY",
                                     start_date="2010-01-01",
                                     end_date = "2020-03-19")

colnames(USDTRY3)[2]="usd_to_try"


#haftan?n  g?nlerini ?ektik usdtry paritesi i?in 
stry-USDTRY3%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta g?nleri i?inden pazartesi ve cuma olan ?al??ma g?nlerini ?ektik 
s2try<-USDTRY3%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#s?tun isimlerini ??rendik
names(sise)
#??karmak istedi?imiz s?t?n isimlerini a?a??da yazarak ??kartt?k
#istenmeyen s?t?nlardan temizlenmi? verisetini sise2 de?i?kenine atad?k.
sise2= subset(sise, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date format?na ?evirme 
sise2<-sise2%>%
  mutate(Date=as.Date(sise2$Date))
typeof(sise2$Date)

#left join i?lemi ile aralar?nda bulunan tarih farklar?n? eledik ve zaman aral?klar? e?itlendi


sisematch<-left_join(s2try,sise2, by = c("date" = "Date"))
names(sisematch)



sisematch2= subset(sisematch, select = -c(weekday) )
colnames(sisematch2)<- c("date","usd_try","sise")
names(sisematch2)



melted_matchings4<-melt(sisematch2,id=c("date"))




# Zaman serisi Do?ru g?rselle?tirme
ggplot(melted_matchings4,aes(x=date,y=value,color=variable)) +
  geom_line(alpha=0.8,position = "dodge") + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yıllara Göre Dolar Türk Lirası Ve ŞişeCam Hisse Senedi Fiyat Değişimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varlıklar",
                     labels=c("USDTRY","SISE"), 
                     values=c("green4","black")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini d?zenledik
  geom_smooth(se = T,size=0.5)+ # grafige trend ?izgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari d?zenledik

#-----------------------------------------------------------------------------------------------------------------------------------------------






# AKSA VS USDTRY PAR?TES?




aksa<-read.csv("/home/deniz/Masaüstü/veri_gorsellestirme_odevi/datasets/AKSA.csv")

range(aksa$Date)
#"2010-01-01" "2020-03-19"

USDTRY4 <- historical_exchange_rates("USD",
                                     to ="TRY",
                                     start_date="2010-01-01",
                                     end_date = "2020-03-19")

colnames(USDTRY2)[2]="usd_to_try"


#haftan?n  g?nlerini ?ektik usdtry paritesi i?in 
utry4<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date))
# hafta g?nleri i?inden pazartesi ve cuma olan ?al??ma g?nlerini ?ektik 
utry4<-USDTRY2%>%
  select(date,usd_to_try) %>%
  mutate(weekday = wday(date, label=TRUE)) %>%
  filter(!wday(date) %in% c(1, 7))

#s?tun isimlerini ??rendik
names(aksa)
#??karmak istedi?imiz s?t?n isimlerini a?a??da yazarak ??kartt?k
#istenmeyen s?t?nlardan temizlenmi? verisetini isdmr2 de?i?kenine atad?k.
aksa2= subset(aksa, select = -c(Adj.Close,High,Low,Open,Volume) )



#Date format?na ?evirme 
aksa2<-aksa2%>%
  mutate(Date=as.Date(aksa2$Date))
typeof(aksa2$Date)

#left join i?lemi ile aralar?nda bulunan tarih farklar?n? eledik ve zaman aral?klar? e?itlendi


aksamatch<-left_join(utry4,aksa2, by = c("date" = "Date"))
names(aksamatch)



aksamatch= subset(aksamatch, select = -c(weekday) )
colnames(aksamatch)<- c("date","usd_try","isdmr")
names(aksamatch)



melted_matchings4<-melt(aksamatch,id=c("date"))




# Zaman serisi Do?ru g?rselle?tirme
ggplot(melted_matchings4,aes(x=date,y=value,color=variable)) +
  geom_line() + # grafik tipini berlirledik
  theme_bw()+
  labs(title="Yıllara Göre Dolar Türk Lirası ve Aksa Akrilik Hisse Fiyatı Değişimi",x = "Yillar",
       y = "Fiyat(TL)") + # grafik basligi eksen isimlerini ayarladik
  scale_color_manual(name="Varl?klar",
                     labels=c("USD/TRY","AKSA"), 
                     values=c("green4","darkorange")) +  # lejant ismi ve lejantta yer alan degiskenlerin isimlerini d?zenledik
  geom_smooth(se = T,size=0.4)+ # grafige trend ?izgisi yerlestirdik
  expand_limits(y=c(0,15))  # y ekseninin eksende yer alan sayilari d?zenledik



