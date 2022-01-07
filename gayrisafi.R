install.packages("readxl")

library(readxl)
library(ggplot2)
my_data <- read_excel("/home/deniz/Masaüstü/veri_gorsellestirme_odevi/gsyh.xls")
str(my_data)




ggplot(data =my_data , aes(x =Seneler, y =Value_TRY , group = 1))+
  geom_point()+
  geom_line(color = "red", size = 0.5) +
  geom_smooth(se=FALSE,span = 0.1,method="loess",size=0.5) +
  labs(x = "Yıllar", y = "Kişi başı gayri safi milli hasıla(tl)")+
  theme_classic() 
