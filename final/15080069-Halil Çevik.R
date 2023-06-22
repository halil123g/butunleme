# -------------------------------------------------------------------------- ###
# Soru 1a ----  https://github.com/halil123g/final
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ----
data <- read.csv("c:/datasets/titanic.csv")
head(data)
library(dplyr)

t.test(fare~sex,data=data)
#bağımsız t-testi ile değerlendirildiğinde kadınların (ortalama 46.1981),
#erkeklere göre daha fazla ödediği görülmüştür (t(701)=6.161, p<0.001
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ----
library(tidyverse)
ggplot(data=data)+geom_boxplot(aes(x=sex,y=age))
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ----
ggplot(data=data,aes(x=age))+
  geom_histogram(aes(y = ..density..),alpha=0.3,color="black",fill="white")+
  geom_density(fill="pink",alpha=0.3)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a ----
x <- 10:20
x[seq(1, 5, by = 3)]
#X vektöründeki 1 ve 3. sıradaki değerler seçilir (10 ve 13)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b ----
#bunun için merge komutu kullanılabilir
dat3 <- merge(dat1, dat2, all = TRUE)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----
#grafik scatterplot (saçlımı grafiğidir) aynı zamanda regresyon çizgisi yer almaktadır
#ayrıca %95 GA içermektedir
#örnek kod:
ggplot(data=data)+geom_point(aes(x=a,y=b))+geom_smooth(method = "lm", se = TRUE)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ----
library(tidyverse)
mylist <- list(1:3, c(3:5, NA))
myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()
#cevap olarak çıktıda 2 4 alınır
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ----
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----
zarlar <- function() {
  zar1 <- sample(1:6, 1, replace = TRUE)
  zar2 <- sample(1:6, 1, replace = TRUE)

  cat("1. zar: ", zar1, "\n")
  cat("2. zar: ", zar2, "\n")
}

zarlar()
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----
t.test(age~survived,data=data)
#kurtulan ve kurtulamayan yoluclar arasında yaşları karşılaştırıldığında
# bağımsız t testi ile, kurtulmayanaların ortalaması 30.5 ve kurtulanların
#ortalaması 28. çıktığı ve p=0.07696 çıktı ve bu değer 0.05'den yüksek olduğu
#için bu iki grup arasından yaşların farkı yoktur.
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----
dat1 <- tibble(country=as.factor(c("Ingiltere","Almanya")),
               "2018"=c(8000,10000),"2019"=c(8100,11000),
               "2020"=c(8500,10200))
dat2 <- pivot_longer(
  data = dat1,
  cols = -country,
  names_to = "year",
  values_to = "gdp")
# -------------------------------------------------------------------------- ###

# -------------------------------------------------------------------------- ###
# Soru 5a ----
ggplot(data=dat1)+geom_histogram(aes(x=price))+facet_grid(cols=vars(cut))
# -------------------------------------------------------------------------- ###
