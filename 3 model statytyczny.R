

setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\20181009 Skrzy¿owania Wroclavia\\")



options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")
options(scipen=999)

##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
rm(list = ls())
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów ####
# Za³adowanie danych 
require(readxl) #do wczytania z Excela
library(dplyr) #do select distinct
library(lubridate) #do ilosci dni w moiesiacu
library(ggplot2)
library(MASS) #do automatycznej selekcji

#########################
#obróbka danych


slownik_skrzysowan <- read_excel("9 skrzy¿owañ natê¿enia dzienne 2018.xlsx", sheet = 2)
names(slownik_skrzysowan) <- c("Nr_Skrzyzowania", "Opis", "lat", "lon")

swieta <- as.Date(c("2018-01-01", "2018-01-06", "2018-04-01", "2018-04-02", 
            "2018-05-01", "2018-05-03", "2018-05-20", "2018-05-31",
            "2018-08-15", "2018-11-01", "2018-11-11", "2018-01-01",
            "2018-12-24", "2018-12-25"))


dane_pkt <- read_excel("9 skrzy¿owañ natê¿enia dzienne 2018.xlsx", sheet = 1) %>%
  mutate(data = as.Date(paste0(Rok, 
                               ifelse(Miesiac < 10, paste0("0", Miesiac), Miesiac), 
                               ifelse(Dzien   < 10, paste0("0", Dzien  ), Dzien  ),
                               sep = ""),
                        format = "%Y%m%d" ),
         Dzien_tyg = format(data, format = "%A"),
         Dni_w_miesiacu = days_in_month(data),
         #https://stackoverflow.com/questions/34577812/extract-first-monday-of-every-month/34577841
         Pierwsza_niedziela = ifelse(weekdays(data) == "niedziela" & 
                                       as.numeric(format(data, "%d")) <= 7,
                                     1, 0),
         Ostatnia_niedziela = ifelse(weekdays(data) == "niedziela" & 
                                       as.numeric(format(data, "%d")) >= Dni_w_miesiacu - 7,
                                     1, 0),
         Swieta = ifelse(data %in% swieta, 1, 0), 
         Handlowa = ifelse((Pierwsza_niedziela + Ostatnia_niedziela > 0 & Miesiac >= 3) ,
                           "Niedziela handlowa", "Niedziela nie handlowa"),
         Handlowa = ifelse(Swieta > 0, "Œwiêta", Handlowa),
         Dzien_tyg = ifelse(Dzien_tyg == "niedziela" | Swieta > 0, Handlowa, Dzien_tyg),
         Dzien_tyg = factor(Dzien_tyg, levels = c("Niedziela nie handlowa", "Œwiêta", "Niedziela handlowa",
                                                  "poniedzia³ek", "wtorek", "œroda",
                                                  "czwartek", "pi¹tek", "sobota")),
         
         Handlowa_lag7 = lag(Handlowa, 7),
         
         Liczba_Pojazdow_lag1 = lag(Liczba_Pojazdow, 1),
         Liczba_Pojazdow_lag2 = lag(Liczba_Pojazdow, 2),
         Liczba_Pojazdow_lag3 = lag(Liczba_Pojazdow, 3),
         Liczba_Pojazdow_lag4 = lag(Liczba_Pojazdow, 4),
         Liczba_Pojazdow_lag5 = lag(Liczba_Pojazdow, 5),
         Liczba_Pojazdow_lag6 = lag(Liczba_Pojazdow, 6),
         Liczba_Pojazdow_lag7 = lag(Liczba_Pojazdow, 7)) %>%
  merge(y = slownik_skrzysowan, by = "Nr_Skrzyzowania", by.y = "Nr_Skrzyzowania", all = T)



###########
# testy danych
if(F){
#ogólny wykres
ggplot(dane_pkt, aes(Liczba_Pojazdow)) + 
  geom_histogram(aes(y = (..density.. * 5)), colour = "grey50")

# w rozbiciu
ggplot(dane_pkt, aes(Liczba_Pojazdow)) + 
  geom_histogram(aes(y = (..density.. * 5)), colour = "grey50") +
  facet_wrap(Opis ~ Dzien_tyg)

}

###########
#modelowanie

linearMod_1 <- lm(Liczba_Pojazdow ~ ., 
                  data=dane_pkt)  # build linear regression model on full data
summary(linearMod_1)

linearMod_1_AIC <- stepAIC(linearMod_1)
summary(linearMod_1_AIC)


linearMod_2 <- lm(Liczba_Pojazdow ~ 
                    Dzien_tyg+
                    Opis +
                    Miesiac  + 
                    Liczba_Pojazdow_lag1 + Liczba_Pojazdow_lag2 + Liczba_Pojazdow_lag3 + Liczba_Pojazdow_lag4 + 
                    Liczba_Pojazdow_lag5 + Liczba_Pojazdow_lag6 + Liczba_Pojazdow_lag7, 
                  data = dane_pkt)  # build linear regression model on full data

summary(linearMod_2)

linearMod_2_AIC <- stepAIC(linearMod_2)
summary(linearMod_2_AIC)






linearMod_3 <- lm(Liczba_Pojazdow ~ 
                    Dzien_tyg +  
                    Opis +
                    Liczba_Pojazdow_lag1 + Liczba_Pojazdow_lag2 + Liczba_Pojazdow_lag3 +
                    Liczba_Pojazdow_lag4 + Liczba_Pojazdow_lag5 + Liczba_Pojazdow_lag6 +
                    Liczba_Pojazdow_lag7, 
                  data = dane_pkt) 

summary(linearMod_3)



