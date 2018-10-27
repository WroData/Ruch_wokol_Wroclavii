#2 kod Mapa

setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\20181009 Skrzy¿owania Wroclavia\\")



options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "en")

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
#library(lubridate) # do operacji na datach
#library(plyr) #do join  
library(ggplot2) #do wykresów
library(scales) #do napisania osi jako %
library(dplyr) #do select distinct
library(tidyr) #do zmiany wide w long
library(extrafont) #do czcionek
library(stringr) #do wrap labels
library(lubridate) #do ilosci dni w moiesiacu
library(ggpubr)
library(grid) #do polaczenia wykresow w jeden
#install.packages("gridExtra")
library(gridExtra) #do polaczenia wykresow w jeden
#install.packages("lubridate")
#library(ggmap) # do mapy
#install.packages("ggmap")
install.packages("devtools")
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap) # do mapy


#########################

load("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\API_KEY.RData") 
register_google(key = API_KEY)

#########################
#obróbka danych


slownik_skrzysowan <- read_excel("9 skrzy¿owañ natê¿enia dzienne 2018.xlsx", sheet = 2)
names(slownik_skrzysowan) <- c("Nr_Skrzyzowania", "Opis", "lat", "lon")

# slownik_skrzysowan <- slownik_skrzysowan %>%
#   mutate(Adres = paste(Opis, ", Wroclaw"),
#          wspolzendne = geocode(Adres))
# 
# geocodeQueryCheck() 

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
         Swieta = ifelse(data == "2018-04-01", 1, 0), 
         Handlowa = ifelse(Pierwsza_niedziela + Ostatnia_niedziela + Swieta > 0 |
                             Miesiac < 3,
                           "Niedziela handlowa", "Niedziela nie handlowa")) %>%
  merge(y = slownik_skrzysowan, by = "Nr_Skrzyzowania", by.y = "Nr_Skrzyzowania", all = T) %>%
  filter(Dzien_tyg == "niedziela") %>%
  group_by(Handlowa, Opis, lon, lat) %>%
  mutate(sr_niedziela = mean(Liczba_Pojazdow)) %>%
  ungroup(Handlowa, Opis, lon, lat) %>%
  group_by(sr_niedziela, Handlowa, Opis, lon, lat) %>%
  summarise(sr = mean(Liczba_Pojazdow)) %>%
  group_by(Opis) %>%
  arrange(Handlowa, .by_group = TRUE) %>%
  mutate(pct_change = (sr/lead(sr) - 1)) %>%
  filter(Handlowa == "Niedziela handlowa")



###########
#Mapa
Theme <-  theme(text = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
  
                legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                #legend.title = element_blank(),
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.title      = element_text(family = "Ubuntu", size = 12, color = "#22211d"),
                legend.text       = element_text(family = "Ubuntu", size = 10, hjust = 0, color = "#22211d"),
                
                axis.title   = element_blank(),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x  = element_blank(),
                axis.ticks.x = element_blank(),
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.text.x  = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                #axis.text.y  = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
                #axis.ticks.x = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
)  




y = 51.0967152
x = 17.0319616
margines_x = 0.03
margines_y = 0.03

map <- get_map(location = c(left   = x - margines_x, 
                            bottom = y - margines_y, 
                            right  = x + margines_x, 
                            top    = y + margines_y),
               source = 'google', maptype = 'roadmap')
Map <- ggmap(map, extent = 'normal')
Map


Mapa <- Map + 
  # ggplot() + 
  geom_point(data = dane_pkt, aes(x = lon + 0.0025, 
                                  y = lat,
                                  fill = pct_change,
                                  size = sr_niedziela),
             colour="black", pch=21,
             alpha = 0.9) +
  xlim(c(17.008, 17.05))  + 
  ylim(c(51.08,  51.105)) +
  scale_fill_gradient2(midpoint = 0, 
                        low  = "#065535",
                        mid  = "white",
                        high = "#85000b", 
                        space = "Lab",
                        labels = function(x) format(paste0(round(x*100), "%"), scientific = FALSE)) + 
  scale_size_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE),
                        range = c(3, 15)) +
  labs(title = paste0("Nate¿enie ruchu samochodowego\nna 9 skrzy¿owaniach wokó³ Wroclavii"),
       subtitle = paste0("Na podstawie danych z ITS"),
       x = "",
       y = "",
       caption = "Autor: WroData | ród³o: ZDIUM",
       fill  = stringr::str_wrap("Ró¿nica w natê¿eniu ruchu w niedzielê handlow¹ w porównaniu do niedzieli nie handlowej", 30),
       size  = stringr::str_wrap("Œrednia liczba samochodów pokonuj¹ca skrzy¿owanie w ci¹gu niedzieli", 30)) +
  Theme +
  theme(legend.box = "vertical")

Mapa



########
#zapis

a <- 9

png(filename = paste("Mapa ", Sys.Date(), ".png", sep=""),
    bg="white", width = a , height = a * 1.25, units = 'in', res = 150)
  plot(Mapa )
dev.off()
