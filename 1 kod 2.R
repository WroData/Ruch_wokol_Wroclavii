
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

# 
# #https://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-r
# numberOfDays <- function(date) {
#   m <- format(date, format="%m")
#   
#   while (format(date, format="%m") == m) {
#     date <- date + 1
#   }
#   
#   return(as.integer(format(date - 1, format="%d")))
# }


slownik_skrzysowan <- read_excel("9 skrzy¿owañ natê¿enia dzienne 2018.xlsx", sheet = 2)
names(slownik_skrzysowan) <- c("Nr_Skrzyzowania", "Opis", "lon", "lat")



natezenie <- read_excel("9 skrzy¿owañ natê¿enia dzienne 2018.xlsx", sheet = 1) %>%
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
  merge(y = slownik_skrzysowan, by = "Nr_Skrzyzowania", by.y = "Nr_Skrzyzowania", all = T)



kolejnosc <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  group_by(Opis) %>%
  summarise(sr = mean(Liczba_Pojazdow)) %>%
  arrange(desc(sr))

natezenie$Opis <- factor(natezenie$Opis, levels = unique(kolejnosc$Opis))
levels(natezenie$Opis)


Dane_W1 <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  filter(Opis %in% c("Œlê¿na-Armii Krajowej", 
                     "Powstañców Œl¹skich-Al. Hallera",
                     "Borowska-Armii Krajowej"))

Dane_W2 <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  filter(Opis %in% c("Bardzka-Armii Krajowej", 
                     "Powstañców Œl¹skich-Swobodna",
                     "Œwidnicka-Pi³sudskiego"))

Dane_W3 <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  filter(Opis %in% c("Ko³³¹taja-Pi³sudskiego", 
                     "Stawowa-Pi³sudskiego",
                     "Stawowa-Peronowa"))




srednio <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  group_by(Handlowa, Opis) %>%
  summarise(sr = mean(Liczba_Pojazdow)) %>%
  group_by(Opis) %>%
  arrange(Handlowa, .by_group = TRUE) %>%
  mutate(pct_change = (sr/lead(sr) - 1) * 100)






Dane_W1_srednio <- srednio %>%
  filter(Opis %in% c("Œlê¿na-Armii Krajowej", 
                     "Powstañców Œl¹skich-Al. Hallera",
                     "Borowska-Armii Krajowej"))

Dane_W2_srednio <- srednio %>%
  filter(Opis %in% c("Bardzka-Armii Krajowej", 
                     "Powstañców Œl¹skich-Swobodna",
                     "Œwidnicka-Pi³sudskiego"))

Dane_W3_srednio <- srednio %>%
  filter(Opis %in% c("Ko³³¹taja-Pi³sudskiego", 
                     "Stawowa-Pi³sudskiego",
                     "Stawowa-Peronowa"))

#wykres

Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 12, hjust = 0, color = "#22211d"),
                #axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                #axis.title   = element_blank(),
                #axis.text.y  = element_blank(),
                #axis.ticks.y = element_blank(),
                
                #axis.ticks.x = element_text(family = "Ubuntu", size = 11, color = "#22211d"),
                text = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                axis.text.x  = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                axis.text.y  = element_text(family = "Ubuntu", size = 10, color = "#22211d"),
                strip.text.x = element_text(family = "Ubuntu", size = 12, color = "#22211d"),
                panel.grid.major = element_blank(),
                #panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border  = element_blank()
)  


W1 <- ggplot(Dane_W1, 
             aes(x = data, y = Liczba_Pojazdow, colour = Handlowa)) +
  geom_point( ) + #size = 3) +
  facet_wrap(~ Opis, ncol = 3) + #,  scales = "free_y"
  labs(title = paste0("Nate¿enie ruchu samochodowego\nna 9 skrzy¿owaniach wokó³ Wroclavii"),
       subtitle = paste0("Na podstawie danych z ITS"),
       x = "",
       y = "Liczba pojazdów",
       caption = "" ) +
  scale_colour_manual(
    values = c("#fe9929", "#d95f0e"),
    breaks = c("Niedziela handlowa", "Niedziela nie handlowa"),
    labels = c("Niedziela handlowa", "Niedziela nie handlowa")) + 
  coord_cartesian(ylim = c(40000, 60000)) +
  geom_hline(data = Dane_W1_srednio[Dane_W1_srednio$Handlowa == "Niedziela handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#fe9929", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) + 
  geom_hline(data = Dane_W1_srednio[Dane_W1_srednio$Handlowa == "Niedziela nie handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#d95f0e", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) +  
  Theme +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  guides(color = guide_legend(override.aes = list(size = 4 ))) +
  theme(legend.position="none")



plot(W1)



##############################
W2 <- ggplot(Dane_W2, 
             aes(x = data, y = Liczba_Pojazdow, colour = Handlowa)) +
  geom_point( ) + #size = 3) +
  facet_wrap(~ Opis, ncol = 3) + #,  scales = "free_y"
  labs(title = paste0(""),
       subtitle = paste0(""),
       x = "",
       y = "Liczba pojazdów",
       caption = "" ) +
  scale_colour_manual(
    values = c("#fe9929", "#d95f0e"),
    breaks = c("Niedziela handlowa", "Niedziela nie handlowa"),
    labels = c("Niedziela handlowa", "Niedziela nie handlowa")) + 
  coord_cartesian(ylim = c(20000, 35000)) +
  geom_hline(data = Dane_W2_srednio[Dane_W2_srednio$Handlowa == "Niedziela handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#fe9929", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) + 
  geom_hline(data = Dane_W2_srednio[Dane_W2_srednio$Handlowa == "Niedziela nie handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#d95f0e", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) +  
  Theme +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  guides(color = guide_legend(override.aes = list(size = 4 ))) +
  theme(legend.position="none")



plot(W2)
################################
W3 <- ggplot(Dane_W3, 
             aes(x = data, y = Liczba_Pojazdow, colour = Handlowa)) +
  geom_point( ) + #size = 3) +
  facet_wrap(~ Opis, ncol = 3) + #,  scales = "free_y"
  labs(title = paste0(""),
       subtitle = paste0(""),
       x = "",
       y = "Liczba pojazdów",
       caption = "Autor: WroData | ród³o: ZDIUM" ) +
  scale_colour_manual(
    values = c("#fe9929", "#d95f0e"),
    breaks = c("Niedziela handlowa", "Niedziela nie handlowa"),
    labels = c("Niedziela handlowa", "Niedziela nie handlowa")) + 
  coord_cartesian(ylim = c(15000, 30000)) +
  geom_hline(data = Dane_W3_srednio[Dane_W3_srednio$Handlowa == "Niedziela handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#fe9929", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) + 
  geom_hline(data = Dane_W3_srednio[Dane_W3_srednio$Handlowa == "Niedziela nie handlowa",], 
             mapping = aes(yintercept = sr), 
             colour = "#d95f0e", linetype = 2, #size = 1.5,
             inherit.aes = FALSE) +  
  Theme +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  guides(color = guide_legend(override.aes = list(size = 4 )))

plot(W3)
 ################
#polaczenie wykresu

w <- ggarrange(W1, W2, W3, nrow = 3, 
               #common.legend = TRUE, legend="bottom", 
               heights = c(3, 3, 3.75)) +
  bgcolor("#f5f5f2")+
  border("#f5f5f2")



plot(w)


# wielkoœæ obrazka
a <- 9

png(filename = paste("W ", Sys.Date(), ".png", sep=""),
    bg="white", width = a * 1.5, height = a * 1.2 / 2, units = 'in', res = 150)
  plot(w)
dev.off()


#W1
png(filename = paste("W1 ", Sys.Date(), ".png", sep=""),
    bg="white", width = a , height = a * 1.1  / 2, units = 'in', res = 150)
plot(W1)
dev.off()

#w2
png(filename = paste("W2 ", Sys.Date(), ".png", sep=""),
    bg="white", width = a , height = a * 1    / 2, units = 'in', res = 150)
plot(W2)
dev.off()


#w3
png(filename = paste("W3 ", Sys.Date(), ".png", sep=""),
    bg="white", width = a , height = a * 1.15 / 2, units = 'in', res = 150)
plot(W3)
dev.off()
