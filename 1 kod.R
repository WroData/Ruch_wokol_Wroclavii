rm(list = ls())
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\20181009 Skrzy¿owania Wroclavia\\")

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
names(slownik_skrzysowan) <- c("Nr_Skrzyzowania", "Opis")



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


srednio <- natezenie %>%
  filter(Dzien_tyg == "niedziela") %>%
  group_by(Handlowa, Opis) %>%
  summarise(sr = mean(Liczba_Pojazdow))

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
                panel.border = element_blank()
)  

w <- ggplot(natezenie[natezenie$Dzien_tyg == "niedziela", ], 
             aes(x = data, y = Liczba_Pojazdow, colour = Handlowa)) +
        geom_point( ) + #size = 3) +
        facet_wrap(~ Opis, ncol = 3) +
        labs(title = paste0("Nate¿enie ruchu samochodowego\nna 9 skrzy¿owaniach wokó³ Wroclavii"),
             subtitle = paste0("Na podstawie danych z ITS"),
             x = "",
             y = "Liczba pojazdów",
             caption = "Autor: WroData | ród³o: ZDIUM" ) +
        scale_colour_manual(
          values = c("#fe9929", "#d95f0e"),
          breaks = c("Niedziela handlowa", "Niedziela nie handlowa"),
          labels = c("Niedziela handlowa", "Niedziela nie handlowa")) + 
        geom_hline(data = srednio[srednio$Handlowa == "Niedziela handlowa",], 
                   mapping = aes(yintercept = sr), 
                   colour = "#fe9929", linetype = 2, #size = 1.5,
                   inherit.aes = FALSE) + 
        geom_hline(data = srednio[srednio$Handlowa == "Niedziela nie handlowa",], 
                   mapping = aes(yintercept = sr), 
                   colour = "#d95f0e", linetype = 2, #size = 1.5,
                   inherit.aes = FALSE) +  
        Theme +
        scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
        guides(color = guide_legend(override.aes = list(size = 4 )))



plot(w)

# wielkoœæ obrazka
a <- 9

png(filename = paste("W1 ", Sys.Date(), ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
  plot(w)
dev.off()