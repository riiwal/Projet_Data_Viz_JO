library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)

#On créer un subset avec seulement les votes et les circonscription
top3<-dtaf[,c(3,56:67)] %>% 
  pivot_longer(cols=2:13,
               names_to = "candidat",
               values_to = "pourcentage")

top3$candidat <- as.factor(top3$candidat)

summary(top3)

#la j'ai les 3 premiers 
top3<-top3 %>% 
  group_by(codeCirconscription) %>% 
  slice_max(pourcentage, n = 3, with_ties = FALSE)

top3 <- top3 %>%
  mutate(candidat = recode(candidat,
                           "Arthaud_exp" = "Arthaud",
                           "Roussel_exp" = "Roussel",
                           "Macron_exp" = "Macron",
                           "Lassalle_exp" = "Lasalle",
                           "LePen_exp" = "LePen",
                           "Zemmour_exp" = "Zemmour",
                           "Melenchon_exp" = "Melenchon",
                           "Hidalgo_exp" = "Hidalgo",
                           "Jadot_exp" = "Jadot",
                           "Pecresse_exp" = "Pecresse",
                           "Poutou_exp" = "Poutou",
                           "DupontAignan_exp" = "DupontAignan"))



ggplot(dtaf) +
  geom_sf(aes(fill=act_cho), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "XXX par Circonscriptions")+
  scale_fill_distiller(palette = "RdYlGn", direction = -1) 


ggplot(dtaf) +
  geom_sf(aes(fill=act_cho), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "XXX par Circonscriptions")+
  scale_fill_distiller(palette = "RdYlGn", direction = -1) 

