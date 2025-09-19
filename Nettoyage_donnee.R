#Nettoyage des données 
library(dplyr)
athletes <- read.csv("athlete_events.csv", header = TRUE, stringsAsFactors=TRUE)

athletes <- athletes %>% filter(Year>1924) %>%
  mutate(Medal = ifelse(is.na(Medal), "No_medal", Medal))%>% mutate(Medal=as.factor(Medal)) %>% na.omit()
summary(athletes)
