#packages utilisés ---
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(factoextra)
library(patchwork)
library(scales)
library(forcats)
library("ggpubr")
library("ggrepel")
#library(ggiraph)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(visNetwork)
library(heatmaply)

dtaf_loaded <- readRDS("data/dtaf.RDS")

# Base géo UNIQUE et propre (circonscriptions)
dtaf_base <- dtaf_loaded %>%
  st_make_valid() %>%
  st_transform(2154) %>%
  mutate(codeCirconscription = as.character(codeCirconscription))
