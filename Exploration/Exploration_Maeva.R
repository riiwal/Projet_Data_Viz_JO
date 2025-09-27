# exploration des données


summary(dtaf)
# Packages nécessaires
library(tidyverse)
library(sf)
library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(factoextra)
library(viridis)

# Vérification de la structure
str(dtaf)

# ---- 1. ANALYSE UNIVARIÉE ----
# Variables électorales principales
num_vars <- c("Inscrit_22", "pop_légal_19", "age_moyen", "actemp", "actcho", "inactret",
              ,    "actdip_PEU","actdip_CAP","actdip_BAC","actdip_BAC2","actdip_BAC3",
              ,    "actdip_BAC5","proprio","locatai","mfuel","men_seul","tx_pauvrete60_diff",
              ,    "nivvie_median_diff","% Voix/Ins3","% Voix/Ins5","% Voix/Ins7","% Voix/Ins8","% Voix/Ins9","% Voix/Ins10","% Voix/Ins11","% Voix/Ins12")

# Histogrammes et densités
for (v in num_vars) {
  print(
    ggplot(dtaf, aes_string(x = v)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      geom_density(color = "red", size = 1, alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Distribution de", v))
  )
}

# ---- 2. ANALYSE BIVARIÉE ----
# Votes principaux en fonction de critères sociodémographiques
votes_vars <- c("`% Voix/Ins3`", "`% Voix/Ins5`", "`% Voix/Ins7`", "`% Voix/Ins8`", "`% Voix/Ins9`", "`% Voix/Ins10`", "`% Voix/Ins11`", "`% Voix/Ins12`")

for (v in votes_vars) {
  for (crit in c("age_moyen","actcho","actemp","proprio","tx_pauvrete60_diff","nivvie_median_diff")) {
    print(
      ggplot(dtaf, aes_string(x = crit, y = v)) +
        geom_point(alpha = 0.6, color = "darkblue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = paste(v, "selon", crit))
    )
  }
}

# ---- 3. MATRICE DE CORRÉLATIONS ----
# Corrélation entre socio-démographie et votes

votes_vars <- c("% Voix/Ins3", "% Voix/Ins5", "% Voix/Ins7", "% Voix/Ins8", "% Voix/Ins9", "% Voix/Ins10", "% Voix/Ins11", "% Voix/Ins12")

subdata <- dtaf %>%
  st_drop_geometry() %>%
  select(all_of(c(num_vars, votes_vars))) %>%
  na.omit()

corr <- cor(subdata)
corrplot(corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7,
         col = brewer.pal(n = 8, name = "RdBu"))

#carte
# Taux de votes principaux sur la carte
votes_vars <- c("`% Voix/Ins3`", "`% Voix/Ins5`", "`% Voix/Ins7`", "`% Voix/Ins8`", "`% Voix/Ins9`", "`% Voix/Ins10`", "`% Voix/Ins11`", "`% Voix/Ins12`")

for (v in votes_vars) {
  print(
    ggplot(dtaf) +
      geom_sf(aes_string(fill = v), color = NA) +
      scale_fill_viridis(option = "plasma", direction = -1) +
      theme_void() +
      labs(title = paste("Répartition géographique de", v))
  )
}

# COMPARAISONS PAR DÉPARTEMENT
dtaf %>%
  st_drop_geometry() %>% 
  group_by(nomDepartement) %>%
  summarise(
    moy_voix_lepen = mean(`% Voix/Ins5`, na.rm = TRUE),
    moy_voix_macron = mean(`% Voix/Ins3`, na.rm = TRUE),
    moy_voix_melenchon = mean(`% Voix/Ins7`, na.rm = TRUE)
  ) %>%
  pivot_longer(-nomDepartement, names_to = "candidat", values_to = "moy_voix") %>%
  ggplot(aes(x = reorder(nomDepartement, moy_voix), y = moy_voix, fill = candidat)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Votes moyens par département")


# Clustering des circonscriptions
set.seed(123)
res_kmeans <- kmeans(scale(subdata), centers = 4, nstart = 25)
fviz_cluster(res_kmeans, data = scale(subdata))


# Ajout des clusters dans la carte
dtaf$cluster <- factor(res_kmeans$cluster)
ggplot(dtaf) +
  geom_sf(aes(fill = cluster), color = NA) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  labs(title = "Typologie des circonscriptions (K-means)")


#variables trop corrélées
cor_mat <- cor(dtaf_acp, use = "pairwise.complete.obs")
library(corrplot)
corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.6)
