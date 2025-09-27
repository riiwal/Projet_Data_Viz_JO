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

# ---- 5. CARTOGRAPHIE ----
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

# ---- 6. COMPARAISONS PAR DÉPARTEMENT ----
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


# ---- 7. PROFILS TYPES ----
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

# -------------------- ACP -------------------

str(dtaf)
labels(dtaf)
library(FactoMineR)
library(factoextra)

vars_acp <- c("pop_légal_19","pop_pole_aav","pop_cour_aav","pop_horsaav",
              "pop_urb","pop_rur_periu","pop_rur_non_periu","age_moyen",
              "actemp","actcho","inactret","actdip_PEU","actdip_CAP",
              "actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5",
              "actdip_BAC3P","act_agr","act_art","act_cad","act_int",
              "act_emp","act_ouv","act_cho","proprio","locatai","mfuel",
              "men_seul","men_coupae","men_coupse","men_monop",
              "modtrans_aucun","modtrans_pied","modtrans_velo",
              "modtrans_moto","modtrans_voit","modtrans_commun",
              "tx_pauvrete60_diff","nivvie_median_diff","acc_ecole",
              "acc_college","acc_lycee","acc_medecin","acc_dentiste",
              "acc_pharmacie")

dtaf_acp <- dtaf[, vars_acp]
dtaf_acp %>% st_drop_geometry()-> dtaf_acp

res.pca <- PCA(dtaf_acp, scale.unit = TRUE, graph = TRUE)

# 6. Visualisations avec factoextra

# Éboulis (valeurs propres)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Cercle des corrélations (variables)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("blue", "orange", "red"),
             repel = TRUE)

# Carte des individus
fviz_pca_ind(res.pca, geom.ind = "point",
             col.ind = "cos2", # qualité de représentation
             gradient.cols = c("blue", "orange", "red"),
             repel = TRUE)

# Contribution des variables à la dimension 1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 15)

# Contribution des variables à la dimension 2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 15)

str(dtaf)

# les votes pour macron

# Lancer l'ACP uniquement sur les variables structurelles
res.pca <- PCA(dtaf_acp, scale.unit = TRUE, graph = FALSE)

# Ajouter la couleur des individus selon le % de Macron
fviz_pca_ind(res.pca, geom.ind = "point",
             col.ind = dtaf$Macron_exp, # variable pour la couleur
             gradient.cols = c("blue", "white", "red"),
             repel = TRUE) +
  ggtitle("ACP - individus colorés selon le vote Macron (%)")

## essai
vars_vote <- c("Abs_insc","Vot_insc","Blanc_vote","Nul_vote",
               "Arthaud_exp","Roussel_exp","Macron_exp",
               "Lassalle_exp","LePen_exp","Zemmour_exp",
               "Melenchon_exp",
               "Hidalgo_exp",
               "Jadot_exp",
               "Pecresse_exp" ,
               "Poutou_exp",
               "DupontAignan_exp") 
# tu ajoutes tous les candidats disponibles dans ton df

# ----------------------
# 3. Nettoyage du dataset (supprimer geometry sf)
# ----------------------
dtaf_acp <- dtaf %>% st_drop_geometry()

#----------------
# 5. Sélectionner les données pour l’ACP
# ----------------------
dtaf_acp <- dtaf_acp[, vars_acp]

# ----------------------
# 6. Lancer l’ACP
# ----------------------

# on retire les variables trop corrélées
cor_mat <- cor(dtaf_acp, use = "pairwise.complete.obs")
library(corrplot)
corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.6)

library(caret)
high_cor <- findCorrelation(cor_mat, cutoff = 0.9)
dtaf_acp <- dtaf_acp[, -high_cor]

res.pca <- PCA(dtaf_acp, scale.unit = TRUE, graph = T)

# ----------------------
# 7. Visualiser : individus colorés par "vainqueur"
# ----------------------
fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = dtaf$Gagnant,   # couleur = vainqueur
             palette = "jco",
             legend.title = "Vote majoritaire") +
  ggtitle("ACP - Individus colorés selon le vote majoritaire")

fviz_pca_var(res.pca,repel = TRUE) +
  ggtitle("ACP - variables")

# vérifier que % d'inertie significatif
# essai arbre

library(rpart)
library(dplyr)
library(visNetwork)
library(rpart.plot)
library(sparkline)

data = dtaf %>%select(Inscrit_22,pop_légal_19,pop_pole_aav,pop_cour_aav,
                      pop_horsaav,pop_urb,pop_rur_periu,pop_rur_non_periu,age_moyen,
                      actemp,actcho,inactret,actdip_PEU,actdip_CAP,actdip_BAC,actdip_BAC2,
                      actdip_BAC3,actdip_BAC5,actdip_BAC3P,
                      act_agr,act_art,act_cad,act_int,act_emp,act_ouv,act_cho,
                      men_seul,men_coupae,men_coupse,men_monop,proprio,locatai,mfuel,
                      modtrans_aucun,modtrans_pied,modtrans_velo,modtrans_voit,modtrans_commun,
                      acc_ecole,acc_college,acc_lycee,acc_medecin,acc_dentiste,acc_pharmacie,
                      tx_pauvrete60_diff,nivvie_median_diff,Jadot_exp) %>%
  st_drop_geometry()
# Basic classification tree
res <- rpart(Gagnant~., data = data)
visTree(res, main = "Vote", width = "100%",height = "600px")

res_jadot <- rpart(Jadot_exp~., data = data)
visTree(res_jadot, main = "Vote", width = "100%",height = "600px")

# renommer variables + ajuster ecriture