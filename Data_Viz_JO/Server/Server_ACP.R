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
