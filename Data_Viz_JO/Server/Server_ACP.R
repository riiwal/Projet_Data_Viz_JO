library(FactoMineR)
library(factoextra)
library(dplyr)
library(sf)
# dtaf_loaded %>% select("pop_légal_19","pop_pole_aav","pop_cour_aav","pop_horsaav",
#                 "pop_urb","pop_rur_periu","pop_rur_non_periu","age_moyen",
#                 "actemp","actcho","inactret","actdip_PEU","actdip_CAP",
#                 "actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5",
#                 "actdip_BAC3P","act_agr","act_art","act_cad","act_int",
#                 "act_emp","act_ouv","act_cho","proprio","locatai","mfuel",
#                 "men_seul","men_coupae","men_coupse","men_monop",
#                 "modtrans_aucun","modtrans_pied","modtrans_velo",
#                 "modtrans_moto","modtrans_voit","modtrans_commun",
#                 "tx_pauvrete60_diff","nivvie_median_diff","acc_ecole",
#                 "acc_college","acc_lycee","acc_medecin","acc_dentiste",
#                 "acc_pharmacie") %>% st_drop_geometry()-> dtaf_loaded_acp

dtaf_loaded %>% select("acc_lycee","act_ouv" ,"acc_college","pop_urb","locatai","proprio","actdip_BAC3",
                "actdip_BAC3P","actdip_CAP","act_cad","actdip_BAC5","men_coupse") %>% st_drop_geometry()-> dtaf_loaded_acp

res.pca <- PCA(dtaf_loaded_acp, scale.unit = TRUE, graph = FALSE)
cos2_tot <- rowSums(res.pca$var$cos2[,1:2])
sort(cos2_tot, decreasing = TRUE)


# valeurs propres
#fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# les votes pour macron

# fviz_pca_ind(res.pca, geom.ind = "point",
#              col.ind = dtaf_loaded$Macron_exp, # couleur selon le vote pour Macron
#              gradient.cols = c("blue", "white", "red"),
#              repel = TRUE) +
#   ggtitle("ACP - individus colorés selon le vote Macron (%)")

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


# individus colorés par "vainqueur"
# server/Server_arbre.R
output$PCA_ind <- renderPlot({
  fviz_pca_ind(res.pca,
               geom.ind = "point",
               col.ind = dtaf_loaded$Gagnant,   # couleur = vainqueur
               palette = c("#D7263D","#223A77","#F2C14E"),
               legend.title = "Vote majoritaire") +
    ggtitle("ACP - Individus colorés selon le vote majoritaire")
})

output$PCA_var <- renderPlot({
fviz_pca_var(res.pca,repel = TRUE) +
  ggtitle("ACP - variables")

}
)

# vérifier que % d'inertie significatif
