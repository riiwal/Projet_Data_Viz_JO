library(FactoMineR)
library(factoextra)


dtaf %>% select("pop_légal_19","pop_pole_aav","pop_cour_aav","pop_horsaav",
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
                "acc_pharmacie") %>% st_drop_geometry()-> dtaf_acp

res.pca <- PCA(dtaf_acp, scale.unit = TRUE, graph = FALSE)

# valeurs propres
#fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# les votes pour macron

# fviz_pca_ind(res.pca, geom.ind = "point",
#              col.ind = dtaf$Macron_exp, # couleur selon le vote pour Macron
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
output$PCA <- renderPlot({
  fviz_pca_ind(res.pca,
               geom.ind = "point",
               col.ind = dtaf$Gagnant,   # couleur = vainqueur
               palette = "jco",
               legend.title = "Vote majoritaire") +
    ggtitle("ACP - Individus colorés selon le vote majoritaire")
  
}
)

fviz_pca_var(res.pca,repel = TRUE) +
  ggtitle("ACP - variables")

# vérifier que % d'inertie significatif
