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