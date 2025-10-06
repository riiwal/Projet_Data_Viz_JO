# essai arbre

library(rpart)
library(dplyr)
library(visNetwork)
library(rpart.plot)
library(sparkline)
library(shinyWidgets)

# server/Server_arbre.R
res <- rpart(Gagnant~., data = dtaf %>%select(Inscrit_22,pop_légal_19,pop_pole_aav,pop_cour_aav,
                                              pop_horsaav,pop_urb,pop_rur_periu,pop_rur_non_periu,age_moyen,
                                              actemp,actcho,inactret,actdip_PEU,actdip_CAP,actdip_BAC,actdip_BAC2,
                                              actdip_BAC3,actdip_BAC5,actdip_BAC3P,
                                              act_agr,act_art,act_cad,act_int,act_emp,act_ouv,act_cho,
                                              men_seul,men_coupae,men_coupse,men_monop,proprio,locatai,mfuel,
                                              modtrans_aucun,modtrans_pied,modtrans_velo,modtrans_voit,modtrans_commun,
                                              acc_ecole,acc_college,acc_lycee,acc_medecin,acc_dentiste,acc_pharmacie,
                                              tx_pauvrete60_diff,nivvie_median_diff,Gagnant) %>%
               st_drop_geometry())
output$tree <- renderVisNetwork({
  visTree(res, main = "Vote", width = "100%", height = "100vh")
}) 

output$tree_cand <- renderVisNetwork({
  
  #arbre candidat
  
  res_cand <- rpart(as.formula(paste(input$cand, "~ .")),dtaf %>%
                      st_drop_geometry() %>%
                      select(
                        Inscrit_22, pop_légal_19, pop_pole_aav, pop_cour_aav, pop_horsaav, pop_urb,
                        pop_rur_periu, pop_rur_non_periu, age_moyen,
                        actemp, actcho, inactret, actdip_PEU, actdip_CAP, actdip_BAC, actdip_BAC2,
                        actdip_BAC3, actdip_BAC5, actdip_BAC3P,
                        act_agr, act_art, act_cad, act_int, act_emp, act_ouv, act_cho,
                        men_seul, men_coupae, men_coupse, men_monop, proprio, locatai, mfuel,
                        modtrans_aucun, modtrans_pied, modtrans_velo, modtrans_voit, modtrans_commun,
                        acc_ecole, acc_college, acc_lycee, acc_medecin, acc_dentiste, acc_pharmacie,
                        tx_pauvrete60_diff, nivvie_median_diff, all_of(input$cand))
  )
  visTree(res_cand, main = paste0("Votes pour ", names(which(c(
    "Nathalie Arthaud" = "Arthaud_exp",
    "Fabrice Roussel" = "Roussel_exp",
    "Emmanuel Macron" = "Macron_exp",
    "Jean Lassalle" = "Lassalle_exp",
    "Marine Le Pen" = "LePen_exp",
    "Eric Zemmour" = "Zemmour_exp",
    "Jean-Luc Mélenchon" = "Melenchon_exp",
    "Anne Hidalgo" = "Hidalgo_exp",
    "Yannick Jadot" = "Jadot_exp",
    "Valérie Pécresse" = "Pecresse_exp",
    "Philippe Poutou" = "Poutou_exp",
    "Nicolas Dupont-Aignan" = "DupontAignan_exp"
  ) == input$cand))), width = "100%",height = "600px")
})
  # renommer variables + ajuster ecriture