library(shiny)
library(DT)
library(dplyr)
library(stargazer)
library(summarytools)


  # création des thèmes pour l'affichage des variables sélectionnées dans le summary
  # theme_vars <- list(
  #   "Démographie" = c("Inscrit_22","pop_légal_19","pop_pole_aav","pop_cour_aav","pop_horsaav","pop_urb","pop_rur_periu","pop_rur_non_periu","age_moyen"),
  #   "Activité" = c("actemp","actcho","inactret"),
  #   "Education"=c("actdip_PEU","actdip_CAP","actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5","actdip_BAC3P"),
  #   "CSP"= c("act_agr","act_art","act_cad","act_int","act_emp","act_ouv","act_cho"),
  #   "Menages" = c("men_seul","men_coupea","men_coupse","men_monop"),
  #   "Logement" = c("proprio","locatai","mfuel"),
  #   "Transport" = c("modtrans_aucun","modtrans_pied","modtrans_velo","modtrans_voit","modtrans_commun"),
  #   "Accès" = c("acc_ecole","acc_college","acc_lycee","acc_medecin","acc_dentiste","acc_pharmacie"),
  #   "Economie" = c("tx_pauvrete60_diff","nivvie_median_diff"),
  #   "Votes" = c("Abs_insc","Vot_insc","Blanc_vote","Nul_vote","Arthaud_exp","Roussel_exp","Macron_exp","Lassalle_exp","LePen_exp","Zemmour_exp",
  #               "Melenchon_exp","Hidalgo_exp","Jadot_exp","Pecresse_exp","Poutou_exp","DupontAignan_exp","Gagnant")
  # )
dtaf_loaded_dt <- dtaf_loaded %>%
  st_drop_geometry() %>%
  rename(
    "Code du département" = codeDepartement,
    "Nom du département" = nomDepartement,
    "Code de la circonscription" = codeCirconscription,
    "Nom de la circonscription" = nomCirconscription,
    "Inscrits 2022" = Inscrit_22,
    "Population légale 2019" = pop_légal_19,
    "% pop. pôles AAV" = pop_pole_aav,
    "% pop. couronnes AAV" = pop_cour_aav,
    "% pop. hors AAV" = pop_horsaav,
    "% pop. urbaine" = pop_urb,
    "% pop. rurale périurbaine" = pop_rur_periu,
    "% pop. rurale non périurbaine" = pop_rur_non_periu,
    "Âge moyen" = age_moyen,
    "% actifs en emploi" = actemp,
    "% chômeurs" = actcho,
    "% inactifs retraités" = inactret,
    "Sans diplôme" = actdip_PEU,
    "CAP-BEP" = actdip_CAP,
    "BAC" = actdip_BAC,
    "Bac+2" = actdip_BAC2,
    "Bac+3" = actdip_BAC3,
    "Bac+5" = actdip_BAC5,
    "Bac+3+" = actdip_BAC3P,
    "% agriculteurs" = act_agr,
    "% artisans/com." = act_art,
    "% cadres" = act_cad,
    "% prof. intermédiaires" = act_int,
    "% employés" = act_emp,
    "% ouvriers" = act_ouv,
    "% chômage" = act_cho,
    "% ménages seuls" = men_seul,
    "% couples avec enfants" = men_coupae,
    "% couples sans enfants" = men_coupse,
    "% familles monoparentales" = men_monop,
    "% propriétaires" = proprio,
    "% locataires" = locatai,
    "% chauffage fioul" = mfuel,
    "% aucun transport" = modtrans_aucun,
    "% à pied" = modtrans_pied,
    "% vélo" = modtrans_velo,
    "% moto" = modtrans_moto,
    "% voiture" = modtrans_voit,
    "% transports en commun" = modtrans_commun,
    "% accès école" = acc_ecole,
    "% accès collège" = acc_college,
    "% accès lycée" = acc_lycee,
    "% accès médecin" = acc_medecin,
    "% accès dentiste" = acc_dentiste,
    "% accès pharmacie" = acc_pharmacie,
    "Taux de pauvreté (60%)" = tx_pauvrete60_diff,
    "Niveau de vie médian" = nivvie_median_diff,
    "Vote majoritaire" = Gagnant,
    "Taux d'abstension" = Abs_insc,
    "Taux de vote" = Vot_insc,
    "% Votes blans" = Blanc_vote,
    "% Votes nuls" = Nul_vote,
    "Votes exprimés pour Arthaud" = Arthaud_exp,
    "Votes exprimés pour Roussel" = Roussel_exp,
    "Votes exprimés pour Macron" = Macron_exp,
    "Votes exprimés pour Lasalle" = Lassalle_exp,
    "Votes exprimés pour Le Pen" = LePen_exp,
    "Votes exprimés pour Zemmour" = Zemmour_exp,
    "Votes exprimés pour Mélenchon" = Melenchon_exp,
    "Votes exprimés pour Hidalgo" = Hidalgo_exp,
    "Votes exprimés pour Jadot" = Jadot_exp,
    "Votes exprimés pour Pécresse" = Pecresse_exp,
    "Votes exprimés pour Poutou" = Poutou_exp,
    "Votes exprimés pour Dupont-Aignan" = DupontAignan_exp
  )

  # dtaf_loaded_summary <- dtaf_loaded %>%
  #   st_drop_geometry()
  # names(dtaf_loaded_summary) <- label_variable_df[names(dtaf_loaded_summary)]
  # Table affichée
  output$table <- renderDT({
    DT::datatable(dtaf_loaded_dt, options = list(pageLength = 10))
  })
  
  # Résumé des variables
  library(stargazer)
  
  output$summary <- renderUI({
    df <- as.data.frame(summary(dtaf_loaded_dt[, input$select_summary, drop = FALSE]))
    HTML(
      stargazer(df, type = "html", summary = FALSE, title = "Résumé statistique")
    )
  })
  
