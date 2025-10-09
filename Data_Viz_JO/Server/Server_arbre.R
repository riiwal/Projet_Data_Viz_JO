# Server arbre

# liste pour renommer les variables dans les arbres
label_variable <- reactive({ c(Inscrit_22 = "Inscrits 2022",pop_légal_19 = "Population légale 2019",
  pop_pole_aav = "% pop. pôles AAV", pop_cour_aav = "% pop. couronnes AAV", pop_horsaav = "% pop. hors AAV",
  pop_urb = "% pop. urbaine", pop_rur_periu = "% pop. rurale périurbaine", pop_rur_non_periu = "% pop. rurale non périurbaine",
  age_moyen = "Âge moyen",actemp = "% actifs en emploi", actcho = "% chômeurs", inactret = "% inactifs retraités",
  actdip_PEU = "Sans diplôme", actdip_CAP = "CAP-BEP", actdip_BAC = "BAC", actdip_BAC2 = "Bac+2", actdip_BAC3 = "Bac+3",
  actdip_BAC5 = "Bac+5", actdip_BAC3P = "Bac+3+", act_agr = "% agriculteurs", act_art = "% artisans/com.",
  act_cad = "% cadres", act_int = "% prof. intermédiaires", act_emp = "% employés", act_ouv = "% ouvriers", act_cho = "% chômage",
  men_seul = "% ménages seuls", men_coupae = "% couples avec enfants", men_coupse = "% couples sans enfants",
  men_monop = "% familles monoparentales", proprio = "% propriétaires", locatai = "% locataires", mfuel = "% chauffage fioul",
  modtrans_aucun = "% aucun transport", modtrans_pied = "% à pied", modtrans_velo = "% vélo", modtrans_voit = "% voiture",
  modtrans_commun = "% transports en commun", acc_ecole = "% accès école", acc_college = "% accès collège",
  acc_lycee = "% accès lycée", acc_medecin = "% accès médecin", acc_dentiste = "% accès dentiste", acc_pharmacie = "% accès pharmacie",
  tx_pauvrete60_diff = "Taux de pauvreté (60%)", nivvie_median_diff = "Niveau de vie médian", Gagnant = "Vote majoritaire"
)})

# Sélection des variables socio-démo + Gagnant
tree_all <- reactive({dtaf_loaded_tree <- dtaf_loaded %>%
  select(Inscrit_22,pop_légal_19,pop_pole_aav,pop_cour_aav,pop_horsaav,pop_urb,pop_rur_periu,pop_rur_non_periu,
         age_moyen,actemp,actcho,inactret,actdip_PEU,actdip_CAP,actdip_BAC,actdip_BAC2,actdip_BAC3,actdip_BAC5,
         actdip_BAC3P,act_agr,act_art,act_cad,act_int,act_emp,act_ouv,act_cho,men_seul,men_coupae,men_coupse,
         men_monop,proprio,locatai,mfuel,modtrans_aucun,modtrans_pied,modtrans_velo,modtrans_voit,modtrans_commun,
         acc_ecole,acc_college,acc_lycee,acc_medecin,acc_dentiste,acc_pharmacie,tx_pauvrete60_diff,
         nivvie_median_diff,Gagnant) %>%
  st_drop_geometry()# on retire la géométrie de la carte

# Renommage des variables avec les labels
names(dtaf_loaded_tree) <- label_variable()[names(dtaf_loaded_tree)]

# arbre
res <- rpart(`Vote majoritaire` ~ .,
             data = dtaf_loaded_tree)
visTree(res, main = "Vote majoritaire dans la circonscription", # titre
        width = "100%", height = "100vh") # taille de la visu
})

# création de l'output
output$tree <- renderVisNetwork({
  tree_all()
})

#arbre par candidat

# selection var socio-démo + candidat sélectionné dans le menu déroulant de l'UI
tree_candidat <- reactive({dtaf_loaded_tree_cand <- dtaf_loaded %>%
  select(Inscrit_22,pop_légal_19,pop_pole_aav,pop_cour_aav,pop_horsaav,pop_urb,pop_rur_periu,pop_rur_non_periu,
         age_moyen,actemp,actcho,inactret,actdip_PEU,actdip_CAP,actdip_BAC,actdip_BAC2,actdip_BAC3,actdip_BAC5,
         actdip_BAC3P,act_agr,act_art,act_cad,act_int,act_emp,act_ouv,act_cho,men_seul,men_coupae,men_coupse,
         men_monop,proprio,locatai,mfuel,modtrans_aucun,modtrans_pied,modtrans_velo,modtrans_voit,modtrans_commun,
         acc_ecole,acc_college,acc_lycee,acc_medecin,acc_dentiste,acc_pharmacie,tx_pauvrete60_diff,
         nivvie_median_diff, all_of(input$cand)) %>%
  st_drop_geometry() # on retire la géométrie de la carte

# Renommage des variables avec les labels
names(dtaf_loaded_tree_cand) <- c(
  label_variable()[names(dtaf_loaded_tree_cand)[names(dtaf_loaded_tree_cand) != input$cand]],# on renomme tout sauf la variable du candidat
  input$cand)

# arbre
res_cand <- rpart(as.formula(paste(input$cand, "~ .")),
                  data = dtaf_loaded_tree_cand)

})

# output
output$tree_cand <- renderVisNetwork({
  visTree(tree_candidat(), 
          main = paste0("Votes pour ", names(which(c( # paramétrage pour titre intéractif selon le candidat choisi
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
            "Nicolas Dupont-Aignan" = "DupontAignan_exp") == input$cand))),
          width = "100%",height = "600px") # taille de la visu
})