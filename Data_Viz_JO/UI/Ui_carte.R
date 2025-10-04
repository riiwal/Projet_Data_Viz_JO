library(shiny)
tagList(fluidPage(fluidRow(column(
  width = 12,
  offset = 1,
  h1("Profil électoral et démographique par circonscription ", align = "center") #Titre du graphique
)), fluidRow(
  column(
    3,
    selectInput( #Liste de choix pour la coloration de la carte
      "select",
      "Choisissez une variable à afficher sur la carte :",
      choices = list(
        "Population" = list(
          "% de Population pôles AAV" = "pop_pole_aav",
          "% de Population couronnes AAV" = "pop_cour_aav",
          "% de Population hors AAV" = "pop_horsaav",
          "% de Population urbaine" = "pop_urb",
          "% de Population rurale périurbaine" = "pop_rur_periu",
          "% de Population rurale non périurbaine" = "pop_rur_non_periu"
        ),
        "Emploi et diplômes" = list(
          "% d'Actifs en emploi" = "actemp",
          "% de Chômeurs" = "actcho",
          "% d'Inactifs retraités" = "inactret",
          "% de Diplômé < CAP" = "actdip_PEU",
          "% de CAP/BEP" = "actdip_CAP",
          "% de Diplomé du Baccalauréat" = "actdip_BAC",
          "% de Diplomé d'un Bac+2" = "actdip_BAC2",
          "% de Diplomé d'un Bac+3" = "actdip_BAC3",
          "% de Diplomé d'un Bac+5" = "actdip_BAC5"
          
        ),
        "Professions" = list(
          "% d'Agriculteurs" = "act_agr",
          "% d'Artisans/commerçants" = "act_art",
          "% de Cadres" = "act_cad",
          "% de Professions intermédiaires" = "act_int",
          "% d'Employés" = "act_emp",
          "% d'Ouvriers" = "act_ouv",
          "% de Chômeurs" = "act_cho"
        ),
        "Logement" = list("% Propriétaires" = "proprio", "% Locataires" = "locatai"),
        "Ménages" = list(
          "% de Ménages seuls" = "men_seul",
          "% de Couples avec enfants" = "men_coupae",
          "% de Couples sans enfants" = "men_coupse",
          "% de Familles monoparentales" = "men_monop"
        ),
        "Transport majoritairement utilisé pour aller au travail" = list(
          "% n'utilisant aucun transport"    = "modtrans_aucun",
          "% se déplaçant à pied"           = "modtrans_pied",
          "% se déplaçant à vélo"           = "modtrans_velo",
          "% se déplaçant en moto"          = "modtrans_moto",
          "% se déplaçant en voiture"       = "modtrans_voit",
          "% utilisant les transports en commun" = "modtrans_commun"
        ),
        "Revenus et pauvreté" = list("Taux pauvreté (seuil 60% du niveau de vie médian)" = "tx_pauvrete60_diff"),
        "Accès aux services sur la commune de résidence" = list(
          "% ayant accès à une école"     = "acc_ecole",
          "% ayant accès à un collège"    = "acc_college",
          "% ayant accès à un lycée"      = "acc_lycee",
          "% ayant accès à un médecin"    = "acc_medecin",
          "% ayant accès à un dentiste"   = "acc_dentiste",
          "% ayant accès à une pharmacie" = "acc_pharmacie"
          
        )
      )
    ),
    h3("AAV = Aire d'attraction des villes", style = "font-size:12px;") #Légende
  ), column(9, leafletOutput("mymap", height = 550), ) #Carte
)))
