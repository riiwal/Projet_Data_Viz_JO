library(shiny)
tagList(fluidPage(fluidRow(column(
  width = 12,
  offset = 1,
  h1("Carte ", align = "center")
)), fluidRow(
  column(
    3,
    selectInput(
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
        "Logement" = list(
          "% Propriétaires" = "proprio",
          "% Locataires" = "locatai",
          "Mode chauffage : fioul" = "mfuel"
        ),
        "Ménages" = list(
          "% de Ménages seuls" = "men_seul",
          "% de Couples avec enfants" = "men_coupae",
          "% de Couples sans enfants" = "men_coupse",
          "% de Familles monoparentales" = "men_monop"
        ),
        "Transport majoritairement utilisé pour aller au travail" = list(
          "Transport : aucun" = "modtrans_aucun",
          "Transport : à pied" = "modtrans_pied",
          "Transport : vélo" = "modtrans_velo",
          "Transport : moto" = "modtrans_moto",
          "Transport : voiture" = "modtrans_voit",
          "Transport : commun" = "modtrans_commun"
        ),
        "Revenus et pauvreté" = list(
          "Taux pauvreté (60%)" = "tx_pauvrete60_diff",
          "Niveau de vie médian" = "nivvie_median_diff"
        ),
        "Accès aux services sur la commune de résidence" = list(
          "Accès école" = "acc_ecole",
          "Accès collège" = "acc_college",
          "Accès lycée" = "acc_lycee",
          "Accès médecin" = "acc_medecin",
          "Accès dentiste" = "acc_dentiste",
          "Accès pharmacie" = "acc_pharmacie"
        )
      )
    )), column(9, leafletOutput("mymap", height = 550), )
)))
