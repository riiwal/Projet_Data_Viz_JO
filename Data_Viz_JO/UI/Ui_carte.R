library(shiny)
tagList(fluidPage(fluidRow(column(
  width = 12,
  offset = 1,
  h1("Carte de Riwal", align = "center")
)), fluidRow(
  column(
    3,
    selectInput(
      "select",
      "Choisissez une variable à afficher sur la carte :",
      choices = list(
        "Population" = list(
          "Population pôles AAV" = "pop_pole_aav",
          "Population couronnes AAV" = "pop_cour_aav",
          "Population hors AAV" = "pop_horsaav",
          "Population urbaine" = "pop_urb",
          "Population rurale périurbaine" = "pop_rur_periu",
          "Population rurale non périurbaine" = "pop_rur_non_periu",
          "Âge moyen" = "age_moyen"
        ),
        "Emploi et diplômes" = list(
          "Actifs en emploi" = "actemp",
          "Chômage" = "actcho",
          "Inactifs retraités" = "inactret",
          "Diplôme < CAP" = "actdip_PEU",
          "CAP/BEP" = "actdip_CAP",
          "Baccalauréat" = "actdip_BAC",
          "Bac+2" = "actdip_BAC2",
          "Bac+3" = "actdip_BAC3",
          "Bac+5" = "actdip_BAC5",
          "Bac+3 et plus" = "actdip_BAC3P"
        ),
        "Professions" = list(
          "Agriculteurs" = "act_agr",
          "Artisans/commerçants" = "act_art",
          "Cadres" = "act_cad",
          "Professions intermédiaires" = "act_int",
          "Employés" = "act_emp",
          "Ouvriers" = "act_ouv",
          "Chômeurs" = "act_cho"
        ),
        "Logement" = list(
          "% Propriétaires" = "proprio",
          "% Locataires" = "locatai",
          "Mode chauffage : fioul" = "mfuel"
        ),
        "Ménages" = list(
          "Ménages seuls" = "men_seul",
          "Couples avec enfants" = "men_coupae",
          "Couples sans enfants" = "men_coupse",
          "Familles monoparentales" = "men_monop"
        ),
        "Transport" = list(
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
        "Accès aux services" = list(
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
