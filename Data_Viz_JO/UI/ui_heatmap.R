tagList(
  selectInput(
    inputId = "themes",
    label   = "Thèmes socio-éco",
    choices = c(
      "Démographie"              = "demo",
      "Activité / emploi"        = "activ",
      "Diplômes"                 = "dipl",
      "Catégories socio-professionnelle"     = "csp",
      "Logement / énergie"       = "logt",
      "Ménages"                  = "men",
      "Mobilités"                = "mob",
      "Pauvreté & niveau de vie" = "nivvie",
      "Accès aux services"       = "acc"
    ),
    selected = c("demo","activ","dipl","csp","logt","men","mob","nivvie","acc")
  ),  plotlyOutput("heatmapcorr", height = 500))
  