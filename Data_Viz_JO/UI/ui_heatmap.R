tagList(
  fluidRow(
    column(
      width = 10, offset = 1,

      h4("Corrélation entre les candidats et les variables socio-éco-démographiques",
         align = "center"),

      selectInput(
        inputId = "themes",
        label   = "Choisissez un thème à afficher sur la heatmap : ",
        choices = c(
          "Démographie"                         = "demo",
          "Activité / emploi"                   = "activ",
          "Diplômes"                            = "dipl",
          "Catégories socio-professionnelles"   = "csp",
          "Logement / énergie"                  = "logt",
          "Ménages"                             = "men",
          "Mobilités"                           = "mob",
          "Pauvreté & niveau de vie"            = "nivvie",
          "Accès aux services"                  = "acc"
        ),
        selected = c("demo","activ","dipl","csp","logt","men","mob","nivvie","acc")
      ),

      plotlyOutput("heatmapcorr", height = "400px")
    )
  )
)
