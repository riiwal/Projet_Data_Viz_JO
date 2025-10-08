# UI global

shinyUI(navbarPage(title = "Présidentielles 2022", theme = theme_presidentielles,
             tabPanel(
               title = "Accueil",
               plotOutput("image_fond_accueil", width = "100%", height = "700px")
             ),
             # Réference autres UI
             tabPanel(title = "Données",  source("UI/UI_data_summ.R", local = TRUE)$value),
             tabPanel(title = "Analyse Descriptive", source("ui/ui_descriptif.R", local = TRUE)$value),
             tabPanel(title = "Carte",   source("UI/Ui_carte.R", local = TRUE)$value),
             tabPanel(title = "ACP",     source("UI/Ui_ACP.R",   local = TRUE)$value),
             tabPanel(title = "Arbre",   source("UI/Ui_arbre.R", local = TRUE)$value),
             tabPanel(title = "Heatmap", source("ui/ui_heatmap.R", local = TRUE)$value)
  )
)
