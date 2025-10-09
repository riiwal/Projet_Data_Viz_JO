# page données
tagList(navset_card_tab( # pour englober le DF
  fluidRow(
    DTOutput("table", # sortie
             height = 550) # hauteur
  ))
)