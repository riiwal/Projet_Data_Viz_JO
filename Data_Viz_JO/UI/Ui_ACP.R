# Page ACP

tagList(
  fluidRow(
    column(
      6,
      card(class = "mycard",
           card_header("Individus",font_size = 15), card_body(plotOutput("PCA_ind"))) # graphe des individus
    ),
    column(
      6,
      card(class = "mycard",
           card_header("Variables"),
           card_body(plotOutput("PCA_var"))) # graphe des variables
    )))