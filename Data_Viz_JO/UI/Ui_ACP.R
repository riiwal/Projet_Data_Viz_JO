# Page ACP

tagList(
  fluidRow(
    column(
      6,
      card(class = "mycard",
          h4 ("ACP - Individus colorés selon le vote majoritaire"), # titre
           card_body(plotOutput("PCA_ind"))) # graphe des individus
    ),
    column(
      6,
      card(class = "mycard",
           h4("ACP - Variables"), # titre
           card_body(plotOutput("PCA_var"))) # graphe des variables
    )))