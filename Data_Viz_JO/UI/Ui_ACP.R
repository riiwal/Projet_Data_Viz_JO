# Page ACP

tagList(
  fluidRow(
    column(
      6,
      card(class = "mycard",
           card_header("Individus"), card_body(plotOutput("PCA_ind"))) # graphe des individus
    ),
    column(
      6,
      card(class = "mycard",
           card_header("Varibles"),
           card_body(plotOutput("PCA_var"))) # graphe des variables
    )))