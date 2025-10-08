# Server global

shinyServer(function(input, output, session) {
  output$image_fond_accueil <- renderPlot({
    img <- readPNG(file.path("image_urne_fond_2.png"))
    
    # Plein écran
    op <- par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i"); on.exit(par(op), add = TRUE)
    plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
    rasterImage(img, 0, 0, 1, 1)
    
    # positions
    x        <- 0.55   
    y_titre  <- 0.70   
    y_sous_titre    <- 0.63   
    y_legende    <- 0.25   
    
    # Ombrage
    sh <- function(tx, x, y, cex=1, font=1) {
      text(x+0.002, y-0.002, tx, col = adjustcolor("black", 0.6),
           cex = cex, font = font, adj = c(0, .5))
    }
    
    # Titre
    sh("Présidentielles 2022", x, y_titre, cex = 4, font = 2)
    text(x, y_titre, "Présidentielles 2022", col = "white",
         cex = 4, font = 2, adj = c(0, .5))
    
    # Sous-titre
    sh("Analyse & visualisations de données", x+0.03, y_sous_titre, cex = 2)
    text(x+0.03, y_sous_titre, "Analyse & visualisations de données",
         col = "white", cex = 2, adj = c(0, .5))
    
    # Panneau transparent
    rect(x+0.04, y_legende-0.06, 0.90, y_legende+0.03,
         col = adjustcolor("black", 0.20), border = NA,
         angle = 30) 
    
    # Légende
    sh("Projet réalisé par :", x+0.15, y_legende, cex = 1.1)
    text(x+0.15, y_legende, "Projet réalisé par :", col = "white",
         cex = 1.1, adj = c(0, .5))
    
    # Légende Nom
    sh("Riwal Le Moan--Delalande, Maéva Montier, Melina Clément", x+0.05, y_legende-0.03, cex = 1.1)
    text(x+0.05, y_legende-0.03, "Riwal Le Moan--Delalande, Maéva Montier, Melina Clément", col = "white",
         cex = 1.1, adj = c(0, .5))
  }, bg = "black")
  
  # Référence autres servers
  source("server/Server_data_summ.R",local = TRUE)
  source("server/server_descriptive.R", local = TRUE)
  source('Server/Server_carte.R',local=TRUE)
  source("Server/Server_ACP.R",local = TRUE)
  source("Server/Server_arbre.R",local = TRUE)
  source("server/server_heatmap.R",local = TRUE)
})
