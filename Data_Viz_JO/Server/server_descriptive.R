### Carte de participation ####

output$carteparticipation <- renderPlot({
  
  ggplot(dtaf_base) +
    geom_sf(aes(fill = Vot_insc), # graphique des circonscription
            color = "white",
            size = 0.2) +
    scale_fill_viridis_c(name = "Participation (%)",
                         option = "viridis",
                         direction = -1) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    labs(title = "Participation par circonscription", x = NULL, y = NULL) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title.position = "plot",
      plot.title  = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      plot.margin = margin(
        t = 12,
        r = 20,
        b = 10,
        l = 24
      )
    )
})

### Carte des gagnants par circonscription ####

output$cartegagnant <- renderPlot({
  
  # Palette de couleurs 
  colonne <- c(
    "MACRON" = "#F2C14E",
    "LE PEN" = "#0D3B66",
    "MÉLENCHON" = "#D7263D"
  )
  
  ggplot(dtaf_base %>%
           mutate(Gagnant = factor(
             Gagnant, levels = c("MACRON", "LE PEN", "MÉLENCHON")
           ))) +
    geom_sf(aes(fill = Gagnant), color = "white", size = 0.2) +
    scale_fill_manual(
      values = colonne,
      na.value = "grey85",
      drop = FALSE,
      name = "Gagnant"
    ) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    labs(title = "Candidat gagnant par circonscription", x = NULL, y = NULL) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title.position = "plot",
      plot.title  = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      plot.margin = margin(
        t = 12,
        r = 20,
        b = 10,
        l = 24
      )
    )
})

# Histogramme national pondéré par candidat
histo_resultat <- reactive({
  
  # Label des candidats
  candidat <- c(
    "Emmanuel\nMacron"       = "Macron_exp",
    "Marine\nLe Pen"         = "LePen_exp",
    "Jean-Luc\nMélenchon"    = "Melenchon_exp",
    "Éric\nZemmour"          = "Zemmour_exp",
    "Valérie\nPécresse"      = "Pecresse_exp",
    "Yannick\nJadot"         = "Jadot_exp",
    "Jean\nLassalle"         = "Lassalle_exp",
    "Fabien\nRoussel"        = "Roussel_exp",
    "Nicolas\nDupont-Aignan" = "DupontAignan_exp",
    "Anne\nHidalgo"          = "Hidalgo_exp",
    "Philippe\nPoutou"       = "Poutou_exp",
    "Nathalie\nArthaud"      = "Arthaud_exp"
  )
  
  # Vecteur avec les noms des colonnes qui correspondent aux candidats
  cols <- unname(candidat)
  
  # Enlever la géométrie et garder les colonnes
  df <- st_drop_geometry(dtaf_base[, c("Inscrit_22","Vot_insc","Blanc_vote","Nul_vote", cols)])
  
  # Matrice des pourcentages des candidats 
  pct_mat <- as.matrix(as.data.frame(df[cols]))
  
  # Calcul du nombre de suffrages exprimés par circonscription
  expr <- with(df, Inscrit_22 * (Vot_insc/100) * (1 - Blanc_vote/100 - Nul_vote/100))
  
  # Votes par candidat 
  votes <- colSums(pct_mat/100 * expr)
  
  
  pourcentage_candidat <- tibble(
    var   = names(votes),
    votes = as.numeric(votes)
  ) %>% 
    mutate(
      vote     = votes / sum(votes),
      candidat = names(candidat)[match(var, cols)]
    ) %>% 
    arrange(desc(vote)) %>% 
    mutate(candidat = factor(candidat, levels = candidat))
  
  # Palette
  couleur_parti <- c(
    "Emmanuel\nMacron"       = "#F2C14E",
    "Marine\nLe Pen"         = "#223A77",
    "Jean-Luc\nMélenchon"    = "#D7263D",
    "Éric\nZemmour"          = "#6B5C89",
    "Valérie\nPécresse"      = "#5C6BC0",
    "Yannick\nJadot"         = "#6DA34D",
    "Jean\nLassalle"         = "#7D2941",
    "Fabien\nRoussel"        = "#E64A19",
    "Nicolas\nDupont-Aignan" = "#7A7B86",
    "Anne\nHidalgo"          = "#D97B8B",
    "Philippe\nPoutou"       = "#8A2F2A",
    "Nathalie\nArthaud"      = "#6E3E3B"
  )
  
  p <- ggplot(pourcentage_candidat, aes(x = candidat, y = vote, fill = candidat)) +
    geom_col(width = 0.85) +
    geom_text(aes(label = percent(vote, accuracy = 0.01)),
              vjust = -0.3, size = 3.8, fontface = "bold") +
    scale_y_continuous(labels = label_percent(accuracy = 1),
                       expand = expansion(mult = c(0, 0.08))) +
    scale_fill_manual(values = couleur_parti, guide = "none") +
    labs(title = "Résultat des élections présidentielles au 1er tour", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 11, face = "bold",
                                        lineheight = 0.95, margin = margin(t = 6)),
      plot.margin        = margin(t = 14, r = 18, b = 12, l = 28),
      plot.title         = element_text(hjust = 0.5, size = 20, face = "bold")
    )
  
  p
})

output$histogrammeresultat <- renderPlot({histo_resultat()
})

dtaf_histo <- st_drop_geometry(dtaf_base)

share_bar <- function(df,
                      cols,
                      label_map,                 
                      title,
                      weight_col = "pop_légal_19",
                      x_text_size = 12) {
  
  
  
  poids <- df[[weight_col]]
  matrice_ponderee <- as.matrix(df[, cols, drop = FALSE])
 
  shares <- colSums(matrice_ponderee/100 * poids, na.rm = TRUE) / sum(poids, na.rm = TRUE)
  
  tb <- tibble(
    col   = names(shares),
    share = as.numeric(shares),
    label = unname(label_map[col])
  )
  
  # Mettre en descendant
  tb <- tb[order(tb$share, decreasing = TRUE), ]
  tb$label <- factor(tb$label, levels = tb$label)
  
  ggplot(tb, aes(x = label, y = share, fill = label)) +
    geom_col(width = 0.70) +
    geom_text(aes(label = percent(share, accuracy = 0.1)),
                       vjust = -0.30, fontface = "bold") +
    scale_y_continuous(labels = label_percent(),
                                expand = expansion(mult = c(0, .08))) +
   labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
   theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = x_text_size, face = "bold"),
      plot.title         = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position    = "none"
    )
}


output$histogrammehabitat <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("pop_urb","pop_rur_periu","pop_rur_non_periu"),
    label_map = c(
      pop_urb           = "Urbain",
      pop_rur_periu     = "Rural périurbain",
      pop_rur_non_periu = "Rural non périurbain"
    ),
    title = "Répartition par type d’espace"
  )
})

output$histoAAV <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("pop_pole_aav","pop_cour_aav","pop_horsaav"),
    label_map = c(
      pop_pole_aav = "Pôle AAV",
      pop_cour_aav = "Couronne AAV",
      pop_horsaav  = "Hors AAV"
    ),
    title = "Répartition par aire d’attraction (AAV)"
  )
})

output$bar_diplomes <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("actdip_PEU","actdip_CAP","actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5"),
    label_map = c(
      actdip_PEU = "Sans diplôme",
      actdip_CAP = "CAP/BEP",
      actdip_BAC = "BAC",
      actdip_BAC2 = "BAC+2",
      actdip_BAC3 = "BAC+3",
      actdip_BAC5 = "BAC+5"
    ),
    title = "Niveaux de diplôme",
    x_text_size = 11
  )
})

output$bar_csp <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("act_agr","act_art","act_cad","act_int","act_emp","act_ouv","act_cho"),
    label_map = c(
      act_agr = "Agriculteurs",
      act_art = "Artisans/Com.",
      act_cad = "Cadres",
      act_int = "Intermédiaires",
      act_emp = "Employés",
      act_ouv = "Ouvriers",
      act_cho = "Chômeurs"
    ),
    title = "Catégories socio-professionnelles",
    x_text_size = 9
  )
})

output$bar_menages <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("men_seul","men_coupae","men_coupse","men_monop"),
    label_map = c(
      men_seul   = "Personnes \nseules",
      men_coupae = "Couples \navec enfants",
      men_coupse = "Couples \nsans enfants",
      men_monop  = "Familles \nmonoparentales"
    ),
    title = "Types de ménages"
  )
})

output$bar_mobilites <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("modtrans_aucun","modtrans_pied","modtrans_velo","modtrans_moto","modtrans_voit","modtrans_commun"),
    label_map = c(
      modtrans_aucun  = "Aucun",
      modtrans_pied   = "À pied",
      modtrans_velo   = "Vélo",
      modtrans_moto   = "Deux-roues",
      modtrans_voit   = "Voiture",
      modtrans_commun = "Transports \nen commun"
    ),
    title = "Transport domicile-travail principal"
  )
})

output$bar_logement <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("proprio","locatai"),
    label_map = c(
      proprio = "Propriétaires",
      locatai = "Locataires"
    ),
    title = "Statut d’occupation logement"
  )
})

output$bar_acc_education <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("acc_ecole","acc_college","acc_lycee"),
    label_map = c(
      acc_ecole   = "École",
      acc_college = "Collège",
      acc_lycee   = "Lycée"
    ),
    title = "Accessibilité à l’éducation"
  )
})

output$bar_acc_soins <- renderPlot({
  share_bar(
    df = dtaf_histo,
    cols = c("acc_medecin","acc_dentiste","acc_pharmacie"),
    label_map = c(
      acc_medecin  = "Médecin",
      acc_dentiste = "Dentiste",
      acc_pharmacie= "Pharmacie"
    ),
    title = "Accessibilité aux soins"
  )
})
