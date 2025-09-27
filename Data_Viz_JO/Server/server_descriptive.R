output$cartegagnant <- renderPlot({
  dtaf <- dtaf_loaded
  
  dtaf <- dtaf %>%
    mutate(
      Gagnant = factor(Gagnant, levels = c("MACRON", "LE PEN", "MÉLENCHON")),
      codeDepartement = as.character(codeDepartement)
    )
  
  dep <- dtaf %>%
    group_by(codeDepartement) %>%
    summarise() %>%
    ungroup()
  
  cols <- c(
    "MACRON" = "#F2C14E",
    "LE PEN" = "#0D3B66",
    "MÉLENCHON" = "#D7263D"
  )
  
  ggplot(dtaf) +
    geom_sf(
      aes(fill = Gagnant),
      color = "white",
      size = 0.1,
      linewidth = 0.1
    ) +
    geom_sf(
      data = dep,
      fill = NA,
      color = "white",
      size = 0.25,
      linewidth = 0.25
    ) +
    scale_fill_manual(values = cols,
                      na.value = "grey85",
                      drop = FALSE) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank()
    )
})

output$carteabstention <- renderPlot({
  ggplot(dtaf) +
    geom_sf(aes(fill = Abs_insc),
            color = "white",
            size = 0.1) +
    geom_sf(
      data = dep,
      fill = NA,
      color = "white",
      size = 0.2
    ) + scale_fill_viridis_c(option = "magma", direction = -1) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
})

output$histogrammeresultat <- renderPlot({
  cand_map <- c(
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
  
  dtaf_sum <- dtaf_loaded %>%
    mutate(
      inscrits          = Inscrit_22,
      part_prop         = Vot_insc / 100,
      blancs_prop       = Blanc_vote / 100,
      nuls_prop         = Nul_vote / 100,
      exprimes          = inscrits * part_prop * (1 - blancs_prop - nuls_prop)
    ) %>%
    select(all_of(unname(cand_map)), exprimes)
  
  nat_long <- dtaf_sum %>%
    pivot_longer(cols = all_of(unname(cand_map)),
                 names_to = "var",
                 values_to = "pct_exp") %>%
    mutate(votes_cand = exprimes * (pct_exp / 100)) %>%
    group_by(var) %>%
    summarise(votes = sum(votes_cand, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(share = votes / sum(votes),
           candidat = names(cand_map)[match(var, unname(cand_map))])
  
  cols <- c(
    "Emmanuel\nMacron"      = "#F2C14E",
    "Marine\nLe Pen"        = "#223A77",
    "Jean-Luc\nMélenchon"   = "#D7263D",
    "Éric\nZemmour"         = "#6B5C89",
    "Valérie\nPécresse"     = "#5C6BC0",
    "Yannick\nJadot"        = "#6DA34D",
    "Jean\nLassalle"        = "#7D2941",
    "Fabien\nRoussel"       = "#E64A19",
    "Nicolas\nDupont-Aignan" = "#7A7B86",
    "Anne\nHidalgo"         = "#D97B8B",
    "Philippe\nPoutou"      = "#8A2F2A",
    "Nathalie\nArthaud"     = "#6E3E3B" )
  
  nat_long <- nat_long %>%
    arrange(desc(share)) %>%
    mutate(candidat = factor(candidat, levels = candidat))
  
  ggplot(nat_long, aes(x = candidat, y = share, fill = candidat)) +
    geom_col(width = 0.85) +
    geom_text(
      aes(label = percent(share, accuracy = 0.01)),
      vjust = -0.3,
      size = 3.8,
      fontface = "bold"
    ) +
    scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
    scale_fill_manual(values = cols, guide = "none") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(lineheight = 0.95, margin = margin(t = 6)),
      plot.margin        = margin(10, 20, 20, 10)
    )
  
})
