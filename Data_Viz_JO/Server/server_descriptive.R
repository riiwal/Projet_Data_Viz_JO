# server/server_descriptive.R
output$hist <- renderPlot({
  cand_cols <- grep("_exp$", names(dtaf), value = TRUE)
  
  dat_long <- dtaf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(cand_cols)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Candidat", values_to = "Pourcentage")
  
  nom_candidat <- c(
    "Arthaud_exp"="Arthaud","Roussel_exp"="Roussel","Macron_exp"="Macron",
    "Lassalle_exp"="Lassalle","LePen_exp"="Le Pen","Zemmour_exp"="Zemmour",
    "Melenchon_exp"="Mélenchon","Hidalgo_exp"="Hidalgo","Jadot_exp"="Jadot",
    "Pecresse_exp"="Pécresse","Poutou_exp"="Poutou","DupontAignan_exp"="Dupont-Aignan"
  )
  dat_long <- dplyr::mutate(dat_long, Candidat = dplyr::recode(Candidat, !!!nom_candidat, .default = Candidat))
  
  ggplot2::ggplot(dat_long, ggplot2::aes(
    x = forcats::fct_reorder(Candidat, Pourcentage, .fun = median, .desc = TRUE),
    y = Pourcentage, fill = Candidat
  )) +
    ggplot2::geom_boxplot(width = 0.7, notch = TRUE, outlier.alpha = 0.35, outlier.shape = 21) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "white") +
    ggplot2::scale_y_continuous(limits = c(0, 100), labels = scales::label_number(accuracy = 0.1, suffix = " %")) +
    ggplot2::scale_fill_viridis_d(option = "plasma", direction = -1, guide = "none") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Distribution des pourcentages par candidat",
      subtitle = paste0("n = ", nrow(dtaf), " circonscriptions"),
      x = NULL, y = "Pourcentage"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
})

output$cartegagnant<-renderPlot({
  dtaf <- st_make_valid(dtaf)
  
  
  dtaf <- dtaf %>% 
    mutate(Gagnant = factor(Gagnant, levels = c("MACRON","LE PEN","MÉLENCHON")),
           codeDepartement = as.character(codeDepartement))
  
  # Agrégation par département
  dtaf <- st_transform(dtaf, 2154)              # Lambert-93
  dtaf <- st_make_valid(dtaf)                   # répare les anneaux croisés
  dtaf <- st_collection_extract(dtaf, "POLYGON") %>%   # enlève GEOMETRYCOLLECTIONs
    st_cast("MULTIPOLYGON")
  dtaf$geometry <- st_buffer(dtaf$geometry, 0)
  
  dep <- dtaf %>% 
    group_by(codeDepartement) %>% 
    summarise(.groups = "drop", do_union = TRUE)
  
  # Couleur
  cols <- c("MACRON"= "#F2C14E", "LE PEN"= "#0D3B66","MÉLENCHON"= "#D7263D")
  
  # Carte
  ggplot(dtaf) +
    geom_sf(aes(fill = Gagnant), color = "white", size = 0.1) +
    geom_sf(data = dep, fill = NA, color = "white", size = 0.2) +
    scale_fill_manual(values = cols, na.value = "grey85") +
    coord_sf(expand = FALSE) +
    labs(
      title = "Résultats par circonscription – France métropolitaine (gagnant)",
      fill = "Gagnant") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
})
