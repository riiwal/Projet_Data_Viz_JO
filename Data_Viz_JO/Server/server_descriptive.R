# Carte de participation
output$carteparticipation <- renderPlot({
  
  ggplot(dtaf_base) +
    geom_sf(aes(fill = Vot_insc),
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

# Carte des gagnants
output$cartegagnant <- renderPlot({
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
output$histogrammeresultat <- renderPlot({
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
  
  dtaf_sum <- dtaf_base %>%
    mutate(
      inscrits    = Inscrit_22,
      part_prop   = Vot_insc / 100,
      blancs_prop = Blanc_vote / 100,
      nuls_prop   = Nul_vote / 100,
      exprimes    = inscrits * part_prop * (1 - blancs_prop - nuls_prop)
    ) %>%
    select(all_of(unname(candidat)), exprimes)
  
  pourcentage_candidat <- dtaf_sum %>%
    pivot_longer(cols = all_of(unname(candidat)),
                 names_to = "var",
                 values_to = "pct_exp") %>%
    mutate(votes_cand = exprimes * (pct_exp / 100)) %>%
    group_by(var) %>%
    summarise(votes = sum(votes_cand, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(vote = votes / sum(votes),
           candidat = names(candidat)[match(var, unname(candidat))]) %>%
    arrange(desc(vote)) %>%
    mutate(candidat = factor(candidat, levels = candidat))
  
  couleur_parti <- c(
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
    "Nathalie\nArthaud"     = "#6E3E3B"
  )
  
  ggplot(pourcentage_candidat, aes(x = candidat, y = vote, fill = candidat)) +
    geom_col(width = 0.85) +
    geom_text(
      aes(label = percent(vote, accuracy = 0.01)),
      vjust = -0.3,
      size = 3.8,
      fontface = "bold"
    ) +
    scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
    scale_fill_manual(values = couleur_parti, guide = "none") +
    labs(title = "Résultat des élections présidentielles au 1er tour", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(
        size = 11,
        face = "bold",
        lineheight = 0.95,
        margin = margin(t = 6)
      ),
      plot.margin = margin(
        t = 14,
        r = 18,
        b = 12,
        l = 28
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      )
    )
})


dtaf_histo <- st_drop_geometry(dtaf_base)

# Histogramme du milieu d'habitation des français
output$histogrammehabitat <- renderPlot({
  colonne_habitat <- c("pop_urb", "pop_rur_periu", "pop_rur_non_periu")
  
  histo_habitat <- dtaf_histo %>%
    select(all_of(colonne_habitat)) %>%
    add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_habitat = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "pop_urb"            = "Urbain",
        "pop_rur_periu"      = "Rural périurbain",
        "pop_rur_non_periu"  = "Rural non périurbain"
      )
    ) %>%
    arrange(desc(pourcentage_habitat)) %>%
    mutate(label = factor(label, levels = label))
  
  ggplot(histo_habitat, aes(x = label, y = pourcentage_habitat, fill = label)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent(pourcentage_habitat, accuracy = 0.1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Répartition par type d’espace", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des AAV des français
output$histoAAV <- renderPlot ({
  colonne_AAV <- c("pop_pole_aav", "pop_cour_aav", "pop_horsaav")
  
  histoAAV <- dtaf_histo %>%
    select(all_of(colonne_AAV)) %>%
    add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_AAV = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "pop_pole_aav" = "Pôle AAV",
        "pop_cour_aav" = "Couronne AAV",
        "pop_horsaav"  = "Hors AAV"
      )
    ) %>%
    arrange(desc(pourcentage_AAV)) %>%
    mutate(label = factor(label, levels = label))
  
  ggplot(histoAAV, aes(x = label, y = pourcentage_AAV, fill = label)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent(pourcentage_AAV, accuracy = 0.1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Répartition par aire d’attraction (AAV)", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des études des français
output$bar_diplomes <- renderPlot({
  colonne_etude <- c(
    "actdip_PEU",
    "actdip_CAP",
    "actdip_BAC",
    "actdip_BAC2",
    "actdip_BAC3",
    "actdip_BAC5"
  )
  
  histoetude <- dtaf_histo %>%
    select(all_of(colonne_etude)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_etude = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "actdip_PEU" = "Sans diplôme",
        "actdip_CAP" = "Cap/Bep",
        "actdip_BAC" = "BAC",
        "actdip_BAC2" = "BAC+2",
        "actdip_BAC3" = "BAC+3",
        "actdip_BAC5" = "BAC+5"
      )
    ) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histoetude, aes(x = label, y = pourcentage_etude, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_etude, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Niveaux de diplôme", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des CSP des français
output$bar_csp <- renderPlot({
  colonne_csp <- c("act_agr",
            "act_art",
            "act_cad",
            "act_int",
            "act_emp",
            "act_ouv",
            "act_cho")
  
  histocsp <- dtaf_histo %>%
    select(all_of(colonne_csp)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_csp = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "act_agr" = "Agriculteurs",
        "act_art" = "Artisans/Com.",
        "act_cad" = "Cadres",
        "act_int" = "Intermédiaires",
        "act_emp" = "Employés",
        "act_ouv" = "Ouvriers",
        "act_cho" = "Chômeurs"
      )
    ) %>%
    arrange(desc(pourcentage_csp)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histocsp, aes(x = label, y = pourcentage_csp, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_csp, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Catégories socio-professionnelles", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 9, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des ménages
output$bar_menages <- renderPlot({
  colonne_menage <- c("men_seul", "men_coupae", "men_coupse", "men_monop")
  
  histo_menage <- dtaf_histo %>%
    select(all_of(colonne_menage)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_menage = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "men_seul" = "Personnes \nseules",
        "men_coupae" = "Couples \navec enfants",
        "men_coupse" = "Couples \nsans enfants",
        "men_monop" = "Familles \nmonoparentales"
      )
    ) %>%
    arrange(desc(pourcentage_menage)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histo_menage, aes(x = label, y = pourcentage_menage, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_menage, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Types de ménages", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des mobilités travail maison des français
output$bar_mobilites <- renderPlot({
  colonne_transport <- c(
    "modtrans_aucun",
    "modtrans_pied",
    "modtrans_velo",
    "modtrans_moto",
    "modtrans_voit",
    "modtrans_commun"
  )
  
  histo_transport <- dtaf_histo %>%
    select(all_of(colonne_transport)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_transport = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "modtrans_aucun" = "Aucun",
        "modtrans_pied" = "À pied",
        "modtrans_velo" = "Vélo",
        "modtrans_moto" = "Deux-roues",
        "modtrans_voit" = "Voiture",
        "modtrans_commun" = "Transports \nen commun"
      )
    ) %>%
    arrange(desc(pourcentage_transport)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histo_transport, aes(x = label, y = pourcentage_transport, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_transport, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Transport domicile-travail principal", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 20,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme des propriétaires et locatires 
output$bar_logement <- renderPlot({
  colonne_logement <- c("proprio", "locatai")
  
  histo_logement <- dtaf_histo %>%
    select(all_of(colonne_logement)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_logement = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label = recode(cat, "proprio" = "Propriétaires", "locatai" =
                            "Locataires")) %>%
    arrange(desc(pourcentage_logement)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histo_logement, aes(x = label, y = pourcentage_logement, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_logement, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Statut d’occupation logement", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 17,
        face = "bold"
      ),
      legend.position = "none"
    )
})

# Histogramme de l'accès à l'éducation 

output$bar_acc_education <- renderPlot({
  colonne_education <- c("acc_ecole", "acc_college", "acc_lycee")
  
  histo_education <- dtaf_histo %>%
    select(all_of(colonne_education)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_education = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label = recode(
      cat,
      "acc_ecole" = "École",
      "acc_college" = "Collège",
      "acc_lycee" = "Lycée"
    )) %>%
    arrange(desc(pourcentage_education)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histo_education, aes(x = label, y = pourcentage_education, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_education, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Accessibilité à l’éducation", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 17,
        face = "bold"
      ),
      legend.position = "none"
    )
})
# Histogramme de l'accès aux soins

output$bar_acc_soins <- renderPlot({
  colonne_soins <- c("acc_medecin", "acc_dentiste", "acc_pharmacie")
  
  histo_soins <- dtaf_histo %>%
    select(all_of(colonne_soins)) %>% add_column(weight = dtaf_histo$pop_légal_19) %>%
    pivot_longer(-weight, names_to = "cat", values_to = "pct") %>%
    group_by(cat) %>%
    summarise(
      pourcentage_soins = sum(pct / 100 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = recode(
        cat,
        "acc_medecin" = "Médecin",
        "acc_dentiste" = "Dentiste",
        "acc_pharmacie" = "Pharmacie"
      )
    ) %>%
    arrange(desc(pourcentage_soins)) %>% mutate(label = factor(label, levels = label))
  
  ggplot(histo_soins, aes(x = label, y = pourcentage_soins, fill = label)) +
    geom_col(width = .7) +
    geom_text(aes(label = percent(pourcentage_soins, accuracy = .1)),
              vjust = -0.3,
              fontface = "bold") +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, .08))) +
    labs(title = "Accessibilité aux soins", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        size = 17,
        face = "bold"
      ),
      legend.position = "none"
    )
})