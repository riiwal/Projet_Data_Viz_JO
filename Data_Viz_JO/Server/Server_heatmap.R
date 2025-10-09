output$heatmapcorr <- renderPlotly({
  dtaf_heatmap <- st_drop_geometry(dtaf_base)
  
  # Sélection des colonnes des candidats qui finissent par _exp
  candidat_colonne <- grep("_exp$", names(dtaf_heatmap), value = TRUE)
  
  # Liste des thèmes socio
  theme_socio <- list(
    demo   = c("pop_pole_aav","pop_cour_aav","pop_horsaav","pop_urb","pop_rur_periu","pop_rur_non_periu"),
    activ  = c("actemp","actcho","inactret"),
    dipl   = c("actdip_PEU","actdip_CAP","actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5"),
    csp    = c("act_agr","act_art","act_cad","act_int","act_emp","act_ouv","act_cho"),
    logt   = c("proprio","locatai","mfuel"),
    men    = c("men_seul","men_coupae","men_coupse","men_monop"),
    mob    = c("modtrans_aucun","modtrans_pied","modtrans_velo","modtrans_moto","modtrans_voit","modtrans_commun"),
    nivvie = c("tx_pauvrete60_diff","nivvie_median_diff"),
    acc    = c("acc_ecole","acc_college","acc_lycee","acc_medecin","acc_dentiste","acc_pharmacie")
  )
  
  # Séléction du thème socio par l'utilisateur
  sel <- input$themes
  
 # Extraction des noms des variables
  socio_colonne <- unique(unlist(theme_socio[sel], use.names = FALSE))
  socio_colonne <- intersect(socio_colonne, names(dtaf_heatmap))
  
  # Label des candidats
  label_candidat <- c(
    "Zemmour_exp"="Éric\nZemmour",
    "LePen_exp"="Marine\nLe Pen",
    "DupontAignan_exp"="Nicolas\nDupont-Aignan",
    "Pecresse_exp"="Valérie\nPécresse",
    "Macron_exp"="Emmanuel\nMacron",
    "Jadot_exp"="Yannick\nJadot",
    "Hidalgo_exp"="Anne\nHidalgo",
    "Roussel_exp"="Fabien\nRoussel",
    "Melenchon_exp"="Jean-Luc\nMélenchon",
    "Poutou_exp"="Philippe\nPoutou",
    "Arthaud_exp"="Nathalie\nArthaud",
    "Lassalle_exp"="Jean\nLassalle"
  )
  
  # Les labels renommés
  label_variable <- c(
    pop_pole_aav="% pop. pôles AAV", pop_cour_aav="% pop. couronnes AAV", pop_horsaav="% pop. hors AAV",
    pop_urb="% pop. urbaine", pop_rur_periu="% pop. rurale périurbaine", pop_rur_non_periu="% pop. rurale non périurbaine",
    actemp="% actifs en emploi", actcho="% chômeurs", inactret="% inactifs retraités",
    actdip_PEU="Sans diplôme", actdip_CAP="CAP-BEP", actdip_BAC="BAC", actdip_BAC2="Bac+2", actdip_BAC3="Bac+3", actdip_BAC5="Bac+5",
    act_agr="% agriculteurs", act_art="% artisans/com.", act_cad="% cadres", act_int="% prof. intermédiaires",
    act_emp="% employés", act_ouv="% ouvriers", act_cho="% chômage",
    proprio="% propriétaires", locatai="% locataires", mfuel="% chauffage fioul",
    men_seul="% ménages seuls", men_coupae="% couples avec enfants", men_coupse="% couples sans enfants", men_monop="% familles monoparentales",
    modtrans_aucun="% aucun transport", modtrans_pied="% à pied", modtrans_velo="% vélo",
    modtrans_moto="% moto", modtrans_voit="% voiture", modtrans_commun="% transports en commun",
    tx_pauvrete60_diff="Taux de pauvreté (60%)", nivvie_median_diff="Niveau de vie médian",
    acc_ecole="% accès école", acc_college="% accès collège", acc_lycee="% accès lycée",
    acc_medecin="% accès médecin", acc_dentiste="% accès dentiste", acc_pharmacie="% accès pharmacie"
  )
  
  # Calcul de la matrice de corrélation
  cm <- cor(
    as.matrix(dtaf_heatmap[, socio_colonne, drop = FALSE]),
    as.matrix(dtaf_heatmap[, candidat_colonne, drop = FALSE]),
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  # Ordre des candidats
  ordre_candidat <- c(
    "Arthaud_exp", "Poutou_exp", "Melenchon_exp", "Roussel_exp","Lassalle_exp", 
    "Hidalgo_exp","Jadot_exp",
    "Macron_exp", "Pecresse_exp",
    "DupontAignan_exp", "LePen_exp", "Zemmour_exp"
  )
  
  
  candidat <- intersect(ordre_candidat, colnames(cm))
  cm <- cm[, candidat, drop = FALSE]
  
  # Réorganisation des lignes selon l'ordre défini dans label_variable
  ordre_labels <- names(label_variable)
  ordre_labels <- intersect(ordre_labels, rownames(cm))
  cm <- cm[ordre_labels, , drop = FALSE]
  
  # Remplacer les noms par les labels
  rownames(cm) <- label_variable[rownames(cm)]
  colnames(cm) <- label_candidat[colnames(cm)]
  
  # Matrice de texte à afficher au survol
  rr <- rownames(cm)
  cc <- colnames(cm)
  txt <- outer(
    rr, cc,
    Vectorize(function(r, c) sprintf("Variable : %s<br>Candidat : %s<br>Corrélation : %.2f", r, c, cm[r, c]))
  )
  
  # Palette
  palette <- colorRampPalette(c("#2C7BB6","white","#D7191C"))(256)
  
  # Construction de la heatmap interactive
  heatmaply(
    cm,
    colors = palette,
    limits = c(-1, 1),
    dendrogram = "none",
    Rowv = FALSE, Colv = FALSE, seriate = "none",
    plot_method = "plotly",
    custom_hovertext = txt,
    fontsize_row = 14,             
    fontsize_col = 14,             
    colorbar = list(              
      tickfont = list(size = 20),
      title = list(text = "Corrélation", font = list(size = 16))
    )
  )
  
})

