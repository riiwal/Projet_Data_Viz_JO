output$heatmapcorr <- renderPlotly({
  dtaf_heatmap <- st_drop_geometry(dtaf_base)
  
  candidat_colonne <- grep("_exp$", names(dtaf_heatmap), value = TRUE)
  
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
  
  sel <- input$themes
  
 
  socio_colonne <- unique(unlist(theme_socio[sel], use.names = FALSE))
  socio_colonne <- intersect(socio_colonne, names(dtaf_heatmap))
  
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
  
  cm <- cor(
    as.matrix(dtaf_heatmap[, socio_colonne, drop = FALSE]),
    as.matrix(dtaf_heatmap[, candidat_colonne, drop = FALSE]),
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  ordre_candidat <- c(
    "Zemmour_exp","LePen_exp","DupontAignan_exp","Pecresse_exp",
    "Macron_exp","Jadot_exp","Hidalgo_exp","Roussel_exp",
    "Melenchon_exp","Poutou_exp","Arthaud_exp","Lassalle_exp"
  )
  
  candidat <- intersect(ordre_candidat, colnames(cm))
  cm <- cm[, candidat, drop = FALSE]
  
  ordre_ligne <- order(apply(abs(cm), 1, max, na.rm = TRUE), decreasing = TRUE)
  cm <- cm[ordre_ligne, , drop = FALSE]
  
  rn <- rownames(cm); rn_lab <- label_variable[rn]; rn[!is.na(rn_lab)] <- rn_lab[!is.na(rn_lab)]; rownames(cm) <- rn
  cn <- colnames(cm); cn_lab <- label_candidat[cn]; cn[!is.na(cn_lab)] <- cn_lab[!is.na(cn_lab)]; colnames(cm) <- cn
  
  rr <- rownames(cm)
  cc <- colnames(cm)
  txt <- outer(
    rr, cc,
    Vectorize(function(r, c) sprintf("Variable : %s<br>Candidat : %s<br>Corrélation : %.2f", r, c, cm[r, c]))
  )
  
  palette <- colorRampPalette(c("#2C7BB6","white","#D7191C"))(256)
  
  heatmaply(
    cm,
    colors = palette,
    limits = c(-1, 1),
    grid_color = "grey90",
    grid_width = 0.3,
    row_text_angle = 0,
    column_text_angle = 45,
    xlab = NULL, ylab = NULL,
    dendrogram = "none",
    Rowv = FALSE, Colv = FALSE, seriate = "none",
    plot_method = "plotly",
    hide_colorbar = FALSE,
    custom_hovertext = txt
  )
})
