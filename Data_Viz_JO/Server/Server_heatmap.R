output$heatmapcorr <- renderPlotly({
    df <- if (exists("dtaf_base")) sf::st_drop_geometry(dtaf_base) else sf::st_drop_geometry(dtaf_loaded)
    
    cand_cols <- grep("_exp$", names(df), value = TRUE)
    
    sel <- input$themes %||% character(0)
    
    cols_demo  <- grep("^(pop_|age_)", names(df), value = TRUE)
    cols_dipl  <- grep("^actdip_", names(df), value = TRUE)
    cols_activ <- setdiff(grep("^act", names(df), value = TRUE), cols_dipl)  # act* sans actdip_*
    cols_csp   <- grep("^act_(agr|art|cad|int|emp|ouv|cho)$", names(df), value = TRUE)
    cols_logt  <- grep("^(proprio|locatai|mfuel)$", names(df), value = TRUE)
    cols_men   <- grep("^men_", names(df), value = TRUE)
    cols_mob   <- grep("^modtrans_", names(df), value = TRUE)
    cols_niv   <- grep("^(tx_pauvrete60_diff|nivvie_median_diff)$", names(df), value = TRUE)
    cols_acc   <- grep("^acc_", names(df), value = TRUE)
    
    pool <- character(0)
    if ("demo"  %in% sel) pool <- c(pool, cols_demo)
    if ("activ" %in% sel) pool <- c(pool, cols_activ)
    if ("dipl"  %in% sel) pool <- c(pool, cols_dipl)
    if ("csp"   %in% sel) pool <- c(pool, cols_csp)
    if ("logt"  %in% sel) pool <- c(pool, cols_logt)
    if ("men"   %in% sel) pool <- c(pool, cols_men)
    if ("mob"   %in% sel) pool <- c(pool, cols_mob)
    if ("nivvie"%in% sel) pool <- c(pool, cols_niv)
    if ("acc"   %in% sel) pool <- c(pool, cols_acc)
    soc_cols <- unique(pool)
    
    keep <- intersect(c(cand_cols, soc_cols), names(df))
    df[keep] <- lapply(df[keep], function(x) as.numeric(as.character(x)))
    
    if (length(soc_cols) == 0 || length(cand_cols) == 0) {
      plot.new(); title("Sélectionne au moins un thème"); return(invisible())
    }
    
    cm <- cor(as.matrix(df[, soc_cols]), as.matrix(df[, cand_cols]), use = "pairwise.complete.obs")
    
    heatmaply(cm,
              dendrogram = "none",             # <-- pas de clusters affichés
              Rowv = FALSE, Colv = FALSE,      # <-- pas d’ordonnancement par clustering
              seriate = "none",
              colors = colorRampPalette(c("#2C7BB6","white","#D7191C"))(256),
              xlab = NULL, ylab = NULL,
              plot_method = "plotly")
  })

