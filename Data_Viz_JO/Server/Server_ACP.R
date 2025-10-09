# création d'un df pour l'acp avec les variables les mieux projetées

acp1 <- reactive({ dtaf_loaded_acp <-dtaf_loaded %>% select("acc_lycee","act_ouv" ,"acc_college","pop_urb","locatai","proprio","actdip_BAC3",
                       "actdip_BAC3P","actdip_CAP","act_cad","actdip_BAC5","men_coupse") %>%
  st_drop_geometry()

# choix des variables les mieux projetées

res.pca <- PCA(dtaf_loaded_acp, scale.unit = TRUE, graph = FALSE)
# cos2_tot <- rowSums(res.pca$var$cos2[,1:2])
# sort(cos2_tot, decreasing = TRUE)

# liste des variables de vote
vars_vote <- reactive({c("Abs_insc","Vot_insc","Blanc_vote","Nul_vote",
               "Arthaud_exp","Roussel_exp","Macron_exp",
               "Lassalle_exp","LePen_exp","Zemmour_exp",
               "Melenchon_exp",
               "Hidalgo_exp",
               "Jadot_exp",
               "Pecresse_exp" ,
               "Poutou_exp",
               "DupontAignan_exp") })

fviz_pca_ind(res.pca,
             geom.ind = "point",
             col.ind = dtaf_loaded$Gagnant,  # individus colorés par "vainqueur"
             palette = c("#223A77","#F2C14E","#D7263D"), # couleur correspondant aux partis gagnants
             legend.title = "Vote majoritaire",
             title = "")+
  theme(
    legend.text = element_text(size = 14),      # taille du texte de la légende
    legend.title = element_text(size = 16),     # taille du titre de la légende
    axis.title = element_text(size = 16)       # taille du nom des axes
  )})

acp2 <- reactive({ dtaf_loaded_acp2 <- dtaf_loaded %>% select("acc_lycee","act_ouv" ,"acc_college","pop_urb","locatai","proprio","actdip_BAC3",
                                          "actdip_BAC3P","actdip_CAP","act_cad","actdip_BAC5","men_coupse") %>%
    st_drop_geometry()
  names(dtaf_loaded_acp2) <- label_variable()[names(dtaf_loaded_acp2)]
  # choix des variables les mieux projetées
  
  res.pca2 <- PCA(dtaf_loaded_acp2, scale.unit = TRUE, graph = FALSE)
  # cos2_tot <- rowSums(res.pca$var$cos2[,1:2])
  # sort(cos2_tot, decreasing = TRUE)
  
  # liste des variables de vote
  vars_vote <- reactive({c("Abs_insc","Vot_insc","Blanc_vote","Nul_vote",
                           "Arthaud_exp","Roussel_exp","Macron_exp",
                           "Lassalle_exp","LePen_exp","Zemmour_exp",
                           "Melenchon_exp",
                           "Hidalgo_exp",
                           "Jadot_exp",
                           "Pecresse_exp" ,
                           "Poutou_exp",
                           "DupontAignan_exp") })
  
  fviz_pca_var(res.pca2, repel = TRUE,      # évite le chevauchement des noms
               labelsize = 5,title  = "")+
    theme(legend.text = element_text(size = 14),      # taille du texte de la légende
    legend.title = element_text(size = 16),     # taille du titre de la légende
    axis.title = element_text(size = 16)) # taille du nom des axes
})

# création de l'output acp individus
output$PCA_ind <- renderPlot({ acp1()
  
})

# création de l'output acp variables
output$PCA_var <- renderPlot({ acp2()
})
