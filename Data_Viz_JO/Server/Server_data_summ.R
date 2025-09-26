library(shiny)
library(DT)
library(dplyr)
library(stargazer)
library(summarytools)

server <- function(input, output, session) {
  # création des thèmes pour l'affichage des variables sélectionnées dans le summary
  theme_vars <- list(
    "Démographie" = c("Inscrit22","pop_légal_19","pop_pole_aav","pop_cour_aav","pop_horsaav","pop_urb","pop_rur_periu","pop_rur_non_periu","age_moyen"),
    "Activité" = c("actemp","actcho","inactret"),
    "Education"=c("actdip_PEU","actdip_CAP","actdip_BAC","actdip_BAC2","actdip_BAC3","actdip_BAC5","actdip_BAC3P"),
    "CSP"= c("act_agr","act_art","act_cad","act_int","act_emp","act_ouv","act_cho"),
    "Menages" = c("men_seul","men_coupea","men_coupse","men_monop"),
    "Logement" = c("proprio","locatai","mfuel"),
    "Transport" = c("modtrans_aucun","modtrans_pied","modtrans_velo","modtrans_voit","modtrans_commun"),
    "Accès" = c("acc_ecole","acc_college","acc_lycee","acc_medecin","acc_dentiste","acc_pharmacie"),
    "Economie" = c("tx_pauvrete60_diff","nivvie_median_diff"),
    "Votes" = c("Abs_insc","Vot_insc","Blanc_vote","Nul_vote","Arthaud_exp","Roussel_exp","Macron_exp","Lassalle_exp","LePen_exp","Zemmour_exp",
                "Melenchon_exp","Hidalgo_exp","Jadot_exp","Pecresse_exp","Poutou_exp","DupontAignan_exp","Gagnat")
  )
  
  # Table affichée
  output$table <- renderDT({
    req(input$theme)
    vars <- theme_vars[[input$theme]]
    DT::datatable(dtaf[, vars, drop = FALSE], options = list(pageLength = 10))
  })
  
  # Résumé des variables
  output$summary <- renderUI({
    req(input$theme)
    vars <- theme_vars[[input$theme]]
    df <- dtaf[, vars, drop = FALSE]
    
    # résumé en HTML avec summarytools
    res <- summarytools::dfSummary(df, plain.ascii = FALSE, style = "grid", graph.col = FALSE)
    HTML(summarytools::print(dfSummary(df), method = "render"))
  })
}
