# ===== libs
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# ===== point de départ
df <- donnees

# helper : cast numérique à la volée (gère espaces/virgules)
to_num <- function(x){
  if (is.numeric(x)) return(x)
  x <- gsub("\\s+","", as.character(x))
  x <- gsub(",",".", x)
  suppressWarnings(as.numeric(x))
}

# ========= 1) Participation : distributions & nuage
ggplot(df, aes(x = to_num(Inscrits))) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution des inscrits", x = "Inscrits", y = "Fréquence")

ggplot(df, aes(x = to_num(Votants))) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution des votants", x = "Votants", y = "Fréquence")

ggplot(df, aes(x = to_num(`% Vot/Ins`))) +
  geom_histogram(bins = 30, fill = "orange", color = "white") +
  labs(title = "Taux de participation (%)", x = "% Vot/Ins", y = "Nombre de circonscriptions")

ggplot(df, aes(x = to_num(Inscrits), y = to_num(Votants))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Inscrits vs Votants", x = "Inscrits", y = "Votants")

# ========= 2) Prépa scores candidats (% Voix/Exp…)
# colonnes de % voix exprimées (ex: "% Voix/Exp", "% Voix/Exp3", ...)
score_cols <- names(df)[grepl("^%\\s*Voix/Exp\\d*$|^%\\s*Voix/Exp$", names(df))]
# ordre par numéro (on considère "% Voix/Exp" = candidat 1)
sid <- parse_number(ifelse(score_cols == "% Voix/Exp", "% Voix/Exp1", score_cols))
score_cols <- score_cols[order(sid)]

# colonnes de noms (Nom, Nom2… si présentes)
name_cols <- names(df)[grepl("^Nom\\d*$|^Nom$", names(df))]

# long des scores
res_long <- df |>
  select(all_of(score_cols),
         any_of(c("tx_pauvrete60_diff","pop_urb","age_moyen")),
         any_of(name_cols)) |>
  pivot_longer(all_of(score_cols), names_to = "score_key", values_to = "score_raw") |>
  mutate(
    candidat_id = parse_number(ifelse(score_key == "% Voix/Exp", "% Voix/Exp1", score_key)),
    score = to_num(score_raw)
  )

# mapping des noms (si disponibles)
if (length(name_cols) > 0) {
  noms_long <- df |>
    select(all_of(name_cols)) |>
    pivot_longer(everything(), names_to = "nom_key", values_to = "nom_candidat") |>
    mutate(candidat_id = parse_number(ifelse(nom_key == "Nom", "Nom1", nom_key))) |>
    distinct(candidat_id, nom_candidat)
  res_long <- res_long |>
    left_join(noms_long, by = "candidat_id") |>
    mutate(nom_affiche = ifelse(is.na(nom_candidat),
                                paste0("Candidat ", candidat_id),
                                as.character(nom_candidat)))
} else {
  res_long <- res_long |> mutate(nom_affiche = paste0("Candidat ", candidat_id))
}

# ========= 3) Scores par candidat : distributions & “clichés”
# boxplot des % voix
ggplot(res_long, aes(x = reorder(nom_affiche, -score, FUN = median, na.rm = TRUE), y = score)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution des % voix exprimées par candidat",
       x = "Candidat", y = "% voix exprimées")

# clichés : score ~ pauvreté / urbanité / âge (facettes par candidat)
if ("tx_pauvrete60_diff" %in% names(res_long)) {
  ggplot(res_long, aes(x = to_num(tx_pauvrete60_diff), y = score)) +
    geom_point(alpha = .45) +
    facet_wrap(~ nom_affiche, scales = "free_y") +
    labs(title = "Score ~ Taux de pauvreté", x = "Taux de pauvreté (%)", y = "% voix exprimées")
}
if ("pop_urb" %in% names(res_long)) {
  ggplot(res_long, aes(x = to_num(pop_urb), y = score)) +
    geom_point(alpha = .45) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ nom_affiche, scales = "free_y") +
    labs(title = "Score ~ Part de population urbaine", x = "Population urbaine (%)", y = "% voix exprimées")
}
if ("age_moyen" %in% names(res_long)) {
  ggplot(res_long, aes(x = to_num(age_moyen), y = score)) +
    geom_point(alpha = .45) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ nom_affiche, scales = "free_y") +
    labs(title = "Score ~ Âge moyen", x = "Âge moyen", y = "% voix exprimées")
}

# ========= 4) “Qui gagne où ?” (barplot + lollipop top 25)
# matrice des scores
mat <- df |>
  select(all_of(score_cols)) |>
  mutate(across(everything(), to_num)) |>
  as.matrix()
win_idx <- max.col(mat, ties.method = "first")
win_score <- mat[cbind(seq_len(nrow(mat)), win_idx)]

winners <- data.frame(
  row_id = seq_len(nrow(df)),
  winner_id = win_idx,
  winner_score = win_score
)

# nom du vainqueur si dispo
winners <- winners |>
  left_join(res_long |> distinct(candidat_id, nom_affiche), by = c("winner_id" = "candidat_id"))

# barplot : nb de circonscriptions gagnées
winners |>
  count(nom_affiche, sort = TRUE) |>
  ggplot(aes(x = reorder(nom_affiche, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Circonscriptions gagnées (1er tour)", x = "Candidat", y = "Nombre")

# lollipop : top 25 circonscriptions par score du vainqueur
winners |>
  arrange(desc(winner_score)) |>
  slice_head(n = 25) |>
  mutate(circo_idx = sprintf("circo_%03d", row_id)) |>
  ggplot(aes(x = reorder(circo_idx, winner_score), y = winner_score)) +
  geom_segment(aes(xend = circo_idx, y = 0, yend = winner_score)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Top 25 – score du vainqueur par circonscription",
       x = "Circonscription (index)", y = "% voix du vainqueur")

# ========= 5) Corrélations rapides (participation & socio-démo)
vars <- intersect(c("% Vot/Ins","tx_pauvrete60_diff","pop_urb","age_moyen",
                    "actemp","actcho","inactret","proprio","locatai",
                    "modtrans_commun","nivvie_median_diff"),
                  names(df))
if (length(vars) >= 3) {
  mat_num <- df |>
    transmute(across(all_of(vars), to_num)) |>
    as.data.frame()
  cmat <- cor(mat_num, use = "pairwise.complete.obs")
  corr_long <- as.data.frame(as.table(cmat))
  ggplot(corr_long, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Freq))) +
    scale_fill_viridis_c() +
    labs(title = "Corrélations (participation & socio-démo)", x = "", y = "", fill = "r")
}

# ========= 6) participation vs pauvreté/urbanité en 2D-binning
if (all(c("% Vot/Ins","tx_pauvrete60_diff") %in% names(df))) {
  ggplot(df, aes(x = to_num(tx_pauvrete60_diff), y = to_num(`% Vot/Ins`))) +
    geom_bin2d() + scale_fill_viridis_c() +
    labs(title = "Participation vs Pauvreté", x = "Pauvreté (%)", y = "% Vot/Ins", fill = "N")
}
if (all(c("% Vot/Ins","pop_urb") %in% names(df))) {
  ggplot(df, aes(x = to_num(pop_urb), y = to_num(`% Vot/Ins`))) +
    geom_bin2d() + scale_fill_viridis_c() +
    labs(title = "Participation vs Urbanité", x = "Population urbaine (%)", y = "% Vot/Ins", fill = "N")
}
