# Packages
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
### Les graphiques descriptifs ####

summary(dtaf)

### Carte des résultats ####
# Harmonise les modalités pour des couleurs stables
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
  geom_sf(data = dep, fill = NA, color = "grey25", size = 0.2) +
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

### Carte d'abstension ####
ggplot(dtaf) +
  geom_sf(aes(fill = Abs_insc), color = "white", size = 0.1) +
  geom_sf(data = dep, fill = NA, color = "grey25", size = 0.2) +
  scale_fill_viridis_c(name = "Abstention (%)", labels = label_number(accuracy = 0.1, suffix = " %"))+
  coord_sf(expand = FALSE) +
  labs(title = "Abstention par circonscription – France métropolitaine") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

### Distribution des pourcentages par candidat ####
# Enlever la géométrie
cand_cols <- grep("_exp$", names(dtaf), value = TRUE) 

dat_long <- dtaf %>% 
  st_drop_geometry() %>% 
  select(all_of(cand_cols)) %>% 
  pivot_longer(everything(), names_to = "Candidat", values_to = "Pourcentage")

# Nom des candidats
nom_candidat <- c(
  "Arthaud_exp"="Arthaud","Roussel_exp"="Roussel","Macron_exp"="Macron",
  "Lassalle_exp"="Lassalle","LePen_exp"="Le Pen","Zemmour_exp"="Zemmour",
  "Melenchon_exp"="Mélenchon","Hidalgo_exp"="Hidalgo","Jadot_exp"="Jadot",
  "Pecresse_exp"="Pécresse","Poutou_exp"="Poutou","DupontAignan_exp"="Dupont-Aignan"
)
dat_long <- dat_long %>% 
  mutate(Candidat = recode(Candidat, !!!nom_candidat, .default = Candidat))

ggplot(dat_long, aes(
  x = fct_reorder(Candidat, Pourcentage, .fun = median, .desc = TRUE),
  y = Pourcentage,
  fill = Candidat
)) +
  geom_boxplot(
    width = 0.7,
    notch = TRUE,
    outlier.alpha = 0.35,
    outlier.shape = 21
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 2.5,
    fill = "white"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_number(accuracy = 0.1, suffix = " %")
  ) +
  scale_fill_viridis_d(option = "plasma",
                       direction = -1,
                       guide = "none") +
  coord_flip() +
  labs(
    title = "Distribution des pourcentages par candidat",
    subtitle = paste0("n = ", nrow(dtaf), " circonscriptions"),
    x = NULL,
    y = "Pourcentage"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
