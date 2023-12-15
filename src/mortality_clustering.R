library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(patchwork)
library(vegan)

set.seed(42)

# data directory from gdrive
basedir <- "G:/Meu Drive/PRM review/"
datadir <- file.path(basedir, "data/fish_base_data")
outdir <- file.path(basedir, "data/outputs") # calls present data from raster files

sim_results <- read_csv(here::here(basedir, "data", "simulation_results.csv")) %>% 
  left_join(sim_data)

# Sim Results -------------------------------------------------------------

sim_data <- read_csv(here::here("data", "simulation_data.csv")) %>%
  select(r_value, scientific_name)

sim_results <- read_csv(here::here("data", "simulation_results.csv")) %>% 
  left_join(sim_data)

iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>% 
  mutate(status = case_when(
    str_detect(redlistCategory, "Near") ~ "NT",
    str_detect(redlistCategory, "Vuln") ~ "VU",
    str_detect(redlistCategory, "Least") ~ "LC",
    str_detect(redlistCategory, "Critically") ~ "CR",
    str_detect(redlistCategory, "Data") ~ "DD",
    redlistCategory == "Endangered" ~ "EN"
  )) %>% 
  rename(scientific_name = scientificName) %>% 
  select(scientific_name, status)

sim_sub = sim_results %>% 
  filter(t == 200) %>% 
  filter(!is.na(mort_scenario)) %>% 
  mutate(t = as.factor(t)) %>% 
  select(n_div_k, avm, prm, scientific_name, total_mort, mort_scenario, r_value) %>% 
  group_by(scientific_name, mort_scenario) %>% 
  filter(n_div_k == max(n_div_k)) %>% 
  distinct()

max_avm = sim_sub %>% 
  filter(n_div_k >= 0.5)

sim_pca_w_species = sim_sub %>%  
  pivot_wider(names_from = mort_scenario, values_from = c(avm, prm, total_mort, n_div_k)) %>% 
  ungroup()

sim_pca = sim_pca_w_species %>% 
  select(-scientific_name)%>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )

fviz_nbclust(sim_pca, FUNcluster = kmeans) 
km.out <- kmeans(sim_pca, centers = 3, nstart = 20)

sim_pca_w_species$kmeans = as.factor(km.out$cluster)

sim_pca_w_species = sim_pca_w_species %>% 
  select(scientific_name, kmeans)

sim_sub = sim_sub %>% 
  full_join(sim_pca_w_species) 

p1 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(r_value, n_div_k, color = kmeans)) +
  theme_bw() +
  labs(x = "Growth Rate",
       y = "N/K",
       color = "Group") +
  scale_color_viridis_d() +
  geom_hline(aes(yintercept = 0.5), color = "red", linetype = "dashed")

p2 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, avm, color = kmeans)) +
  theme_bw() +
  labs(x = "At-Vessel Mortality",
       y = "N/K",
       color = "Group") +
  scale_color_viridis_d() +
  geom_hline(aes(yintercept = 0.5), color = "red", linetype = "dashed")

p3 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, prm, color = kmeans))  +
  theme_bw() +
  labs(x = "Post Release Mortality",
       y = "N/K",
       color = "Group") +
  scale_color_viridis_d() +
  geom_hline(aes(yintercept = 0.5), color = "red", linetype = "dashed")

plot1 = p1 /  p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot1

ggsave(plot1, file = paste0("clustering.pdf"), path = here::here("figs"), height = 15, width = 12)


# nmds --------------------------------------------------------------------
sim_pca = sim_pca %>%
  mutate(r_value = case_when(
    r_value >= 0 ~ r_value,
    r_value < 0 ~ 0
  ))

nmds <- metaMDS(sim_pca, distance = "euclidian")

# extract NMDS scores (x and y coordinates)
filled_scores <- as.data.frame(scores(nmds)$sites)

# add columns to data frame
filled_scores$scientific_name <- sim_pca_w_species$scientific_name
filled_scores$kmeans <- sim_pca_w_species$kmeans

filled_scores <- filled_scores %>%
  mutate(scientific_name = as.factor(scientific_name), 
         kmeans = as.factor(kmeans))

centroid <- filled_scores %>%
  group_by(kmeans) %>%
  summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

species_scores <- as.data.frame(nmds[["species"]])

rownames(species_scores) <- c("r", "BAU AVM", "Low Mortality AVM", "Median Mortality AVM", "High Mortaltity AVM" ,
                               "BAU PRM", "Low Mortality PRM", "Median Mortality PRM", "High Mortaltity PRM",
                               "BAU Total", "Low Mortality Total", "Median Mortality Total", "High Mortaltity Total" ,
                               "BAU N/K", "Low Mortality N/K", "Median Mortality N/K", "High Mortaltity N/K")

species_scores <- species_scores %>% 
  rownames_to_column(var = "var") 

species_scores = species_scores %>% 
  filter(abs(MDS1) >= 0.1) %>% 
  filter(abs(MDS2) >= 0.1)


nmds <- ggplot(filled_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(color = kmeans), alpha = 0.3) +
  theme_bw(base_size = 16)+
  stat_ellipse(geom = "polygon", aes(color = kmeans, fill = kmeans), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(data = centroid, aes(color = kmeans), size = 4, alpha = 0.9)+
  scale_color_manual(values = c("#a0da39", "#d0e11c",  "#4ac16d")) +
  scale_fill_manual(values = c("#a0da39", "#d0e11c",  "#4ac16d")) +
  labs(x = "nMDS1", y = "nMDS2", color = "Group") +
  scale_y_continuous(position = "right")+
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 0.7, y = 0.8, label = "Stress: 0.138") +
  geom_segment(data = species_scores, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores, aes(x = MDS1, y = MDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25) 
nmds


  
  
  
   
  

nmds4


# Add IUCN Red List category for each species
sim_sub = sim_sub %>% 
  ungroup() %>% 
  select(scientific_name, kmeans) %>% 
  distinct() %>% 
  left_join(iucn_data, by = "scientific_name")

iucn_summary = sim_sub %>% 
  group_by(status, kmeans) %>% 
  summarize(count = n())

write_csv(sim_sub, here::here("data", "simulation_results_clustered.csv"))
