library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(patchwork)

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
