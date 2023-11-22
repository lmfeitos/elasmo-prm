library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggfortify)

# Sim Results -------------------------------------------------------------

r_data = sim_data %>%
  select(r_value, scientific_name)

sim_results <- read_csv(here::here("data", "simulation_results.csv")) %>% 
  left_join(r_data)

sim_sub = sim_results %>% 
  filter(t == 200) %>% 
  mutate(t = as.factor(t)) %>% 
  select(n_div_k, avm, prm, scientific_name, total_mort, mort_scenario, r_value) %>% 
  filter(mort_scenario != "In-Between Mortality") %>%
  group_by(scientific_name, mort_scenario) %>% 
  filter(n_div_k == max(n_div_k)) %>% 
  distinct()

ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, avm)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, prm)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, prm*avm)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, r_value)) +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

sim_pca_w_species = sim_sub %>%  
  pivot_wider(names_from = mort_scenario, values_from = c(avm, prm, total_mort, n_div_k)) %>% 
  drop_na() %>% 
  ungroup()

sim_pca = sim_pca_w_species %>% 
  select(-scientific_name)

fviz_nbclust(sim_pca, FUNcluster = kmeans) 
km.out <- kmeans(sim_pca, centers = 3, nstart = 20)

sim_pca_w_species$kmeans = as.factor(km.out$cluster)

sim_pca_w_species = sim_pca_w_species %>% 
  select(scientific_name, kmeans)

sim_sub = sim_sub %>% 
  full_join(sim_pca_w_species)

ggplot(sim_sub) +
  geom_point(aes(n_div_k, r_value, color = kmeans)) 

ggplot(sim_sub) +
  geom_point(aes(n_div_k, avm, color = kmeans))

ggplot(sim_sub) +
  geom_point(aes(n_div_k, prm, color = kmeans)) 

write_csv(sim_sub, here::here("data", "simulation_results_clustered.csv"))
