library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(patchwork)

# Sim Results -------------------------------------------------------------

sim_data <- read_csv(here::here("data", "simulation_data.csv")) %>%
  select(r_value, scientific_name)

sim_results <- read_csv(here::here("data", "simulation_results.csv")) %>% 
  left_join(sim_data)

sim_sub = sim_results %>% 
  filter(t == 200) %>% 
  mutate(t = as.factor(t)) %>% 
  select(n_div_k, avm, prm, scientific_name, total_mort, mort_scenario, r_value) %>% 
  filter(mort_scenario != "In-Between Mortality") %>%
  group_by(scientific_name, mort_scenario) %>% 
  filter(n_div_k == max(n_div_k)) %>% 
  distinct()

max_avm = sim_sub %>% 
  filter(n_div_k >= 0.5) %>% 
  pull(avm) %>% 
  max()
max_prm = sim_sub %>% 
  filter(n_div_k >= 0.5)%>% 
  pull(prm) %>% 
  max()
max_total = sim_sub %>% 
  filter(n_div_k >= 0.5) %>% 
  mutate(total_mort = avm * prm)%>% 
  pull(total_mort) %>% 
  max()

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
  ungroup()

sim_pca = sim_pca_w_species %>% 
  select(-scientific_name)%>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )

fviz_nbclust(sim_pca, FUNcluster = kmeans) 
km.out <- kmeans(sim_pca, centers = 2, nstart = 20)

sim_pca_w_species$kmeans = as.factor(km.out$cluster)

sim_pca_w_species = sim_pca_w_species %>% 
  select(scientific_name, kmeans)

sim_sub = sim_sub %>% 
  full_join(sim_pca_w_species)

p1 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, r_value, color = kmeans)) +
  theme_bw() +
  labs(x = "N/K",
       y = "Growth Rate",
       color = "Group") +
  scale_color_viridis_d() +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

p2 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, avm, color = kmeans)) +
  theme_bw() +
  labs(x = "N/K",
       y = "At Vessel Mortality",
       color = "Group") +
  scale_color_viridis_d() +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

p3 = ggplot(sim_sub %>% filter(mort_scenario != "BAU")) +
  geom_point(aes(n_div_k, prm, color = kmeans))  +
  theme_bw() +
  labs(x = "N/K",
       y = "Post Release Mortality",
       color = "Group") +
  scale_color_viridis_d() +
  geom_vline(aes(xintercept = 0.5), color = "red", linetype = "dashed")

plot1 = p1 /  p2 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot1

ggsave(plot1, file = paste0("clustering.pdf"), path = here::here("figs"), height = 15, width = 12)

write_csv(sim_sub, here::here("data", "simulation_results_clustered.csv"))
