library(tidyverse)
library(patchwork)
set.seed(42)

sim_results = read_csv(here::here("data", "simulation_results.csv"))

no_cq = sim_results %>% 
  filter(scenario != "CQ")

clustered_post = read_csv(here::here("data", "simulation_results_clustered.csv")) %>% 
  select(scientific_name, kmeans) %>% 
  distinct() %>% 
  arrange(kmeans)

no_cq = no_cq %>% 
  right_join(clustered_post) %>%
  drop_na() %>%
  mutate(scientific_name = fct_reorder(as.factor(scientific_name), kmeans))

p <- ggplot() +
  geom_rect(data = clustered_post, aes(xmin = -Inf, xmax = Inf, ymin = 1.1, ymax = 1.25, fill = as.factor(kmeans)), alpha = 0.4) +
  geom_line(data = no_cq, aes(t, n_div_k, color = scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
             labeller = label_wrap_gen(30), scales = "free") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "Group"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("initial_sim_0_2.pdf"), path = here::here("figs"), height = 20, width = 25)

sim_sub = sim_results %>% 
  filter(t == 10 | t == 200) %>% 
  mutate(t = as.factor(t)) %>% 
  distinct()

no_cg_sub = sim_sub %>% 
  filter(scenario != "CQ")%>% 
  right_join(clustered_post)

errors_mort = no_cg_sub %>% 
  ungroup() %>% 
  select(t, scenario, avm, prm, scientific_name, mort_scenario, pop.array, n_div_k, total_mort, kmeans) %>% 
  distinct() %>% 
  group_by(scientific_name, mort_scenario) %>% 
  filter(total_mort == min(total_mort) | total_mort == max(total_mort)) %>% 
  filter(mort_scenario %in% c("BAU", "Low Mortality", "High Mortality")) %>% 
  distinct() %>% 
  ungroup() %>%
  mutate(scientific_name = as.factor(scientific_name))

p2 = ggplot(errors_mort)  +
  geom_rect(data = clustered_post, aes(xmin = -Inf, xmax = Inf, ymin = 1.1, ymax = 1.25, fill = as.factor(kmeans)), alpha = 0.4) +
  geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_point(aes(t, n_div_k, color = mort_scenario)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
             labeller = label_wrap_gen(30), scales = "free") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "Group"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p2

ggsave(p2, file = paste0("timepoints_0_2.pdf"), path = here::here("figs"), height = 20, width = 25)

# Species Sub Plots -------------------------------------------------------

species_sub = c("Raja montagui", "Prionace glauca", "Carcharhinus limbatus", "Galeorhinus galeus", "Galeocerdo cuvier", "Rhincodon typus", "Squalus acanthias", "Raja undulata", "Mustelus lunulatus")

no_cq_sub = no_cq %>% 
  filter(scientific_name %in% species_sub)

clustered_post_sub = clustered_post %>% 
  filter(scientific_name %in% species_sub)

p <- ggplot() +
  geom_rect(data = clustered_post_sub, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.1, fill = as.factor(kmeans)), alpha = 0.4) +
  geom_line(data = no_cq_sub, aes(t, n_div_k, color = scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
             labeller = label_wrap_gen(30), scales = "free") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "Group"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("species_full_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)

errors_mort_sub = errors_mort %>% 
  filter(scientific_name %in% species_sub)%>%
  mutate(scientific_name = as.factor(scientific_name)) %>% 
  mutate(scientific_name = fct_reorder(scientific_name, kmeans))

p2 = ggplot(errors_mort_sub)+
  geom_rect(data = clustered_post_sub, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.2, fill = as.factor(kmeans)), alpha = 0.4) +
  geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_point(aes(t, n_div_k, color = mort_scenario)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
             labeller = label_wrap_gen(30), scales = "free") +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "Group"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p2

ggsave(p2, file = paste0("species_timepoints_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)
