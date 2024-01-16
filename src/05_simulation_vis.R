library(tidyverse)
library(patchwork)
set.seed(42)

# data directory from gdrive
basedir <- "G:/Meu Drive/PRM review/"
datadir <- file.path(basedir, "data/fish_base_data")
outdir <- file.path(basedir, "data/outputs") 

sim_results <- read_csv(here::here(basedir, "data", "simulation_results.csv")) %>% 
  filter(scenario != "CQ") %>% 
  filter(!is.na(mort_scenario))

sim_results_count <- sim_results %>% 
  filter(t == 200 & mort_scenario == "Low Mortality" & n_div_k <= 0.5) %>% 
  select(scientific_name)  %>% 
  distinct()

sim_results_total_count <- sim_results %>% 
  select(scientific_name)  %>% 
  distinct()

sim_results = read_csv(here::here("data", "simulation_results.csv"))%>% 
  filter(scenario != "CQ") %>% 
  filter(!is.na(mort_scenario))

iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(systems, "Marine")) %>% 
  select(scientific_name, redlist_category, year_published) %>% 
  mutate(redlist_category = case_when(
    str_detect(redlist_category, "Near") ~ "NT",
    str_detect(redlist_category, "Vul") ~ "VU",
    str_detect(redlist_category, "Data") ~ "DD",
    redlist_category == "Endangered" ~ "EN",
    redlist_category == "Critically Endangered" ~ "CR",
    str_detect(redlist_category, "Least") ~ "LC",
    is.na(redlist_category) ~ "NE",
    TRUE ~ redlist_category
  )) %>% 
  select(scientific_name, redlist_category)

no_cq = sim_results %>% 
  mutate(mort_scenario = fct_relevel(as.factor(mort_scenario), "Low Mortality", after = Inf)) %>% 
  distinct() %>% 
  left_join(iucn_data) %>% 
  mutate(redlist_category = case_when(
    is.na(redlist_category) ~ "NE",
    TRUE ~ redlist_category
  ))  %>% 
  mutate(redlist_category = fct_relevel(as.factor(redlist_category), c("CR", "EN", "VU", "NT", "LC", "DD", "NE"))) 
  arrange(redlist_category, scientific_name)


no_cq_sci <- no_cq %>% 
  distinct(scientific_name) %>% 
  pull()

p <- 
  ggplot() +
  geom_rect(data = no_cq, aes(xmin = -Inf, xmax = Inf, ymin = 1.1, ymax = 1.9, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~ factor(scientific_name,
                      levels = no_cq_sci),
             labeller = label_wrap_gen(15)) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2", "grey", "white")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("initial_sim.pdf"), path = here::here("figs", "supp"), height = 20, width = 25)

# Species Sub Plots -------------------------------------------------------

species_sub = c("Prionace glauca", "Carcharhinus limbatus", "Isurus oxyrinchus", 
                "Mustelus canis", "Squalus acanthias", "Alopias vulpinus", 
                "Pseudocarcharias kamoharai", "Carcharhinus falciformis", "Sphyrna mokarran",
                "Carcharhinus hemiodon", "Squatina squatina", "Isogomphodon oxyrhynchus", "Squalus blainville",
                "Echinorhinus cookei", "Sphyrna corona", "Galeorhinus galeus")

no_cq_sub = no_cq %>% 
  filter(scientific_name %in% species_sub) 

p <- ggplot() +
  geom_hline(yintercept = 0.5,
             color = "gray",
             linetype = "dashed") +
  geom_rect(data = no_cq_sub,
            aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.13, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_sub, aes(t, n_div_k, color = mort_scenario, group = total_mort), linewidth = 2) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~scientific_name,
             #labeller = label_wrap_gen(10),
             nrow = 4) +
  theme_bw() +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976", "#91CF60", "#1A9850", "gray")) +
  labs(
    x = "Time",
    y = "N/K",
    fill = "IUCN Category",
    color = "Scenario"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = NA),
        strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("species_full.pdf"), path = here::here("figs"), height = 15, width = 20)


percent_calc = sim_results %>% 
  mutate(f_mort = ((100 * f) - (100 * f * (1-mid_avm) * (1-mid_prm)))/100) %>% 
  select(scientific_name, f, f_mort, mid_avm, mid_prm) %>% 
  distinct()%>% 
  mutate(percent_diff = (f - f_mort) / f * 100) %>% 
  mutate(mean_diff = mean(percent_diff, na.rm=TRUE))

percent_over_30 = percent_calc %>% 
  filter(percent_diff >= 50)

24/431*100

sim_200 = sim_results %>% 
  filter(t == 200) %>% 
  filter(mort_scenario == "Median Mortality") %>% 
  select(n_div_k, scientific_name)%>% 
  filter(scientific_name %in% species_sub) 

sim_over = sim_200 %>%
  filter(n_div_k >= 0.5)

sim_under = sim_200  %>%
  filter(n_div_k < 0.5)


# old ---------------------------------------------------------------------


# 
# errors_mort_sub = errors_mort %>% 
#   filter(scientific_name %in% species_sub)%>%
#   mutate(scientific_name = as.factor(scientific_name)) %>% 
#   mutate(scientific_name = fct_reorder(scientific_name, kmeans))
# 
# p2 = ggplot(errors_mort_sub)+
#   geom_rect(data = clustered_post_sub, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.2, fill = as.factor(kmeans)), alpha = 0.4) +
#   geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
#   geom_point(aes(t, n_div_k, color = mort_scenario)) +
#   geom_hline(yintercept = 0.5,
#              color = "gray",
#              alpha = 0.5,
#              linetype = "dashed") +
#   scale_y_continuous(breaks = c(0, 0.5, 1)) +
#   facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
#              labeller = label_wrap_gen(30), scales = "free") +
#   theme_bw() +
#   scale_color_viridis_d() +
#   scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
#   labs(
#     x = "Time",
#     y = "N/K",
#     color = "Scenario",
#     fill = "Group"
#   ) +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background = element_rect(fill=NA),
#         strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
#         axis.title = element_text(size = 10, color = "black"),
#         axis.text = element_text(size = 10, color = "black")) +
#   coord_cartesian(clip="off", ylim = c(0, 1))
# p2
# 
# ggsave(p2, file = paste0("species_timepoints_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)
# 
# sim_sub = sim_results %>% 
#   filter(t == 10 | t == 200) %>% 
#   mutate(t = as.factor(t)) %>% 
#   distinct()
# 
# no_cg_sub = sim_sub %>% 
#   filter(scenario != "CQ")%>% 
#   right_join(clustered_post)
# 
# errors_mort = no_cg_sub %>% 
#   ungroup() %>% 
#   select(t, scenario, avm, prm, scientific_name, mort_scenario, pop.array, n_div_k, total_mort, kmeans) %>% 
#   distinct() %>% 
#   group_by(scientific_name, mort_scenario) %>% 
#   filter(total_mort == min(total_mort) | total_mort == max(total_mort)) %>% 
#   filter(mort_scenario %in% c("BAU", "Low Mortality", "High Mortality")) %>% 
#   distinct() %>% 
#   ungroup() %>%
#   mutate(scientific_name = as.factor(scientific_name))
# 
# p2 = ggplot(errors_mort)  +
#   geom_rect(data = clustered_post, aes(xmin = -Inf, xmax = Inf, ymin = 1.1, ymax = 1.25, fill = as.factor(kmeans)), alpha = 0.4) +
#   geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
#   geom_point(aes(t, n_div_k, color = mort_scenario)) +
#   geom_hline(yintercept = 0.5,
#              color = "gray",
#              alpha = 0.5,
#              linetype = "dashed") +
#   scale_y_continuous(breaks = c(0, 0.5, 1)) +
#   facet_wrap(~fct_reorder(as.factor(scientific_name), kmeans),
#              labeller = label_wrap_gen(30), scales = "free") +
#   theme_bw() +
#   scale_color_viridis_d() +
#   scale_fill_manual(values = c("#d0e11c","#a0da39", "#73d056", "#4ac16d")) +
#   labs(
#     x = "Time",
#     y = "N/K",
#     color = "Scenario",
#     fill = "Group"
#   ) +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background = element_rect(fill=NA),
#         strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
#         axis.title = element_text(size = 10, color = "black"),
#         axis.text = element_text(size = 10, color = "black")) +
#   coord_cartesian(clip="off", ylim = c(0, 1))
# p2
# 
# ggsave(p2, file = paste0("timepoints_0_2.pdf"), path = here::here("figs"), height = 20, width = 25)
