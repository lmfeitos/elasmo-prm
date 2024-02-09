library(tidyverse)
library(janitor)
library(patchwork)
set.seed(42)

iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") | 
           str_detect(scientific_name,"Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus")) %>% # list of genera to keep in the filtering
  select(scientific_name, redlist_category, year_published) %>% 
  mutate(redlist_category = case_when(
    str_detect(redlist_category, "Near") ~ "NT",
    str_detect(redlist_category, "Vul") ~ "VU",
    str_detect(redlist_category, "Data") ~ "DD",
    redlist_category == "Endangered" ~ "EN",
    redlist_category == "Critically Endangered" ~ "CR",
    str_detect(redlist_category, "Least") ~ "LC",
    TRUE ~ redlist_category
  ))

new_dat <- read_csv(here::here("data", "iucn_fishbase_list.csv")) %>% 
  select(scientific_name, common_name)

# data directory from gdrive
basedir <- "G:/Meu Drive/PRM review/"
datadir <- file.path(basedir, "data/fish_base_data")
outdir <- file.path(basedir, "data/outputs") 

sim_results <- read_csv(here::here(basedir, "data", "simulation_results.csv")) %>% 
  filter(scenario != "CQ") %>% 
  filter(!is.na(mort_scenario))

### ADDED THIS CODE TO MAKE FIGURES 5 AND SX DISPLAY COMMON NAMES ON THE FACET STRIPS INSTEAD OF THE SCIENTIFIC NAMES  
sim_results_new <- sim_results %>% 
  left_join(new_dat, by = "scientific_name")

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

# iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>% 
#   janitor::clean_names() %>% 
#   filter(str_detect(systems, "Marine")) %>% 
#   select(scientific_name, redlist_category, year_published) %>% 
#   mutate(redlist_category = case_when(
#     str_detect(redlist_category, "Near") ~ "NT",
#     str_detect(redlist_category, "Vul") ~ "VU",
#     str_detect(redlist_category, "Data") ~ "DD",
#     redlist_category == "Endangered" ~ "EN",
#     redlist_category == "Critically Endangered" ~ "CR",
#     str_detect(redlist_category, "Least") ~ "LC",
#     is.na(redlist_category) ~ "NE",
#     TRUE ~ redlist_category
#   )) %>% 
#   select(scientific_name, redlist_category)

## THIS IS WHERE IT'S FILTERED 

no_cq = sim_results %>% 
  filter(scientific_name %in% iucn_data$scientific_name) %>% 
  mutate(mort_scenario = fct_relevel(as.factor(mort_scenario), "Low Mortality", after = Inf)) %>% 
  distinct() %>% 
  left_join(iucn_data) %>% 
  mutate(redlist_category = fct_relevel(as.factor(redlist_category), c("CR", "EN", "VU", "NT", "LC", "DD"))) 
  #arrange(redlist_category, scientific_name)

length(unique(no_cq$scientific_name)) #282

no_cq_sci <- no_cq %>% 
  distinct(scientific_name) %>% 
  pull()

p <- 
  ggplot() +
  geom_rect(data = no_cq, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.45, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~ factor(scientific_name,
                      levels = no_cq_sci),
             labeller = label_wrap_gen(15)) +
  theme_bw(base_size = 14) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976", "#91CF60", "#1A9850", "grey", "white")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(color = "black", face = "bold.italic"),
        axis.title = element_text(color = "black"),
        axis.text = element_text( color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("initial_sim.pdf"), path = here::here("figs", "supp"), height = 30, width = 30)

# Species Sub Plots -------------------------------------------------------

species_sub = c("Prionace glauca", "Carcharhinus limbatus", "Isurus oxyrinchus", "Squalus acanthias", "Alopias vulpinus", 
                "Pseudocarcharias kamoharai", "Carcharhinus falciformis", "Sphyrna mokarran",
                "Carcharhinus hemiodon", "Squatina squatina", "Sphyrna corona", "Galeorhinus galeus")

no_cq_sub = no_cq %>% 
  filter(scientific_name %in% species_sub) 

tag_text <- data.frame(t = c(180, 180, 180),
                       n_div_k = c(0.98, 0.98, 0.98),
                       label = c("A", "B", "C"),
                       scientific_name = factor(c("Prionace glauca", "Pseudocarcharias kamoharai", "Alopias vulpinus", "Carcharhinus falciformis",
                                  "Isurus oxyrinchus", "Squalus acanthias", "Carcharhinus hemiodon", "Sphyrna corona",
                                  "Squatina squatina", "Sphyrna mokarran", "Carcharhinus limbatus", "Galeorhinus galeus")))

p <- ggplot() +
  geom_hline(yintercept = 0.5,
             color = "gray",
             linetype = "dashed") +
  geom_rect(data = no_cq_sub,
            aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.23, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_sub, aes(t, n_div_k, color = mort_scenario, group = total_mort), linewidth = 2) +
  geom_text(data = tag_text %>% 
             filter(scientific_name %in% c("Prionace glauca", "Isurus oxyrinchus", "Squatina squatina")),
           aes(x = t, y = n_div_k, label = label),
           color = "black",
           fontface = "bold",
           size = 5) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap( ~ factor(scientific_name,
                      levels = c("Prionace glauca", "Pseudocarcharias kamoharai", "Squalus acanthias", "Carcharhinus falciformis",
                                 "Isurus oxyrinchus", "Alopias vulpinus", "Carcharhinus hemiodon", "Sphyrna corona",
                                  "Squatina squatina", "Sphyrna mokarran", "Carcharhinus limbatus", "Galeorhinus galeus"))) +
  theme_bw(base_size = 20) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976", "#91CF60", "#1A9850")) +
  labs(
    x = "Time",
    y = "N/K",
    fill = "IUCN Category",
    color = "Scenario"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = NA),
        strip.text.x = element_text(color = "black", face = "bold.italic"),
        axis.title = element_text( color = "black"),
        axis.text = element_text( color = "black")) +
  coord_cartesian(clip="off", ylim = c(0, 1))
p

ggsave(p, file = paste0("fig5_test.pdf"), path = here::here("figs"), height = 10, width = 20)


percent_calc = sim_results %>% 
  filter(scientific_name %in% iucn_data$scientific_name) %>% 
  mutate(f_mort = ((100 * f) - (100 * f * (1-mid_avm) * (1-mid_prm)))/100) %>% 
  select(scientific_name, f, f_mort, mid_avm, mid_prm) %>% 
  distinct() %>% 
  mutate(percent_diff = (f - f_mort) / f * 100) %>% 
  mutate(mean_diff = mean(percent_diff, na.rm=TRUE),
         sd_diff = sd(percent_diff, na.rm=TRUE))

species_of_interest = c("Galeocerdo cuvier", "Carcharhinus dussumieri",
                        "Carcharhinus longimanus", "Squatina japonica",
                        "Squatina oculata", "Centrophorus squamosus",
                        "Carcharhinus plumbeus", "Isurus paucus",
                        "Cetorhinus maximus", "Carcharhinus leucas",
                        "Squalus acanthias", "Negaprion brevirostris",
                        "Odontaspis ferox", "Carcharodon carcharias",
                        "Lamna nasus", "Carcharhinus brevipinna",
                        "Carcharhinus limbatus", "Squatina guggenheim",
                        "Sphyrna lewini", "Carcharhinus hemiodon", 
                        "Isogomphodon oxyrhynchus", "Prionace glauca",
                        "Hexanchus nakamurai", "Mitsukurina owstoni",
                        "Pseudocarcharias kamoharai", "Squalus blainville",
                        "Echinorhinus cookei", "Prionace glauca", "
                        Pseudocarcharias kamoharai", "Alopias vulpinus",
                        "Carcharhinus falciformis", "Isurus  oxyrinchus",
                        "Squalus acanthias",
                        "Carcharhinus hemiodon", "Sphyrna corona",
                        "Squatina squatina", "Sphyrna mokarran",
                        "Carcharhinus limbatus", "Galeorhinus galeus")

subset_percent = percent_calc %>% 
  filter(scientific_name %in% species_of_interest) %>% 
  select(scientific_name, percent_diff)

write_csv(subset_percent, here::here("data", "table1.1.csv"))
  
percent_over_30 = percent_calc %>% 
  filter(percent_diff >= 50)

41/282*100

sim_200 = sim_results %>% 
  filter(scientific_name %in% iucn_data$scientific_name) %>% 
  filter(t == 200) %>% 
  filter(mort_scenario == "Median Mortality") %>% 
  select(n_div_k, scientific_name)%>% 
  filter(scientific_name %in% species_sub) 

sim_over = sim_200 %>%
  filter(n_div_k >= 0.5)

sim_under = sim_200  %>%
  filter(n_div_k < 0.5)
