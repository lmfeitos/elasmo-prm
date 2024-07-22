library(tidyverse)
library(ggridges)
library(patchwork)
library(janitor)
library(ggh4x)
library(here)
library(ggpmisc)
library(tidymodels)
library(vip)
library(ggrepel)

set.seed(42)

# data load ---------------------------------------------------------------

# read in the raw literature review data
prm_elasmo <- read_csv(here("data", "prm_hypoxia.csv"))

# wrangle for the gillnet and longline data subsets
prm_elasmo_subset <- prm_elasmo %>%
  filter(!grepl("Rajidae", family)) %>%
  filter(family != "Dasyatidae") %>%
  filter(family != "Pristidae") %>%
  filter(family != "Rhinidae") %>%
  filter(family != "Rhinobatidae") %>%
  filter(family != "Myliobatidae") %>%
  mutate(habitat = case_when(
    habitat_associated == "pelagic" ~ "pelagic",
    habitat_associated == "pelagic-oceanic" ~ "pelagic",
    habitat_associated == "reef-associated" ~ "pelagic",
    habitat_associated == "bathypelagic" ~ "pelagic",
    habitat_associated == "benthopelagic" ~ "demersal",
    habitat_associated == "demersal" ~ "demersal",
    habitat_associated == "bathydemersal" ~ "demersal",
  )) %>%
  mutate(habitat = case_when(
    scientific_name == "Carcharhinus perezi" ~ "pelagic",
    TRUE ~ habitat
  )) %>%
  mutate(ventilation_method = case_when(
    scientific_name == "Ginglymostoma cirratum" ~ "stationary",
    TRUE ~ ventilation_method
  )) %>%
  mutate(reproductive_mode = case_when(
    scientific_name == "Hexanchus griseus" ~ "lecithotrophic viviparity",
    scientific_name == "Hexanchus nakamurai" ~ "lecithotrophic viviparity",
    scientific_name == "Ginglymostoma cirratum" ~ "lecithotrophic oviparity",
    TRUE ~ reproductive_mode
  )) %>%
  group_by(scientific_name) %>%
  filter(!grepl("sp", scientific_name)) %>%
  mutate(median_depth = mean(median_depth)) %>%
  mutate(max_size_cm = mean(max_size_cm)) %>%
  distinct()

## gillnet
elasmo_gillnet <- prm_elasmo_subset %>%
  filter(estimate_type == "at-vessel mortality") %>%
  filter(gear_class == "gillnet") %>%
  group_by(scientific_name, gear_class) %>%
  mutate(mortality_prop = weighted.mean(estimate, sample_size)) %>%
  select(ventilation_method, median_depth, max_size_cm, habitat, reproductive_mode, mortality_prop, family, ac) %>%
  distinct() %>%
  drop_na()

## longline
### AVM
elasmo_avm <- prm_elasmo_subset %>%
  filter(estimate_type == "at-vessel mortality") %>%
  filter(sample_size > 15) %>%
  group_by(scientific_name) %>%
  mutate(
    mortality_prop = weighted.mean(estimate, sample_size),
    avm_sd = sd(estimate)
  ) %>%
  # mutate(mortality_prop = estimate) %>%
  select(ventilation_method, median_depth, max_size_cm, habitat, reproductive_mode, mortality_prop, family, ac) %>%
  distinct() %>%
  drop_na()

avm_split <- initial_split(elasmo_avm, prop = .7, strata = family)

avm_train <- training(avm_split)
avm_test <- testing(avm_split)


### PRM
elasmo_prm <- prm_elasmo_subset %>%
  filter(estimate_type == "post-release mortality") %>%
  filter(sample_size > 5) %>%
  group_by(scientific_name) %>%
  mutate(
    mortality_prop = weighted.mean(estimate, sample_size),
    prm_sd = sd(estimate)
  ) %>%
  # mutate(mortality_prop = estimate) %>%
  select(ventilation_method, median_depth, max_size_cm, habitat, reproductive_mode, mortality_prop, family, ac) %>%
  distinct() %>%
  drop_na()

# read in the IUCN data
iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>%
  janitor::clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") |
    str_detect(scientific_name, "Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus|Stegostoma")) %>% # list of genera to keep in the filtering
  select(scientific_name, redlist_category) %>%
  mutate(redlist_category = case_when(
    str_detect(redlist_category, "Near") ~ "NT",
    str_detect(redlist_category, "Vul") ~ "VU",
    str_detect(redlist_category, "Data") ~ "DD",
    redlist_category == "Endangered" ~ "EN",
    redlist_category == "Critically Endangered" ~ "CR",
    str_detect(redlist_category, "Least") ~ "LC",
    TRUE ~ redlist_category
  )) %>%
  filter(!str_detect(scientific_name, "Parmaturus|Bythaelurus|Cirrhoscyllium|Proscyllium|Megachasma|Cetorhinus|Rhincodon "))

# read in taxonomic assigmnets of IUCN data
iucn_taxonomy <- read_csv(here("data", "iucn_data", "taxonomy.csv")) %>%
  clean_names() %>%
  mutate(family = str_to_sentence(family_name)) %>%
  select(family, genus_name, species_name) %>%
  unite(col = "scientific_name", c(genus_name, species_name), sep = " ")

# join IUCN assessement and taxonomic datasets and calcualte sp count per family and redlist category
iucn_join <- iucn_data %>%
  left_join(iucn_taxonomy, by = "scientific_name") %>%
  mutate(threat = ifelse(redlist_category %in% c("CR", "EN", "VU"), "threatened", "not threatened")) %>%
  group_by(family) %>%
  mutate(sp_count = n()) %>%
  ungroup() %>%
  group_by(family, threat) %>%
  summarise(sp_count_threat = n()) %>%
  ungroup() %>%
  group_by(family) %>%
  mutate(
    total_sp_count = sum(sp_count_threat),
    sp_threat_pct = sp_count_threat / total_sp_count
  )


# read in the AVM PRM model predictions
longline_predictions <- read_csv(here::here("data", "full_model_predictions.csv")) %>%
  filter(scientific_name %in% iucn_data$scientific_name)

# join IUCN and prediction data
longline_predictions_iucn <- longline_predictions %>%
  left_join(iucn_data, by = "scientific_name")

# read in ICUN assessments and filter to only longline
iucn_data_non_longline <- read_csv(here("data", "iucn_data", "assessments.csv")) %>%
  clean_names() %>%
  filter(str_detect(systems, "Marine") & !str_detect(threats, "longline"))

# read in ICUN assessments and filter to only gillnet
iucn_data_gill <- read_csv(here("data", "iucn_data", "assessments.csv")) %>%
  clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "gillnet")) %>%
  select(scientific_name, redlist_category) %>%
  mutate(redlist_category = case_when(
    str_detect(redlist_category, "Near") ~ "NT",
    str_detect(redlist_category, "Vul") ~ "VU",
    str_detect(redlist_category, "Data") ~ "DD",
    redlist_category == "Endangered" ~ "EN",
    redlist_category == "Critically Endangered" ~ "CR",
    str_detect(redlist_category, "Least") ~ "LC",
    TRUE ~ redlist_category
  ))

# AVM gillnet predictions
gillnet_predictions <- read_csv(here::here("data", "gillnet_model_predictions.csv")) %>%
  filter(scientific_name %in% iucn_data_gill$scientific_name)

gillnet_predictions_iucn <- gillnet_predictions %>%
  left_join(iucn_data_gill, by = "scientific_name") %>%
  select(-genus, -order)

full_predictions <- full_join(longline_predictions_iucn, gillnet_predictions_iucn)

# read in simulation results
sim_results <- read_csv(here::here("data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  filter(!is.na(mort_scenario)) %>%
  filter(t == 200) %>%
  filter(mort_scenario == "BAU" | mort_scenario == "Median Mortality") %>%
  mutate(f_mort = (100 - ((100 * (1 - f)) + (100 * f * (1 - mid_avm) * (1 - mid_prm)))) / 100) %>%
  select(scientific_name, f, f_mort, mid_avm, mid_prm) %>%
  distinct()

write_csv(sim_results, here::here("data", "pct_mort_reduction_sim.csv"))

sim_results_uncorrected <- read_csv(here::here("data", "uncorrected_results.csv")) %>%
  filter(scenario != "CQ") %>%
  filter(!is.na(mort_scenario)) %>%
  filter(t == 200) %>%
  filter(mort_scenario == "BAU" | mort_scenario == "Median Mortality") %>%
  mutate(f_mort = (100 - ((100 * (1 - f)) + (100 * f * (1 - mid_avm) * (1 - mid_prm)))) / 100) %>%
  select(scientific_name, f, f_mort, mid_avm, mid_prm) %>%
  distinct()

write_csv(sim_results_uncorrected, here::here("data", "pct_mort_reduction_sim_uncorrected.csv"))

f_vals <- read_csv(here::here("data", "ramldb_f_means.csv")) %>%
  rename(scientific_name = scientificname)

sim_results_2 <- read_csv(here::here("data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  filter(!is.na(mort_scenario))

top_shark_mort <- read_csv(here("data", "saup_eez_high_seas_shark_mortality.csv")) %>%
  group_by(scientific_name) %>%
  summarise(mort_sum = sum(mortality)) %>%
  arrange(desc(mort_sum))

# read in fishbase IUCN list
new_dat <- read_csv(here::here("data", "iucn_fishbase_list.csv")) %>%
  select(scientific_name, common_name)

# read in sim results with common name
sim_results_common <- read_csv(here::here("data", "f_sim_results.csv")) %>%
  select(scientific_name, common_name)

# fishing pressure sensitivity results
sim_results1 <- read_csv(here::here("data", "simulation_results_msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 1) %>%
  mutate(corrected = "yes")
sim_results1_5 <- read_csv(here::here("data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 1.5) %>%
  mutate(corrected = "yes")
sim_results2 <- read_csv(here::here("data", "simulation_results_2msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 2) %>%
  mutate(corrected = "yes")
sim_results3 <- read_csv(here::here("data", "simulation_results_3msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 3) %>%
  mutate(corrected = "yes")

uncorrected_results <- read_csv(here::here("data", "uncorrected_results.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 1.5) %>%
  mutate(corrected = "no")

# Figures 1 and S1 and S13 --------------------------------------------------------

# data wrangling for plotting raw data
prm_elasmo_subset <- prm_elasmo %>%
  select(
    scientific_name, gear_class, habitat_associated, estimate_type, estimate, sample_size,
    ventilation_method, max_size_cm, median_depth, reproductive_mode, family, group
  ) %>%
  mutate(max_size_cm = case_when( # filling missing information based on the average per group
    scientific_name == "Dasyatis sp" ~ 143.36,
    scientific_name == "Squalus sp" ~ 166.36,
    scientific_name == "Mustelus sp" ~ 167.24,
    scientific_name == "Centrophorus sp" ~ 165.75,
    scientific_name == "sharks" ~ 351.6828,
    scientific_name == "rays" ~ 164.261, # only calculated for the batoid species with disk width measures
    TRUE ~ max_size_cm
  )) %>%
  mutate(median_depth = case_when( # filling missing information based on the average per group
    scientific_name == "Dasyatis sp" ~ 117.63,
    scientific_name == "Squalus sp" ~ 767.786,
    scientific_name == "Mustelus sp" ~ 246.1,
    scientific_name == "Centrophorus sp" ~ 1136.25,
    scientific_name == "sharks" ~ 309.9474,
    scientific_name == "rays" ~ 99.87,
    TRUE ~ median_depth
  )) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(estimate = case_when(
    estimate_type == "post-release survival" ~ 1 - estimate, # Converts post-release survival into PRM
    TRUE ~ estimate
  )) %>%
  mutate(estimate_type = case_when(
    estimate_type == "post-release survival" ~ "post-release mortality",
    TRUE ~ estimate_type
  ))

# Create subset of the whole dataset with at-vessel mortality and calculate the weighted averages per species
mort_summary <- prm_elasmo_subset %>%
  group_by(scientific_name, estimate_type) %>%
  mutate(mortality_prop = weighted.mean(estimate, sample_size)) %>%
  ungroup() %>%
  mutate(
    prm = ifelse(estimate_type == "post-release mortality", mortality_prop, NA),
    avm = ifelse(estimate_type == "at-vessel mortality", mortality_prop, NA)
  ) %>%
  mutate(habitat = case_when(
    str_detect(habitat_associated, "pelagic") ~ "pelagic",
    habitat_associated == "reef-associated" ~ "pelagic",
    str_detect(habitat_associated, "demersal") ~ "demersal"
  ))

# get weighted average mortality for each mortality type
# sharks
mort_summary_subset <- mort_summary %>%
  filter(group == "Sharks" & gear_class %in% c("longline", "gillnet", "trawl")) %>%
  group_by(estimate_type, gear_class) %>%
  summarise(mean = mean(mortality_prop))

# batoids
mort_summary_subset_bat <- mort_summary %>%
  filter(group == "Batoids" & gear_class %in% c("longline", "gillnet", "trawl")) %>%
  group_by(estimate_type, gear_class) %>%
  summarise(mean = mean(mortality_prop))

# family boxplot subset
boxplot_subset <-
  mort_summary %>%
  group_by(family, estimate_type, gear_class) %>%
  mutate(n = n()) %>%
  filter(n > 2) %>%
  ungroup()

# habitat associated boxplot subset
habitat_subset <-
  mort_summary %>%
  mutate(habitat = case_when(
    str_detect(habitat_associated, "pelagic") ~ "pelagic",
    habitat_associated == "reef-associated" ~ "pelagic",
    str_detect(habitat_associated, "demersal") ~ "demersal"
  )) %>%
  group_by(habitat, estimate_type, gear_class) %>%
  mutate(n = n()) %>%
  filter(n > 2)

mort_proportions_plot_fam <-
  ggplot() +
  geom_point(
    data = mort_summary %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      drop_na(family) %>%
      mutate(family = as.factor(family)) %>%
      arrange(desc(family)),
    aes(
      x = fct_inorder(family),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1),
    show.legend = F
  ) +
  geom_boxplot(
    data = boxplot_subset %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      drop_na(family) %>%
      mutate(family = as.factor(family)) %>%
      arrange(desc(family)),
    aes(
      x = fct_inorder(family),
      y = mortality_prop,
      fill = estimate_type
    ),
    show.legend = F,
    alpha = 0.5,
    fatten = 1
  ) +
  geom_hline(
    data = mort_summary_subset %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ),
    aes(yintercept = mean, color = estimate_type),
    show.legend = F,
    linetype = "dashed",
    alpha = 0.7,
    linewidth = .9
  ) +
  facet_wrap(~ factor(gear_class,
    levels = c("Longline", "Gillnet", "Trawl")
  )) +
  labs(
    x = "",
    y = "",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

habitat_plot <-
  ggplot() +
  geom_point(
    data = mort_summary %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        !is.na(habitat) &
        group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat = str_to_title(habitat),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat = as.factor(habitat)) %>%
      arrange(desc(habitat)),
    aes(
      x = fct_inorder(habitat),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1)
  ) +
  geom_boxplot(
    data = habitat_subset %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        !is.na(habitat) &
        group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat = str_to_title(habitat),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat = as.factor(habitat)) %>%
      arrange(desc(habitat)),
    aes(
      x = fct_inorder(habitat),
      y = mortality_prop,
      fill = estimate_type
    ),
    alpha = 0.5,
    fatten = 1
  ) +
  geom_hline(
    data = mort_summary_subset %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ),
    aes(yintercept = mean, color = estimate_type),
    show.legend = F,
    linetype = "dashed",
    alpha = 0.7,
    linewidth = .9
  ) +
  facet_wrap(~ factor(gear_class,
    levels = c("Longline", "Gillnet", "Trawl")
  )) +
  labs(
    x = "",
    y = "Mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

mort_proportions_plot <-
  mort_proportions_plot_fam /
    habitat_plot +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

mort_proportions_plot

ggsave(mort_proportions_plot, file = paste0("fig1.pdf"), path = here::here("figs"), height = 12, width = 12)

## Figure S1

mort_proportions_plot_fam_bat <-
  ggplot() +
  geom_point(
    data = mort_summary %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        group != "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      drop_na(family) %>%
      mutate(family = as.factor(family)) %>%
      arrange(desc(family)),
    aes(
      x = fct_inorder(family),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1),
    show.legend = F
  ) +
  geom_boxplot(
    data = boxplot_subset %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        group != "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      drop_na(family) %>%
      mutate(family = as.factor(family)) %>%
      arrange(desc(family)),
    aes(
      x = fct_inorder(family),
      y = mortality_prop,
      fill = estimate_type
    ),
    show.legend = F,
    alpha = 0.5,
    fatten = 1
  ) +
  geom_hline(
    data = mort_summary_subset_bat %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ),
    aes(yintercept = mean, color = estimate_type),
    show.legend = F,
    linetype = "dashed",
    alpha = 0.7,
    linewidth = .9
  ) +
  facet_wrap(~
    factor(gear_class,
      levels = c("Longline", "Gillnet", "Trawl")
    )) +
  labs(
    x = "",
    y = "",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

habitat_bat <- habitat_subset %>%
  filter(gear_class %in% c("longline", "trawl", "gillnet") &
    !is.na(habitat) & group != "Sharks") %>%
  distinct(scientific_name, estimate_type, habitat, mortality_prop)

habitat_plot_bat <-
  ggplot() +
  geom_point(
    data = mort_summary %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        !is.na(habitat_associated) &
        group != "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat = str_to_title(habitat),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat = as.factor(habitat)) %>%
      arrange(desc(habitat)),
    aes(
      x = fct_inorder(habitat),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1)
  ) +
  geom_boxplot(
    data = habitat_bat %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
        !is.na(habitat)) %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat = str_to_title(habitat),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat = as.factor(habitat)) %>%
      arrange(desc(habitat)),
    aes(
      x = fct_inorder(habitat),
      y = mortality_prop,
      fill = estimate_type
    ),
    alpha = 0.5,
    fatten = 1
  ) +
  geom_hline(
    data = mort_summary_subset_bat %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ),
    aes(yintercept = mean, color = estimate_type),
    show.legend = F,
    linetype = "dashed",
    alpha = 0.7,
    linewidth = .9
  ) +
  facet_wrap(~ factor(gear_class,
    levels = c("Longline", "Gillnet", "Trawl")
  )) +
  labs(
    x = "",
    y = "Mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )


mort_proportions_plot_bat <-
  mort_proportions_plot_fam_bat /
    habitat_plot_bat +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

mort_proportions_plot_bat

ggsave(mort_proportions_plot_bat, file = paste0("figS1.pdf"), path = here::here("figs", "supp"), height = 15, width = 15)

# Figure S14
obs_count_plot <-
  prm_elasmo_subset %>%
  filter(group == "Sharks") %>%
  group_by(scientific_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(scientific_name = as.factor(scientific_name)) %>%
  slice_head(n = 20) %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(scientific_name, n), y = n),
    fill = "blue4",
    alpha = .7,
    width = .8,
    color = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 150)
  ) +
  labs(
    x = "",
    y = "Number of observations per species"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.y = element_text(color = "black", size = 10, face = "italic"),
    axis.text.x = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(obs_count_plot, file = paste0("figS13.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)

# Figure 2
# function to preform and extract quantile estimates from random forest
preds_bind <- function(data_fit, rf_fit) {
  predict(
    rf_fit$fit$fit$fit,
    workflows::extract_recipe(rf_fit) %>% bake(data_fit),
    type = "quantiles",
    quantiles = c(.05, .25, .5, .75, .95)
  ) %>%
    with(predictions) %>%
    as_tibble() %>%
    set_names(paste0(".pred", c("_05", "_25", "_50", "_75", "_95"))) %>%
    bind_cols(data_fit)
}

gillnet_final <- readRDS(here::here("data", "gillnet_final.rds"))

gillnet_fit <- fit(gillnet_final, elasmo_gillnet) # fit the data to the training data

gillnet_train_predict <- predict(object = gillnet_fit, new_data = elasmo_gillnet) %>% # predict the training set
  bind_cols(elasmo_gillnet) # bind training set column to prediction

gill_train_metrics <- gillnet_train_predict %>%
  metrics(mortality_prop, .pred) # get testing data metrics

gillnet_quant_test <- preds_bind(gillnet_train_predict, gillnet_fit) %>%
  mutate(
    range_50 = .pred_75 - .pred_25,
    range_95 = .pred_95 - .pred_05
  )

p2_gill <- ggplot(gillnet_train_predict, aes(x = mortality_prop, y = .pred)) + # plot ln of real versus ln of predicted
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label("eq")) +
  stat_poly_eq(label.y = 0.9) +
  theme_bw(base_size = 16) +
  labs(
    x = "Observed AVM Rate (Gillnet)",
    y = "Predicted AVM Rate (Gillnet)"
  )

v2_gill <- gillnet_fit %>%
  extract_fit_parsnip() %>%
  vip() +
  theme_bw(base_size = 16) +
  labs(y = "Variable Importance for AVM (Gillnet)") +
  scale_x_discrete(labels = c("Hypoxia Tolerance", "Ventilation Method", "Maximum Size (cm)", "Median Depth (m)", "Reproductive mode"))

q2_gill <-
  ggplot(gillnet_quant_test) +
  geom_boxplot(aes(range_50)) +
  theme_bw(base_size = 16) +
  labs(x = "Distribution of IQR for AVM (Gillnet)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

## Longline
### AVM
avm_final <- readRDS(here::here("data", "avm_final.rds"))

avm_fit <- fit(avm_final, avm_train) # fit the data to the training data

avm_train_predict <- predict(object = avm_fit, new_data = avm_train) %>% # predict the training set
  bind_cols(avm_train) # bind training set column to prediction

avm_test_predict <- predict(object = avm_fit, new_data = avm_test) %>% # predict the training set
  bind_cols(avm_test) # bind prediction to testing data column

avm_data <- full_join(avm_test_predict, avm_train_predict)

# predict quantiles
avm_quant_test <- preds_bind(avm_data, avm_fit) %>%
  mutate(
    range_50 = .pred_75 - .pred_25,
    range_95 = .pred_95 - .pred_05
  )

p1 <- ggplot(avm_quant_test, aes(x = mortality_prop, y = .pred)) + # plot ln of real versus ln of predicted
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label("eq")) +
  stat_poly_eq(label.y = 0.9) +
  theme_bw(base_size = 16) +
  labs(
    x = "Observed AVM Rate (Longline)",
    y = "Predicted AVM Rate (Longline)"
  )

q1 <- ggplot(avm_quant_test) +
  geom_boxplot(aes(range_50)) +
  theme_bw(base_size = 16) +
  labs(x = "Distribution of IQR for AVM (Longline)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

v1 <- avm_fit %>%
  extract_fit_parsnip() %>%
  vip() +
  theme_bw(base_size = 16) +
  labs(y = "Variable Importance for AVM (Longline)") +
  scale_x_discrete(labels = c("Ventilation Method", "Reproductive Mode", "Hypoxia Tolerance", "Median Depth (m)"))

## PRM
prm_final <- readRDS(here::here("data", "prm_final.rds"))

prm_fit <- fit(prm_final, elasmo_prm) # fit the data to the training data

prm_train_predict <- predict(object = prm_fit, new_data = elasmo_prm) %>% # predict the training set
  bind_cols(elasmo_prm) # bind training set column to prediction

prm_train_metrics <- prm_train_predict %>%
  metrics(mortality_prop, .pred) # get testing data metrics

prm_quant_test <- preds_bind(prm_train_predict, prm_fit) %>%
  mutate(
    range_50 = .pred_75 - .pred_25,
    range_95 = .pred_95 - .pred_05
  )

p2 <- ggplot(prm_train_predict, aes(x = mortality_prop, y = .pred)) + # plot ln of real versus ln of predicted
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label("eq")) +
  stat_poly_eq(label.y = 0.9) +
  theme_bw(base_size = 16) +
  labs(
    x = "Observed PRM Rate",
    y = "Predicted PRM Rate"
  )

v2 <- prm_fit %>%
  extract_fit_parsnip() %>%
  vip() +
  theme_bw(base_size = 16) +
  labs(y = "Variable Importance for PRM") +
  scale_x_discrete(labels = c("Habitat", "Reproductive Mode", "Hypoxia Tolerance", "Median Depth", "Maximum Size (cm)"))


q2 <- ggplot(prm_quant_test) +
  geom_boxplot(aes(range_50)) +
  theme_bw(base_size = 16) +
  labs(x = "Distribution of IQR for PRM") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot1 <- (p2_gill + v2_gill + q2_gill) / (p1 + v1 + q1) / (p2 + v2 + q2) + plot_annotation(tag_levels = "A") &
  theme(panel.grid = element_blank())

ggsave(plot1, file = paste0("fig2.pdf"), path = here::here("figs"), height = 14, width = 18)

# Figures 3 and S2 and S3 --------------------------------------------------------

# get the mean of predictions for each species
mean_predictions <- longline_predictions %>%
  group_by(scientific_name) %>%
  mutate(
    avm_pred_long = mean(avm_pred),
    prm_pred_long = mean(prm_pred)
  ) %>%
  mutate(gear = "longline")

# gillnet predictions
mean_gill_predictions <- gillnet_predictions %>%
  group_by(scientific_name) %>%
  mutate(avm_gill_long = mean(avm_gillnet_pred)) %>%
  mutate(gear = "gillnet") %>%
  select(-order, -genus)

# join longline and gillnet predictions
predictions_join <- mean_predictions %>%
  full_join(mean_gill_predictions) %>%
  pivot_longer(
    cols = c("avm_pred_long", "avm_gill_long", "prm_pred_long"),
    names_to = "estimate_type",
    values_to = "mortality_prop"
  ) %>%
  mutate(estimate_type = case_when(
    estimate_type == "avm_pred_long" ~ "AVM Longline",
    estimate_type == "avm_gill_long" ~ "AVM Gillnet",
    estimate_type == "prm_pred_long" ~ "PRM Longline"
  )) %>%
  select(scientific_name, family, reproductive_mode, ac, median_depth, max_size_cm, ventilation_method, habitat, estimate_type, mortality_prop) %>%
  mutate(gear_class = ifelse(estimate_type %in% c("AVM Longline", "PRM Longline"), "Longline", "Gillnet")) %>%
  mutate(estimate_type_new = ifelse(str_detect(estimate_type, "AVM"), "AVM", "PRM")) %>%
  filter(!is.na(mortality_prop))

write_csv(predictions_join, file = here("data", "tableS5.csv"), na = "")

count_avm <- predictions_join %>%
  filter(estimate_type %in% c("AVM Longline", "AVM Gillnet")) %>%
  select(scientific_name) %>%
  distinct()

count_avm_longline <- predictions_join %>%
  filter(estimate_type == "AVM Longline") %>%
  select(scientific_name) %>%
  distinct()

count_prm_longline <- predictions_join %>%
  filter(estimate_type == "PRM Longline") %>%
  select(scientific_name) %>%
  distinct()

count_avm_gillnet <- predictions_join %>%
  filter(estimate_type == "AVM Gillnet") %>%
  select(scientific_name) %>%
  distinct()

# get family level estimates for AVM
mort_subset_avm <- mean_predictions %>%
  group_by(family) %>%
  summarize(fam_mean = mean(avm_pred)) %>%
  distinct()

# get family level estimates for PRM
mort_subset_prm <- mean_predictions %>%
  group_by(family) %>%
  summarize(fam_mean = mean(prm_pred)) %>%
  arrange(desc(fam_mean)) %>%
  distinct(family) %>%
  pull(family)

# get family level estimates for PRM
mort_subset_gill <- mean_gill_predictions %>%
  group_by(family) %>%
  summarize(fam_mean = mean(avm_gillnet_pred)) %>%
  filter(!(family %in% mort_subset_avm$family)) %>%
  distinct()

mort_avm <- full_join(mort_subset_avm, mort_subset_gill) %>%
  arrange(fam_mean) %>%
  pull(family)

p6 <- ggplot() +
  geom_density_ridges(
    data = predictions_join,
    aes(
      x = mortality_prop, y = fct_rev(factor(family, levels = mort_avm)),
      fill = fct_rev(factor(family, levels = mort_avm))
    ),
    alpha = 0.5
  ) +
  geom_point(
    data = predictions_join,
    aes(
      x = mortality_prop, y = fct_rev(factor(family, levels = mort_avm)),
      color = fct_rev(factor(family, levels = mort_avm))
    ),
    alpha = 0.5, size = 6
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  facet_grid(
    ~ factor(estimate_type,
      levels = c("AVM Gillnet", "AVM Longline", "PRM Longline")
    ),
    scales = "free_x",
    space = "free_x"
  ) +
  labs(
    y = "Family",
    x = "Estimated Mortality"
  ) +
  scale_shape(guide = "none") +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(color = "black"),
    # axis.text.y = element_text(face = "italic"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm"),
    legend.position = "none"
  )

ggsave(p6, file = paste0("fig3.pdf"), path = here::here("figs"), height = 22, width = 20)

p1 <- ggplot(predictions_join %>% filter(estimate_type %in% c("PRM Longline", "AVM Gillnet"))) +
  geom_point(aes(max_size_cm, mortality_prop, color = gear_class),
    alpha = 0.5, size = 4
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Estimated Mortality",
    x = "Maximum size (cm)",
    color = "Group"
  ) +
  facet_wrap(~estimate_type,
    scales = "free_x"
  ) +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF")) +
  theme(legend.position = "none")

p2 <- ggplot(predictions_join) +
  geom_point(aes(median_depth, mortality_prop, color = gear_class),
    alpha = 0.5, size = 4
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Estimated Mortality",
    x = "Median Depth (m)",
    color = "Group"
  ) +
  facet_wrap(~estimate_type, scales = "free_x") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF"))

p6 <- ggplot(predictions_join) +
  geom_point(aes(ac, mortality_prop, color = gear_class),
    alpha = 0.5, size = 4
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Estimated Mortality",
    x = "Active Hypoxia Tolerance",
    color = "Group"
  ) +
  facet_wrap(~estimate_type, scales = "free_x") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm"),
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF")) +
  scale_x_log10()

p4 <- ggplot(predictions_join %>% mutate(habitat = str_to_sentence(habitat)) %>% filter(str_detect(estimate_type, "PRM"))) +
  geom_point(aes(mortality_prop, habitat, color = gear_class),
    alpha = 0.1,
    show.legend = F, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, habitat, fill = gear_class),
    outlier.alpha = 0,
    alpha = 0.6,
    show.legend = F
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Associated Habitat",
    x = "Estimated Mortality",
    color = "Group"
  ) +
  facet_wrap(~estimate_type, scales = "free_x") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF")) +
  scale_fill_manual(values = c("#440154FF", "#22A884FF")) +
  theme(legend.position = "none")

p5 <- ggplot(predictions_join %>% mutate(reproductive_mode = str_to_sentence(reproductive_mode))) +
  geom_point(aes(mortality_prop, reproductive_mode, color = gear_class),
    position = position_jitterdodge(jitter.width = .1),
    alpha = 0.1,
    show.legend = F, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, reproductive_mode, fill = gear_class),
    outlier.alpha = 0,
    alpha = 0.6,
    show.legend = F
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Reproductive Mode",
    x = "Estimated Mortality",
    color = "Group"
  ) +
  facet_wrap(~estimate_type, scales = "free_x") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF")) +
  scale_fill_manual(values = c("#440154FF", "#22A884FF"))

p7 <- ggplot(predictions_join %>% mutate(ventilation_method = str_to_sentence(ventilation_method)) %>% filter(str_detect(estimate_type, "AVM"))) +
  geom_point(aes(mortality_prop, ventilation_method, color = gear_class),
    position = position_jitterdodge(jitter.width = .1),
    alpha = 0.1,
    show.legend = F, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, ventilation_method, fill = gear_class),
    outlier.alpha = 0,
    alpha = 0.6,
    show.legend = F
  ) +
  theme_bw(base_size = 16) +
  labs(
    y = "Ventilation Method",
    x = "Estimated Mortality",
    color = "Group"
  ) +
  facet_wrap(~estimate_type, scales = "free_x") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  scale_color_manual(values = c("#440154FF", "#22A884FF")) +
  scale_fill_manual(values = c("#440154FF", "#22A884FF"))

plot <- p2 / p6 / p1 / p5 / p7 / p4 +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(plot, file = paste0("figS2.pdf"), path = here::here("figs", "supp"), height = 25, width = 15)

IQR_analysis <- full_predictions %>%
  mutate(
    range_50_prm = prm_75 - prm_25,
    range_50_avm = avm_75 - avm_25
  ) %>%
  select(range_50_avm, range_50_prm, scientific_name, ventilation_method, median_depth, max_size_cm, habitat, reproductive_mode, ac, family, avm_pred, prm_pred, avm_mort, prm_mort) %>%
  distinct() %>%
  mutate(IQR_cat_avm = case_when(
    range_50_avm <= quantile(range_50_avm, probs = 0.25, na.rm = TRUE) ~ "Low Uncertainty",
    range_50_avm >= quantile(range_50_avm, probs = 0.75, na.rm = TRUE) ~ "High Uncertainty",
    TRUE ~ "Medium Uncertainty"
  )) %>%
  mutate(IQR_cat_avm = fct_relevel(as.factor(IQR_cat_avm), c("Low Uncertainty", "Medium Uncertainty", "High Uncertainty"))) %>%
  mutate(IQR_cat_prm = case_when(
    range_50_prm <= quantile(range_50_prm, probs = 0.25, na.rm = TRUE) ~ "Low Uncertainty",
    range_50_prm >= quantile(range_50_prm, probs = 0.75, na.rm = TRUE) ~ "High Uncertainty",
    TRUE ~ "Medium Uncertainty"
  )) %>%
  mutate(IQR_cat_prm = fct_relevel(as.factor(IQR_cat_prm), c("Low Uncertainty", "Medium Uncertainty", "High Uncertainty")))

fam_level <- IQR_analysis %>%
  group_by(family) %>%
  summarise(fam_mean = mean(avm_pred, na.rm = TRUE)) %>%
  distinct() %>%
  arrange(fam_mean) %>%
  filter(!is.na(fam_mean)) %>%
  pull(family)

p1 <- ggplot(data = IQR_analysis, aes(avm_mort, avm_pred)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label("eq")) +
  stat_poly_eq(label.y = 0.9) +
  theme_bw(base_size = 16) +
  labs(
    x = "Observed PRM Rate",
    y = "Predicted PRM Rate"
  ) +
  facet_wrap(~IQR_cat_avm) +
  theme(
    axis.text = element_text(color = "black"),
    # axis.text.y = element_text(face = "italic"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

p2 <- ggplot(data = IQR_analysis %>% filter(family %in% fam_level), aes(avm_pred, fct_rev(factor(family, levels = fam_level)))) +
  geom_point(aes(color = IQR_cat_avm)) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  labs(
    x = "Predicted Longline AVM",
    y = "Family",
    color = "Uncertainty"
  ) +
  theme(
    axis.text = element_text(color = "black"),
    # axis.text.y = element_text(face = "italic"),
    axis.title = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  ) +
  theme(legend.position = "none")

p3 <- ggplot(data = IQR_analysis, aes(median_depth, avm_pred)) +
  geom_point(aes(color = IQR_cat_avm)) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  labs(
    x = "Median Depth",
    y = "Predicted Longline AVM",
    color = "Uncertainty"
  ) +
  theme(legend.position = "none")

# ggplot(data = IQR_analysis, aes(ac, avm_pred)) +
#   geom_point(aes(color = IQR_cat_avm)) +
#   theme_bw() +
#   scale_color_viridis_d()

p4 <- ggplot(data = IQR_analysis, aes(reproductive_mode)) +
  geom_bar(aes(color = IQR_cat_avm, fill = IQR_cat_avm), position = "fill") +
  theme_bw(base_size = 16) +
  coord_flip() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(
    y = "Proportion",
    x = "Reproductive Mode",
    color = "Uncertainty",
    fill = "Uncertainty"
  )

p5 <- ggplot(data = IQR_analysis, aes(ventilation_method)) +
  geom_bar(aes(color = IQR_cat_avm, fill = IQR_cat_avm), position = "fill") +
  coord_flip() +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(
    y = "Proportion",
    x = "Ventilation Method",
    color = "Uncertainty",
    fill = "Uncertainty"
  )

plot <- p2 / p1 / (p3 / p4 / p5) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")

ggsave(plot, file = paste0("figS3.pdf"), path = here::here("figs", "supp"), height = 25, width = 20)

# Figures 4 and S4 --------------------------------------------------------

iucn_data <- iucn_data %>%
  left_join(iucn_taxonomy, by = "scientific_name")

# data wrangling for the plot
sim_results_iucn <- sim_results %>%
  filter(scientific_name %in% iucn_data$scientific_name) %>%
  left_join(iucn_data, by = "scientific_name") %>%
  left_join(sim_results_common)

sim_results_top <- left_join(top_shark_mort, sim_results_iucn) %>%
  filter(!is.na(f))

sim_results_iucn_pct <- sim_results_iucn %>%
  mutate(
    percent_diff = (f - f_mort) / abs(f) * 100,
    abs_diff = f - f_mort,
    avm_prm = mid_avm + mid_prm
  ) %>%
  mutate(percent_diff = round(percent_diff, 4))

write_csv(sim_results_iucn_pct, here::here("data", "sim_results_pct_diff.csv"))

bins <- quantile(sim_results_iucn_pct$avm_prm, probs = c(0.33, 0.66))

sim_results_iucn_pct <- sim_results_iucn_pct %>%
  mutate(diff_bin = case_when(
    avm_prm < bins[1] ~ "Low Mortality",
    avm_prm >= bins[1] & avm_prm <= bins[2] ~ "Medium Mortality",
    avm_prm > bins[2] ~ "High Mortality"
  )) %>%
  mutate(diff_bin = fct_relevel(as.factor(diff_bin), c("High Mortality", "Medium Mortality", "Low Mortality")))

# bin_count = sim_results_iucn_pct %>%
#   group_by(diff_bin) %>%
#   summarize(count = n())
#
# ggplot() +
#   geom_vline(aes(xintercept = bins))+
#   geom_density(data = sim_results_iucn_pct, aes(avm_prm)) +
#   theme_bw()

f_val_sim <- left_join(f_vals, sim_results_iucn_pct) %>%
  mutate(
    f_fmsy_5 = mean_f_5 / f,
    f_fmsy_10 = mean_f_10 / f
  ) %>%
  mutate(
    f_reduce_5 = mean_f_5 - mean_f_5 * (percent_diff / 100),
    f_reduce_10 = mean_f_10 - mean_f_10 * (percent_diff / 100)
  ) %>%
  mutate(success_5 = case_when(
    f_reduce_5 <= f / 1.5 ~ "yes",
    f_reduce_5 > f / 1.5 ~ "no"
  )) %>%
  mutate(success_10 = case_when(
    f_reduce_10 <= f / 1.5 ~ "yes",
    f_reduce_10 > f / 1.5 ~ "no"
  )) %>%
  mutate(f_ratio_5 = case_when(
    f_fmsy_5 >= 1 ~ "Originally Overfished",
    f_fmsy_5 < 1 ~ "Not originally overfished"
  )) %>%
  mutate(f_ratio_10 = case_when(
    f_fmsy_10 >= 1 ~ "Originally Overfished",
    f_fmsy_10 < 1 ~ "Not originally overfished"
  ))

write_csv(f_val_sim, here::here("data", "f_reduce_f_msy_compare.csv"))

# create data subset with threatened species only
sim_results_iucn_pct_threat <- sim_results_iucn_pct %>%
  filter(redlist_category %in% c("VU", "EN", "CR")) %>%
  rowid_to_column("id") %>%
  mutate(id = fct_reorder(as.factor(id), percent_diff)) %>%
  mutate(redlist_category = as.factor(redlist_category)) %>%
  arrange(id) %>%
  select(-scientific_name) %>%
  rename(scientific_name = common_name)

# create data subset with non-threatened species only
non_threat <- sim_results_iucn_pct %>%
  filter(redlist_category %in% c("DD", "NT", "LC")) %>%
  rowid_to_column("id") %>%
  mutate(id = fct_reorder(as.factor(id), percent_diff)) %>%
  mutate(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    redlist_category = as.factor(redlist_category),
    diff_bin = as.factor(diff_bin)
  ) %>%
  arrange(id) %>%
  select(-scientific_name)

# calculate average mortality reductions per threat status
threatended <- sim_results_iucn_pct %>%
  filter(redlist_category %in% c("VU", "EN", "CR")) %>%
  mutate(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE)
  )

non_threat_mean <- non_threat %>%
  filter(redlist_category %in% c("DD", "NT", "LC")) %>%
  mutate(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE)
  )

sim_results_iucn_pct_sum_stats <- sim_results_iucn_pct %>%
  group_by(redlist_category) %>%
  summarise(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    sd_percent_diff = sd(percent_diff, na.rm = TRUE),
    var_percent_diff = var(percent_diff, na.rm = TRUE)
  )

sim_results_bin_stats <- sim_results_iucn_pct %>%
  group_by(diff_bin) %>%
  summarise(
    mean_percent_diff = mean(abs_diff, na.rm = TRUE),
    sd_percent_diff = sd(abs_diff, na.rm = TRUE),
    var_percent_diff = var(abs_diff, na.rm = TRUE)
  )

# get average mortality reductions per iucn status
sim_results_iucn_pct_plot_m_ave <- sim_results_iucn_pct %>%
  group_by(redlist_category) %>%
  summarise(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    sd_percent_diff = sd(percent_diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    sd_abs_diff = sd(abs_diff, na.rm = TRUE)
  ) %>%
  mutate(
    mean_abs_diff = mean_abs_diff * 100,
    sd_abs_diff = sd_abs_diff * 100
  )

# get average relative mortality reductions per main fished family
diff_fam <- sim_results_iucn_pct %>%
  group_by(family) %>%
  summarise(mean_diff = mean(percent_diff))

# get average absolute mortality reductions per main fished family
diff_fam_abs <- sim_results_iucn_pct %>%
  group_by(family) %>%
  summarise(mean_diff = mean(abs_diff))

# # Set a number of empty bars to add at the end of each group
# empty_bar <- 7
# to_add <- data.frame(matrix(NA, empty_bar * nlevels(sim_results_iucn_pct_threat$diff_bin), ncol(sim_results_iucn_pct_threat)))
# colnames(to_add) <- colnames(sim_results_iucn_pct_threat)
# to_add$diff_bin <- rep(levels(sim_results_iucn_pct_threat$diff_bin), each = empty_bar)
# sim_results_iucn_pct_threat <- rbind(sim_results_iucn_pct_threat, to_add)
# sim_results_iucn_pct_threat <- sim_results_iucn_pct_threat %>% arrange(diff_bin)
# sim_results_iucn_pct_threat$id <- seq(1, nrow(sim_results_iucn_pct_threat))
#
# # get the name and the y position of each label
# label_data_m <- sim_results_iucn_pct_threat
# number_of_bar <- nrow(label_data_m)
# angle <- 90 - 360 * (label_data_m$id - 0.5) / number_of_bar
# label_data_m$hjust <- ifelse(angle < -90, 1, 0)
# label_data_m$angle <- ifelse(angle < -90, angle + 180, angle)
#
# # Not sure how to use the code below
# redlist_breaks <- sim_results_iucn_pct_threat %>%
#   group_by(diff_bin) %>%
#   summarize(
#     start = min(id),
#     end = max(id) - 3
#   ) %>%
#   rowwise() %>%
#   mutate(title = mean(c(start, end))) %>%
#   ungroup() %>%
#   mutate(
#     end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1),
#     start = start - 1
#   )
#
# max_value <- max(sim_results_iucn_pct_threat$percent_diff, na.rm = T)
#
# y_max <- 20 * ceiling(max_value / 20)
#
# v <- c(20, 40, 60, 80, 100)
#
# redlist_breaks <- redlist_breaks %>%
#   mutate(v = list(v)) %>%
#   unnest(cols = c(v))
#
# prop1 <-
#   ggplot(data = sim_results_iucn_pct_threat) +
#   geom_segment(
#     data = redlist_breaks %>%
#       filter(v != 100),
#     aes(x = end, y = v, xend = start, yend = v),
#     color = "grey", linewidth = 0.3, inherit.aes = FALSE
#   ) +
#   annotate("text",
#     x = rep(max(sim_results_iucn_pct_threat$id, length(v))),
#     y = v - 5, label = paste0(head(v), "%"), color = "grey", size = 5, angle = 0, fontface = "bold", hjust = 0.7
#   ) +
#   geom_col(
#     aes(
#       x = id,
#       y = percent_diff,
#       fill = diff_bin
#     ),
#     show.legend = F,
#     width = 0.8
#   ) +
#   ylim(-40, NA) +
#   scale_fill_viridis_d() + # manual(values = c("#E31A1C", "#FD8D3C", "#FED976")) +
#   theme(
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(0, 4), "cm")
#   ) +
#   coord_polar(start = 0) +
#   geom_text(
#     data = label_data_m,
#     aes(x = id, y = percent_diff + 20, label = scientific_name, angle = angle),
#     nudge_x = -0.25, nudge_y = 0.25,
#     color = "black", size = 4, inherit.aes = FALSE
#   ) +
#   geom_segment(
#     data = redlist_breaks,
#     aes(x = start, y = -5, xend = end, yend = -5),
#     colour = "black", alpha = 0.8, inherit.aes = FALSE
#   ) +
#   geom_text(
#     data = redlist_breaks,
#     aes(x = title, y = -18, label = diff_bin, color = diff_bin),
#     show.legend = F,
#     hjust = 0.5, alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE
#   ) +
#   scale_color_viridis_d() + # (values = c("#E31A1C", "#FD8D3C", "#FED976")) +
#   # geom_text(data = redlist_breaks,
#   #             aes(x = title, y = 48, label = redlist_category),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE) +
#   labs(fill = "Redlist Category") + # y = 80
#   theme_void(base_size = 16) +
#   labs(
#     x = "",
#     y = ""
#   )
#
lolli_data1 <- threatended %>%
  select(scientific_name, f, f_mort, avm_prm, abs_diff, diff_bin, common_name, percent_diff) %>%
  mutate(id = fct_reorder(as.factor(common_name), abs_diff))

# lolli1 <- ggplot(lolli_data1, aes(x = id, y = abs_diff)) +
#   geom_segment(aes(xend = id, yend = 0)) +
#   geom_point(aes(color = percent_diff), size = 2) +
#   coord_flip() +
#   theme_bw(base_size = 16) +
#   labs(
#     x = "Species",
#     y = "Absolute Difference in Mortality",
#     color = "Percent Difference in Mortality"
#   ) +
#   scale_color_viridis_c()
# lolli1

empty_bar <- 7
to_add <- data.frame(matrix(NA, empty_bar * nlevels(lolli_data1$diff_bin), ncol(lolli_data1)))
colnames(to_add) <- colnames(lolli_data1)
to_add$diff_bin <- rep(levels(lolli_data1$diff_bin), each = empty_bar)
lolli_data1 <- rbind(lolli_data1, to_add)
lolli_data1 <- lolli_data1 %>% arrange(diff_bin)
lolli_data1$id <- seq(1, nrow(lolli_data1))

# get the name and the y position of each label
label_data_m <- lolli_data1
number_of_bar <- nrow(label_data_m)
angle <- 90 - 360 * (label_data_m$id - 0.5) / number_of_bar
label_data_m$hjust <- ifelse(angle < -90, 1, 0)
label_data_m$angle <- ifelse(angle < -90, angle + 180, angle)

# Not sure how to use the code below
redlist_breaks <- lolli_data1 %>%
  group_by(diff_bin) %>%
  summarize(
    start = min(id),
    end = max(id) - 3
  ) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end))) %>%
  ungroup() %>%
  mutate(
    end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1),
    start = start - 1
  )

max_value <- max(lolli_data1$abs_diff, na.rm = T)

y_max <- 20 * ceiling(max_value / 20)

v <- c(5, 10, 15, 20, 25)

redlist_breaks <- redlist_breaks %>%
  mutate(v = list(v)) %>%
  unnest(cols = c(v))

prop2 <-
  ggplot(data = lolli_data1 %>%
    group_by(diff_bin) %>%
    mutate(id = fct_reorder(as.factor(id), abs_diff))) +
  geom_segment(
    data = redlist_breaks %>%
      filter(v != 100),
    aes(x = end, y = v, xend = start, yend = v),
    color = "grey", linewidth = 0.3, inherit.aes = FALSE
  ) +
  annotate("text",
    x = rep(max(lolli_data1$id, length(v))),
    y = v, label = paste0(head(v), "%"), color = "grey", size = 5, angle = 0, fontface = "bold", hjust = 1
  ) +
  geom_col(
    aes(
      x = id,
      y = abs_diff * 100,
      fill = percent_diff
    ),
    # show.legend = F,
    width = 0.8
  ) +
  ylim(-40, NA) +
  scale_fill_viridis_c() + # manual(values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  scale_x_discrete() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(
    data = label_data_m,
    aes(x = id, y = 25, label = common_name, angle = angle),
    nudge_x = -0.25, nudge_y = 0.25,
    color = "black", size = 4, inherit.aes = FALSE
  ) +
  geom_segment(
    data = redlist_breaks,
    aes(x = start, y = -5, xend = end, yend = -5),
    colour = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  geom_text(
    data = redlist_breaks,
    aes(x = title, y = -18, label = diff_bin, color = diff_bin),
    color = "black",
    show.legend = F,
    hjust = 0.5, alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE
  ) +
  # scale_color_viridis_d() + # (values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  # geom_text(data = redlist_breaks,
  #             aes(x = title, y = 48, label = redlist_category),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE) +
  labs(fill = "Percent difference\n in mortality") + # y = 80
  theme_void(base_size = 16) +
  labs(
    x = "",
    y = ""
  ) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black"))

ggsave(prop2, file = paste0("fig4.pdf"), path = here::here("figs"), height = 20, width = 40) + plot_annotation(tag_levels = "A")

# Set a number of empty bars to add at the end of each group
lolli_data2 <- non_threat_mean %>%
  select(f, f_mort, avm_prm, abs_diff, diff_bin, common_name, percent_diff) %>%
  mutate(id = fct_reorder(as.factor(common_name), abs_diff))

empty_bar <- 7
to_add <- data.frame(matrix(NA, empty_bar * nlevels(lolli_data2$diff_bin), ncol(lolli_data2)))
colnames(to_add) <- colnames(lolli_data2)
to_add$diff_bin <- rep(levels(lolli_data2$diff_bin), each = empty_bar)
lolli_data2 <- rbind(lolli_data2, to_add)
lolli_data2 <- lolli_data2 %>% arrange(diff_bin)
lolli_data2$id <- seq(1, nrow(lolli_data2))


# get the name and the y position of each label
label_data_m <- lolli_data2
number_of_bar <- nrow(label_data_m)
angle <- 90 - 360 * (label_data_m$id - 0.5) / number_of_bar
label_data_m$hjust <- ifelse(angle < -90, 1, 0)
label_data_m$angle <- ifelse(angle < -90, angle + 180, angle)

# Not sure how to use the code below
redlist_breaks <- lolli_data2 %>%
  group_by(diff_bin) %>%
  summarize(
    start = min(id),
    end = max(id) - 3
  ) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end))) %>%
  ungroup() %>%
  mutate(
    end = data.table::shift(end + 1, n = 1, type = "shift", fill = max(end) + 1),
    start = start - 1
  )

max_value <- max(lolli_data2$abs_diff, na.rm = T)

y_max <- 20 * ceiling(max_value / 20)

v <- c(5, 10, 15, 20, 25)

redlist_breaks <- redlist_breaks %>%
  mutate(v = list(v)) %>%
  unnest(cols = c(v))

prop3 <-
  ggplot(data = lolli_data2 %>%
    group_by(diff_bin) %>%
    mutate(id = fct_reorder(as.factor(id), abs_diff))) +
  geom_segment(
    data = redlist_breaks %>%
      filter(v != 100),
    aes(x = end, y = v, xend = start, yend = v),
    color = "grey", linewidth = 0.3, inherit.aes = FALSE
  ) +
  annotate("text",
    x = rep(max(lolli_data2$id, length(v))),
    y = v - 5, label = paste0(head(v), "%"), color = "grey", size = 5, angle = 0, fontface = "bold", hjust = 0.7
  ) +
  geom_col(
    aes(
      x = id,
      y = abs_diff * 100,
      fill = percent_diff,
    ),
    # show.legend = F,
    width = 0.8
  ) +
  ylim(-40, NA) +
  scale_fill_viridis_c() + # manual(values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  scale_x_discrete() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(
    data = label_data_m,
    aes(x = id, y = 30, label = common_name, angle = angle),
    nudge_x = -0.25, nudge_y = 0.25,
    color = "black", size = 4, inherit.aes = FALSE
  ) +
  geom_segment(
    data = redlist_breaks,
    aes(x = start, y = -5, xend = end, yend = -5),
    colour = "black", alpha = 0.8, inherit.aes = FALSE
  ) +
  geom_text(
    data = redlist_breaks,
    aes(x = title, y = -18, label = diff_bin, color = diff_bin),
    color = "black",
    show.legend = F,
    hjust = 0.5, alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE
  ) +
  # scale_color_viridis_d() + # (values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  # geom_text(data = redlist_breaks,
  #             aes(x = title, y = 48, label = redlist_category),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE) +
  labs(fill = "Percent difference\nin mortality") + # y = 80
  theme_void(base_size = 16) +
  labs(
    x = "",
    y = ""
  ) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black"))

ggsave(prop3, file = paste0("figS4.pdf"), path = here::here("figs", "supp"), height = 20, width = 40)

# Figure 5 and S5-10 ----------------------------------------------------------------

### ADDED THIS CODE TO MAKE FIGURES 5 AND SX DISPLAY COMMON NAMES ON THE FACET STRIPS INSTEAD OF THE SCIENTIFIC NAMES
sim_results_new <- sim_results_2 %>%
  left_join(sim_results_common) %>%
  mutate(common_name = case_when(
    scientific_name == "Alopias vulpinus" ~ "Common thresher",
    scientific_name == "Squalus acanthias" ~ "Spiny dogfish",
    TRUE ~ common_name
  ))

no_cq <- sim_results_new %>%
  filter(scientific_name %in% iucn_data$scientific_name) %>%
  mutate(mort_scenario = fct_relevel(as.factor(mort_scenario), "Low Mortality", after = Inf)) %>%
  distinct() %>%
  left_join(iucn_data) %>%
  mutate(redlist_category = fct_relevel(as.factor(redlist_category), c("CR", "EN", "VU", "NT", "LC", "DD"))) %>%
  rename(
    sci = scientific_name,
    scientific_name = common_name
  ) %>%
  mutate(mort_scenario = case_when(
    mort_scenario == "BAU" ~ "Full retention",
    TRUE ~ mort_scenario
  )) %>%
  select(-avm, -prm, -prm_75, -prm_25, -avm_25, -avm_25)

no_cq <- no_cq[!duplicated(no_cq %>% select(-n_div_k, -pop.array)), ]

no_cq_sci <- no_cq %>%
  distinct(scientific_name) %>%
  pull()

no_cq_cr <- no_cq %>%
  filter(redlist_category == "CR")
no_cq_en <- no_cq %>%
  filter(redlist_category == "EN")
no_cq_vu <- no_cq %>%
  filter(redlist_category == "VU")
no_cq_nt <- no_cq %>%
  filter(redlist_category == "NT")
no_cq_lc <- no_cq %>%
  filter(redlist_category == "LC")
no_cq_dd <- no_cq %>%
  filter(redlist_category == "DD")

p1 <- ggplot() +
  geom_rect(data = no_cq_cr, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.6, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_cr, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976", "#91CF60", "#1A9850", "grey", "white")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p1, file = paste0("figS5.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p2 <- ggplot() +
  geom_rect(data = no_cq_en, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 2.1, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_en, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#FD8D3C", "#FED976", "#91CF60", "#1A9850", "grey", "white", "#E31A1C")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p2, file = paste0("figS6.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p3 <- ggplot() +
  geom_rect(data = no_cq_vu, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 2.1, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_vu, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#FED976", "#91CF60", "#1A9850", "grey", "white", "#E31A1C", "#FD8D3C")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p3, file = paste0("figS7.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p4 <- ggplot() +
  geom_rect(data = no_cq_nt, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.80, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_nt, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#91CF60", "#1A9850", "grey", "white", "#E31A1C", "#FD8D3C", "#FED976")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p4, file = paste0("figS8.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p5 <- ggplot() +
  geom_rect(data = no_cq_lc, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.95, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_lc, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#1A9850", "grey", "white", "#E31A1C", "#FD8D3C", "#FED976")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p5, file = paste0("figS9.pdf"), path = here::here("figs", "supp"), height = 18, width = 18)

p6 <- ggplot() +
  geom_rect(data = no_cq_dd, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.46, fill = as.factor(redlist_category))) +
  geom_line(data = no_cq_dd, aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(
    ~ factor(scientific_name,
      levels = no_cq_sci
    ),
    labeller = label_wrap_gen(15)
  ) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("grey", "white", "#E31A1C", "#FD8D3C", "#FED976")) +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario",
    fill = "IUCN Category"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p6, file = paste0("figS10.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

f_val_sim <- read_csv(here::here("data", "f_reduce_f_msy_compare.csv"))

species_sub <- c(
  "Prionace glauca", "Carcharhinus limbatus", "Isurus oxyrinchus", "Squalus acanthias", "Alopias vulpinus",
  "Pseudocarcharias kamoharai", "Carcharhinus falciformis", "Sphyrna mokarran",
  "Carcharhinus hemiodon", "Squatina squatina", "Sphyrna corona", "Galeorhinus galeus"
)

no_cq_sub <- no_cq %>%
  filter(sci %in% f_val_sim$scientific_name) %>%
  mutate(scientific_name = paste0(scientific_name, " (", redlist_category, ")"))

f_val_sim <- f_val_sim %>%
  mutate(name = paste0(common_name, " (", redlist_category, ")")) %>%
  mutate(name = ifelse(name == "Thresher (VU)", "Common thresher (VU)", name))

p1 <- ggplot(data = f_val_sim, aes(f / 1.5, f_reduce_5)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(color = percent_diff, shape = f_ratio_5), size = 4) +
  theme_bw(base_size = 16) +
  scale_color_viridis_c() +
  labs(
    x = expression(F[MSY]), y = "Expected F with Retention Prohibition",
    color = "Percent Mortality \n Reduction from \n Retention Prohibition",
    shape = "",
    title = "Last 5 years of F"
  ) +
  annotate(geom = "text", label = "F[Retention] ==~ F[MSY]", x = 0.15, y = 0.2, parse = TRUE, size = 8) +
  geom_curve(aes(x = 0.15, y = 0.195, xend = 0.15, yend = 0.155), arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label_repel(aes(label = name), size = 5, vjust = "outward", hjust = "outward", alpha = 0.75) +
  theme(
    legend.key.size = unit(8, "mm"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black"))

p2 <- ggplot(data = f_val_sim, aes(f / 1.5, f_reduce_10)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(color = percent_diff, shape = f_ratio_10), size = 4) +
  theme_bw(base_size = 16) +
  scale_color_viridis_c() +
  labs(
    x = expression(F[MSY]), y = "Expected F with Retention Prohibition",
    color = "Percent Mortality \n Reduction from \n Retention Prohibition",
    shape = "",
    title = "Last 10 years of F"
  ) +
  annotate(geom = "text", label = "F[Retention] ==~ F[MSY]", x = 0.15, y = 0.2, parse = TRUE, size = 8) +
  geom_curve(aes(x = 0.15, y = 0.195, xend = 0.15, yend = 0.155), arrow = arrow(length = unit(0.1, "inches"))) +
  geom_label_repel(aes(label = name), size = 5, vjust = "outward", hjust = "outward", alpha = 0.75) +
  theme(
    legend.key.size = unit(8, "mm"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black"))

p1 / p2 + plot_layout(guides = "collect")

p <- ggplot() +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    linetype = "dashed"
  ) +
  geom_rect(
    data = no_cq_sub,
    aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.19, fill = as.factor(redlist_category))
  ) +
  geom_line(data = no_cq_sub %>% filter(!is.na(scenario)), aes(t, n_div_k, color = mort_scenario, group = total_mort), linewidth = 2) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~ factor(scientific_name,
    levels = unique(no_cq_sub$scientific_name)
  )) +
  theme_bw(base_size = 16) +
  scale_color_viridis_d() +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976", "#91CF60", "#1A9850")) +
  labs(
    x = "Time",
    y = "N/K",
    fill = "IUCN Category",
    color = "Scenario"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

p / p1

ggsave(p / p1 + plot_annotation(tag_levels = "A"), file = paste0("fig5.pdf"), path = here::here("figs"), height = 20, width = 20)

# Figure S11-----------------------------------------------------------------------------

pct_mort_reduction <- read_csv(here::here("data", "pct_mort_reduction_sim.csv"))

pct_mort_reduction_uncorrected <- read_csv(here::here("data", "pct_mort_reduction_sim_uncorrected.csv"))

mean_mort_reduction_fam <- pct_mort_reduction %>%
  mutate(f_mort = (100 - ((100 * (1 - f)) + (100 * f * (1 - mid_avm) * (1 - mid_prm)))) / 100) %>%
  select(scientific_name, f, f_mort) %>%
  distinct() %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  mutate(abs_diff = f - f_mort) %>%
  left_join(iucn_taxonomy, by = "scientific_name") %>%
  left_join(iucn_join, by = "family") %>%
  # mutate(threat = ifelse(redlist_category %in% c("CR", "VU", "EN"), "threatened", "not threatened")) %>%
  group_by(family) %>%
  mutate(
    mean_mort_reduction = mean(percent_diff),
    sd_mort_reduction = sd(percent_diff),
    se_mort_reduction = sd(percent_diff) / sqrt(n())
  ) %>%
  mutate(
    mean_abs_mort_reduction = mean(abs_diff),
    sd_abs_mort_reduction = sd(abs_diff),
    se_abs_mort_reduction = sd(abs_diff) / sqrt(n())
  ) %>%
  ungroup() %>%
  filter(threat == "threatened") %>%
  mutate(sp_threat = case_when(
    sp_threat_pct < 0.25 ~ "<0.25",
    sp_threat_pct >= 0.25 & sp_threat_pct < 0.5 ~ "<0.50",
    sp_threat_pct >= 0.5 & sp_threat_pct < 0.75 ~ "<0.75",
    sp_threat_pct >= 0.75 ~ ">0.75"
  )) %>%
  mutate(status = "Corrected")

mean_mort_reduction_fam_unc <- pct_mort_reduction_uncorrected %>%
  mutate(f_mort = (100 - ((100 * (1 - f)) + (100 * f * (1 - mid_avm) * (1 - mid_prm)))) / 100) %>%
  select(scientific_name, f, f_mort) %>%
  distinct() %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  mutate(abs_diff = f - f_mort) %>%
  left_join(iucn_taxonomy, by = "scientific_name") %>%
  left_join(iucn_join, by = "family") %>%
  # mutate(threat = ifelse(redlist_category %in% c("CR", "VU", "EN"), "threatened", "not threatened")) %>%
  group_by(family) %>%
  mutate(
    mean_mort_reduction = mean(percent_diff),
    sd_mort_reduction = sd(percent_diff),
    se_mort_reduction = sd(percent_diff) / sqrt(n())
  ) %>%
  mutate(
    mean_abs_mort_reduction = mean(abs_diff),
    sd_abs_mort_reduction = sd(abs_diff),
    se_abs_mort_reduction = sd(abs_diff) / sqrt(n())
  ) %>%
  ungroup() %>%
  group_by(family) %>%
  mutate(sp_count = n()) %>%
  ungroup() %>%
  filter(threat == "threatened") %>%
  mutate(sp_threat = case_when(
    sp_threat_pct < 0.25 ~ "<0.25",
    sp_threat_pct >= 0.25 & sp_threat_pct < 0.5 ~ "<0.50",
    sp_threat_pct >= 0.5 & sp_threat_pct < 0.75 ~ "<0.75",
    sp_threat_pct >= 0.75 ~ ">0.75"
  )) %>%
  mutate(status = "Uncorrected")

sim_join <- full_join(mean_mort_reduction_fam, mean_mort_reduction_fam_unc)

write_csv(sim_join, file = here::here("data", "sim_pct_correct_uncorrect.csv"))

mort_red_corrected <-
  ggplot(data = sim_join) +
  geom_point(aes(x = fct_reorder(family, mean_abs_mort_reduction), y = mean_abs_mort_reduction, size = sp_threat)) +
  geom_linerange(aes(
    x = family, ymax = mean_abs_mort_reduction + se_abs_mort_reduction,
    ymin = mean_abs_mort_reduction - se_abs_mort_reduction
  )) +
  # geom_hline(
  #   yintercept = 50,
  #   linetype = "dashed"
  # ) +
  facet_wrap(~status) +
  coord_flip() +
  labs(
    y = "Mean absolute mortality reduction from retention prohibition",
    x = "",
    size = "Proportion of\nthreatened species\nper family"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_manual(values = c("<0.25" = 1, "<0.50" = 3, "<0.75" = 5, ">0.75" = 7)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(color = "black", size = 11)
  )

ggsave(mort_red_corrected, file = paste0("figS11.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)

# Figure S12 and Dryad data--------------------------------------------------------------

sim_results <- list(sim_results1_5, sim_results1, sim_results2, sim_results3, uncorrected_results) %>%
  reduce(full_join) %>%
  filter(scientific_name %in% iucn_data$scientific_name)

sim_results <- sim_results[!duplicated(sim_results %>% select(scientific_name, t, mort_scenario, fp, corrected)), ]

rm(sim_results1_5, sim_results1, sim_results2, sim_results3, uncorrected_results)
gc()

eq <- sim_results %>%
  filter(t == 200) %>%
  filter(!is.na(mort_scenario)) %>%
  mutate(mort_scenario = fct_relevel(mort_scenario, "Low Mortality", after = Inf)) %>%
  mutate(scientific_name = fct_reorder(scientific_name, n_div_k))

p <- ggplot(eq %>% filter(corrected == "yes")) +
  geom_line(aes(fp, n_div_k, group = scientific_name), color = "grey") +
  geom_hline(
    yintercept = 0.5,
    color = "red",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  facet_wrap(~mort_scenario, nrow = 4, scales = "free_y") +
  theme_bw(base_size = 16) +
  labs(
    x = "Fishing Pressure (Fmsy Multiplier)",
    y = "N/K"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )
p

ggsave(p, file = paste0("figS12.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)

# Summarize sensitivity stats within paper

percent_calc <- sim_results %>%
  mutate(f_mort = (100 - ((100 * (1 - f)) + (100 * f * (1 - mid_avm) * (1 - mid_prm)))) / 100) %>%
  select(scientific_name, f, f_mort, fp, corrected) %>%
  distinct() %>%
  mutate(
    percent_diff = (f - f_mort) / f * 100,
    abs_diff = f - f_mort
  ) %>%
  mutate(f_max = (f) / (1 - (percent_diff / 100))) %>%
  mutate(f_fmsy = case_when(
    fp == 1 ~ (f_max / f),
    TRUE ~ NA
  )) %>%
  group_by(fp, corrected) %>%
  mutate(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE)
  ) %>%
  ungroup()

sim_200 <- sim_results %>%
  filter(t == 200) %>%
  filter(corrected == "yes") %>%
  filter(mort_scenario == "Median Mortality") %>%
  select(n_div_k, scientific_name, fp) %>%
  filter(n_div_k >= 0.5) %>%
  select(scientific_name, fp) %>%
  distinct() %>%
  group_by(fp) %>%
  summarize(per = n() / 265 * 100)

mort_50 <- percent_calc %>%
  filter(corrected == "yes") %>%
  filter(percent_diff > 75) %>%
  filter(fp == 1.5) %>%
  select(scientific_name) %>%
  distinct()

nrow(mort_50) / 265 * 100

output <- eq %>%
  left_join(percent_calc) %>%
  select(-pop.array, -t, -mean_percent_diff, -mean_abs_diff, -total_mort, -scenario, -quota) %>%
  rename(
    simulation_avm = avm,
    simulation_prm = prm,
    pred_avm_75 = avm_75,
    pred_prm_75 = prm_75,
    pred_avm_25 = avm_25,
    pred_prm_25 = prm_25,
    pred_avm_mean = mid_avm,
    pred_prm_mean = mid_prm,
    percent_mort_diff = percent_diff,
    absolute_mort_diff = abs_diff,
    fishing_mort_bau = f,
    fishing_mort_rb = f_mort,
    msy_multiple_fishing = fp,
    avm_correction = corrected
  ) %>%
  select(
    scientific_name, msy_multiple_fishing, fishing_mort_bau, fishing_mort_rb, percent_mort_diff, absolute_mort_diff, f_fmsy,
    mort_scenario, simulation_avm, simulation_prm, n_div_k,
    pred_avm_25, pred_avm_mean, pred_avm_75, pred_prm_25, pred_prm_mean, pred_prm_75, avm_correction
  )

# create dryad data

write_csv(output, here::here("data", "table_s6.csv"))
