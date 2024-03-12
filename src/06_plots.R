library(tidyverse)
library(ggridges)
library(patchwork)
library(janitor)
library(ggh4x)
library(here)

set.seed(42)

# Figures 1 and S1 and S12 --------------------------------------------------------

# read in the data
prm_elasmo <- read_csv(here("data", "prm_dataset_updated.csv"))

# calculating mean values for those taxa at higher levels than species
## Dasyatis sp
dasyatis_sp <- prm_elasmo %>%
  filter(str_detect(family, "Dasyatidae")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

## Squalus sp
squalus_sp <- prm_elasmo %>%
  filter(str_detect(scientific_name, "Squalus")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

## Mustelus sp
mustelus_sp <- prm_elasmo %>%
  filter(str_detect(scientific_name, "Mustelus")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

## Centrophorus sp
centrophorus_sp <- prm_elasmo %>%
  filter(str_detect(scientific_name, "Centrophorus")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

## sharks
sharks <- prm_elasmo %>%
  filter(str_detect(common_name, "shark")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

## rays
rays <- prm_elasmo %>%
  filter(str_detect(measure, "disk_width")) %>%
  mutate(
    mean_size = mean(max_size_cm, na.rm = TRUE),
    mean_depth = mean(median_depth, na.rm = TRUE)
  )

prm_elasmo_count <- prm_elasmo %>%
  select(estimate_type, reference) %>%
  group_by(estimate_type) %>%
  count()

# data wrangling
prm_elasmo_subset <- prm_elasmo %>%
  select(
    scientific_name, gear_class, habitat_associated, estimate_type, estimate, sample_size,
    method, ventilation_method, max_size_cm, measure, median_depth, reproductive_mode, family
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
  mutate(measure = case_when(
    scientific_name == "sharks" ~ "total_length",
    scientific_name %in% c("rays", "Dasyatis sp") ~ "disk_width",
    TRUE ~ measure
  )) %>%
  mutate(reproductive_mode = case_when(
    scientific_name == "sharks" ~ "yolk-sac viviparity",
    scientific_name == "rays" ~ "yolk-sac viviparity",
    TRUE ~ reproductive_mode
  )) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(group = case_when(
    str_detect(family, "Mobulidae|Dasyatidae|Gymnyridae|Myliobatidae|Torpedinidae|Rhinobatidae|Rhinidae|Aetobatidae|Rajidae|Pristidae") ~ "Batoids",
    str_detect(scientific_name, "Himantura|Dasyatis|Gymnura|Neotrygon|Bathytoshia|rays|Rhinoptera|Rhynchobatus|Aptychotrema|Trygon") ~ "Batoids",
    TRUE ~ "Sharks"
  )) %>%
  mutate(estimate = case_when(
    estimate_type == "post-release survival" ~ 1 - estimate, # Converts post-release survival into PRM
    TRUE ~ estimate
  )) %>%
  mutate(estimate_type = case_when(
    estimate_type == "post-release survival" ~ "post-release mortality",
    TRUE ~ estimate_type
  ))

# write_csv(prm_elasmo_subset, file = here("data", "prm_elasmo_subset.csv"), col_names = TRUE, na = "")

prm_sharks <- prm_elasmo_subset %>%
  filter(group == "Sharks") %>%
  distinct(scientific_name)

prm_batoids <- prm_elasmo_subset %>%
  filter(group == "Batoids") %>%
  distinct(scientific_name)

# Create subset of the whole dataset with at-vessel mortality and calculate the weighted averages per species
mort_summary <- prm_elasmo_subset %>%
  filter(estimate > 0) %>%
  mutate(est_count = sample_size * estimate) %>%
  group_by(scientific_name, estimate_type) %>%
  mutate(mortality_prop = sum(est_count) / sum(sample_size)) %>%
  ungroup() %>%
  mutate(
    prm = ifelse(estimate_type == "post-release mortality", mortality_prop, NA),
    avm = ifelse(estimate_type == "at-vessel mortality", mortality_prop, NA)
  )

# create mortality subset
mort_subset <- mort_summary %>%
  arrange(avm) %>%
  select(scientific_name, avm) %>%
  distinct(scientific_name) %>%
  pull(scientific_name)

elasmo_avm <- prm_elasmo_subset %>%
  filter(estimate_type == "at-vessel mortality" & estimate > 0) %>%
  mutate(est_count = sample_size * estimate) %>%
  group_by(scientific_name) %>%
  mutate(mortality_prop = sum(est_count) / sum(sample_size)) %>%
  drop_na() %>%
  filter(!gear_class %in% c("handline", "purse seine"))

# Create subset of the whole dataset with post-release mortality and calculate the weighted averages per species
elasmo_prm <- prm_elasmo_subset %>%
  filter(estimate_type == "post-release mortality" & estimate > 0) %>%
  mutate(est_count = sample_size * estimate) %>%
  group_by(scientific_name) %>%
  mutate(mortality_prop = sum(est_count) / sum(sample_size)) %>%
  drop_na() %>%
  filter(!gear_class %in% c("handline", "purse seine", "pole and line"))

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

# get summary stats for AVM per family
mort_summary_fam <- mort_summary %>%
  filter(estimate_type == "at-vessel mortality" & gear_class %in% c("longline", "gillnet", "trawl")) %>%
  group_by(family, gear_class) %>%
  summarise(mean_avm = mean(mortality_prop))

# get summary stats for which locations studies were conducted
elasmo_prm_location <- prm_elasmo %>%
  mutate(location_clean = case_when(
    str_detect(location, "Texas") ~ "USA",
    str_detect(location, "California") ~ "USA",
    str_detect(location, "Alabama") ~ "USA",
    str_detect(location, "Rhode Island") ~ "USA",
    str_detect(location, "Massachussets") ~ "USA",
    str_detect(location, "New York") ~ "USA",
    str_detect(location, "Carolina") ~ "USA",
    str_detect(location, "Virginia") ~ "USA",
    str_detect(location, "Hawaii") ~ "USA",
    str_detect(location, "Maine") ~ "USA",
    str_detect(location, "Biscay") ~ "France",
    str_detect(location, "Galicia") ~ "Spain",
    str_detect(location, "Grand banks") ~ "Canada",
    str_detect(location, "Australia") ~ "Australia",
    str_detect(location, "Mexico") ~ "Mexico",
    str_detect(location, "Leone") ~ "Sierra Leone",
    str_detect(location, "South Wales") ~ "Australia",
    str_detect(location, "Tasmania") ~ "Australia",
    str_detect(location, "Antalya") ~ "Turkey",
    str_detect(location, "English") ~ "England",
    str_detect(location, "USA") ~ "USA",
    TRUE ~ location
  )) %>%
  select(reference, location_clean, estimate_type) %>%
  distinct() %>%
  group_by(estimate_type, location_clean) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  ungroup() %>%
  mutate(sum = sum(count))


elasmo_prm_location_subset <- elasmo_prm_location %>%
  filter(location_clean %in% c("USA", "Florida", "Australia", "Spain", "France", "Canada", "England", "New Zealand")) %>%
  mutate(location_clean = case_when(
    location_clean == "Florida" ~ "USA",
    TRUE ~ location_clean
  )) %>%
  select(-estimate_type) %>%
  group_by(location_clean) %>%
  mutate(sum = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(count)) %>%
  mutate(percentages = 100 * sum / total) %>%
  distinct() %>%
  arrange(desc(percentages))

# get summary stats
## number of studies per major gear
prm_elasmo_subset_gear <- prm_elasmo %>%
  filter(gear_class %in% c("longline", "gillnet", "trawl")) %>%
  select(gear_class, reference) %>%
  distinct() %>%
  group_by(gear_class) %>%
  mutate(count = n()) %>%
  select(gear_class, count) %>%
  distinct()

# number of species per type of mortality
prm_elasmo_subset_sp <- prm_elasmo_subset %>%
  distinct(group, estimate_type, scientific_name) %>%
  group_by(group, estimate_type) %>%
  summarise(count = n())

mort_proportions_plot_sp <-
  ggplot(data = mort_summary %>%
           filter(!gear_class %in% c("handline", "purse seine", "pole and line")) %>%
           mutate(
             estimate_type = str_to_sentence(estimate_type),
             gear_class = str_to_title(gear_class)
           )) +
  geom_line(
    aes(
      x = fct_rev(factor(scientific_name, levels = mort_subset)),
      y = mortality_prop,
      group = scientific_name
    ),
    show.legend = F,
    color = "gray30",
    linewidth = 0.6,
    alpha = 0.5
  ) +
  geom_point(
    aes(
      x = fct_rev(factor(scientific_name, levels = mort_subset)),
      y = mortality_prop,
      color = estimate_type
    ),
    size = 1.5,
    alpha = 0.9,
    show.legend = F
  ) +
  facet_grid(
    factor(group, levels = c("Sharks", "Batoids")) ~ factor(gear_class,
                                                            levels = c("Longline", "Gillnet", "Trawl")
    ),
    scales = "free_y",
    space = "free_y"
  ) +
  labs(
    x = "",
    y = "Fishing mortality",
    color = "Estimate type"
  ) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_shape(guide = "none") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(face = "italic"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

# ggsave(mort_proportions_plot_sp, file = paste0("mort_proportions_plot_sp.png"), path = here::here("figs"), height = 12, width = 8)
# Create subset of the whole dataset with at-vessel mortality and calculate the weighted averages per family
mort_summary_fam <- prm_elasmo_subset %>%
  filter(estimate > 0) %>%
  mutate(est_count = sample_size * estimate) %>%
  group_by(family, estimate_type) %>%
  mutate(mortality_prop = sum(est_count) / sum(sample_size)) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(
    prm = ifelse(estimate_type == "post-release mortality", mortality_prop, NA),
    avm = ifelse(estimate_type == "at-vessel mortality", mortality_prop, NA)
  )

mort_subset_fam <- mort_summary_fam %>%
  arrange(avm) %>%
  select(family, avm) %>%
  distinct(family) %>%
  pull(family)

mort_summary_fam_seg <- mort_summary_fam %>%
  pivot_wider(
    names_from = estimate_type,
    values_from = mortality_prop
  )

# Create subset of the whole dataset with at-vessel mortality and calculate the weighted averages per species
elasmo_avm_fam <- elasmo_avm %>%
  group_by(family) %>%
  summarise(mean_prop = mean(mortality_prop))

# Create subset of the whole dataset with post-release mortality and calculate the weighted averages per species
elasmo_prm_fam <- elasmo_prm %>%
  group_by(family) %>%
  summarise(mean_prop = mean(mortality_prop))

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
  group_by(habitat_associated, estimate_type, gear_class) %>%
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
    y = "Fishing mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

# ggsave(mort_proportions_plot_fam, file = paste0("mort_proportions_plot_fam.png"), path = here::here("figs"), height = 12, width = 8)
habitat_plot <-
  ggplot() +
  geom_point(
    data = mort_summary %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
               !is.na(habitat_associated) &
               group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat_associated = str_to_title(habitat_associated),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat_associated = as.factor(habitat_associated)) %>%
      arrange(desc(habitat_associated)),
    aes(
      x = fct_inorder(habitat_associated),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1)
  ) +
  geom_boxplot(
    data = habitat_subset %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
               !is.na(habitat_associated) &
               group == "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat_associated = str_to_title(habitat_associated),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat_associated = as.factor(habitat_associated)) %>%
      arrange(desc(habitat_associated)),
    aes(
      x = fct_inorder(habitat_associated),
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
    y = "Fishing mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 42) +
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

ggsave(mort_proportions_plot, file = paste0("fig1.pdf"), path = here::here("figs"), height = 10, width = 12)

## Batoid Plot

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
    y = "Fishing mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "transparent"),
    panel.spacing.x = unit(5, "mm")
  )

# ggsave(mort_proportions_plot_fam, file = paste0("mort_proportions_plot_fam.png"), path = here::here("figs"), height = 12, width = 8)

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
        habitat_associated = str_to_title(habitat_associated),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat_associated = as.factor(habitat_associated)) %>%
      arrange(desc(habitat_associated)),
    aes(
      x = fct_inorder(habitat_associated),
      y = mortality_prop,
      color = estimate_type, group = estimate_type
    ),
    position = position_jitterdodge(jitter.width = .1)
  ) +
  geom_boxplot(
    data = habitat_subset %>%
      filter(!gear_class %in% c("handline", "purse seine", "pole and line") &
               !is.na(habitat_associated) &
               group != "Sharks") %>%
      mutate(
        estimate_type = str_to_sentence(estimate_type),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(
        habitat_associated = str_to_title(habitat_associated),
        gear_class = str_to_title(gear_class)
      ) %>%
      mutate(habitat_associated = as.factor(habitat_associated)) %>%
      arrange(desc(habitat_associated)),
    aes(
      x = fct_inorder(habitat_associated),
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
    y = "Fishing mortality",
    color = "Estimate type",
    fill = "Estimate type"
  ) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  scale_fill_manual(values = c("#414487FF", "#22A884FF")) +
  coord_flip() +
  theme_bw(base_size = 14) +
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

ggsave(mort_proportions_plot_bat, file = paste0("mort_proportions_plot_bat.pdf"), path = here::here("figs", "supp"), height = 12, width = 15)

# create species count plot
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
    limits = c(0, 25)
  ) +
  labs(
    x = "",
    y = "Number of observations per species"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(color = "black", size = 10, face = "italic"),
    axis.text.x = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(obs_count_plot, file = paste0("obs_count_plot.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)


# Figures 3 and S3 --------------------------------------------------------

# read in the  data
iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>%
  janitor::clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") |
    str_detect(scientific_name, "Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus")) %>% # list of genera to keep in the filtering
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

full_predictions <- read_csv(here::here("data", "full_model_predictions.csv")) %>%
  filter(scientific_name %in% iucn_data$scientific_name)

full_predictions_iucn <- full_predictions %>%
  left_join(iucn_data, by = "scientific_name")

mean_predictions <- full_predictions %>%
  group_by(scientific_name) %>%
  mutate(
    avm_pred = mean(avm_pred),
    prm_pred = mean(prm_pred)
  )

# write_csv(mean_predictions, here::here("data", "full_pred_table.csv"))

predictions <- mean_predictions %>%
  pivot_longer(cols = c("avm_pred", "prm_pred"), names_to = "estimate_type", values_to = "mortality_prop") %>%
  mutate(estimate_type = case_when(
    estimate_type == "avm_pred" ~ "AVM",
    estimate_type == "prm_pred" ~ "PRM"
  ))

mort_subset_avm <- mean_predictions %>%
  group_by(family) %>%
  summarize(fam_mean = mean(avm_pred)) %>%
  arrange(fam_mean) %>%
  distinct(family) %>%
  pull(family)

mort_subset_prm <- mean_predictions %>%
  group_by(family) %>%
  summarize(fam_mean = mean(prm_pred)) %>%
  arrange(desc(fam_mean)) %>%
  distinct(family) %>%
  pull(family)

prm_elasmo <- read_csv(here::here("data", "prm_elasmo_subset.csv")) %>%
  filter(gear_class == "longline") %>%
  filter(!grepl("Rajidae", family)) %>%
  mutate(estimate_type = case_when(
    estimate_type == "at-vessel mortality" ~ "AVM",
    estimate_type == "post-release mortality" ~ "PRM"
  ))

p6 <- ggplot() +
  # geom_density_ridges(data = prm_elasmo, aes(x = estimate, y = fct_rev(factor(family, levels = mort_subset))), alpha = 0.5)+
  # geom_point(data = prm_elasmo, aes(x = estimate, y = fct_rev(factor(family, levels = mort_subset))), alpha = 0.3)+
  geom_density_ridges(data = predictions, aes(x = mortality_prop, y = fct_rev(factor(family, levels = mort_subset_avm)), fill = fct_rev(factor(family, levels = mort_subset_avm))), alpha = 0.5) +
  geom_point(data = predictions, aes(x = mortality_prop, y = fct_rev(factor(family, levels = mort_subset_avm)), color = fct_rev(factor(family, levels = mort_subset_avm))), alpha = 0.5, size = 6) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  facet_grid(
    cols = vars(estimate_type),
    scales = "free_x",
    space = "free_x"
  ) +
  labs(
    y = "Family",
    x = "Estimated Mortality"
  ) +
  scale_shape(guide = "none") +
  theme_bw(base_size = 14) +
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
p6

ggsave(p6, file = paste0("fig3.pdf"), path = here::here("figs"), height = 22, width = 20)

p1 <- ggplot(predictions) +
  geom_point(aes(max_size_cm, mortality_prop),
    alpha = 0.5, size = 4
  ) +
  # geom_smooth(method = "loess", aes(max_size_cm, mortality_prop, color = reproductive_mode), se = FALSE) +
  theme_bw(base_size = 42) +
  labs(
    y = "Estimated mortality",
    x = "Size (cm)",
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
  scale_color_viridis_d() +
  theme(legend.position = "none")

p2 <- ggplot(predictions %>% filter(estimate_type == "AVM")) +
  geom_point(aes(median_depth, mortality_prop),
    alpha = 0.5, size = 4
  ) +
  # geom_smooth(method = "loess", aes(median_depth, mortality_prop, color = reproductive_mode), se = FALSE) +
  theme_bw(base_size = 42) +
  labs(
    y = "Estimated mortality",
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
  scale_color_viridis_d() +
  theme(legend.position = "none")


p3 <- ggplot(predictions %>% mutate(ventilation_method = str_to_sentence(ventilation_method))) +
  geom_point(aes(mortality_prop, ventilation_method),
    alpha = 0.5, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, ventilation_method),
    outlier.alpha = 0,
    alpha = 0.85
  ) +
  theme_bw(base_size = 42) +
  labs(
    y = "Ventilation Method",
    x = "Estimated mortality",
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
  scale_color_viridis_d() +
  theme(legend.position = "none")

p4 <- ggplot(predictions %>% mutate(habitat_associated = str_to_sentence(habitat_associated))) +
  geom_point(aes(mortality_prop, habitat_associated),
    alpha = 0.1,
    show.legend = F, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, habitat_associated),
    outlier.alpha = 0,
    alpha = 0.85,
    show.legend = F
  ) +
  theme_bw(base_size = 42) +
  labs(
    y = "Associated Habitat",
    x = "Estimated mortality",
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
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")

p5 <- ggplot(predictions %>% mutate(reproductive_mode = str_to_sentence(reproductive_mode))) +
  geom_point(aes(mortality_prop, reproductive_mode),
    alpha = 0.1,
    show.legend = F, size = 4
  ) +
  geom_boxplot(aes(mortality_prop, reproductive_mode),
    outlier.alpha = 0,
    alpha = 0.85,
    show.legend = F
  ) +
  theme_bw(base_size = 14) +
  labs(
    y = "Reproductive Mode",
    x = "Estimated mortality",
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
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")

plot <- p1 / p2 / (p4) / p5 / p3 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("preds_predictors.pdf"), path = here::here("figs"), height = 12, width = 15)


# Figures 4 and S4 --------------------------------------------------------


# read in the  data
iucn_data <- read_csv(here("data", "iucn_data", "assessments.csv")) %>%
  clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") | str_detect(scientific_name, "Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus")) %>% # list of genera to keep in the filtering
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

iucn_taxonomy <- read_csv(here("data", "iucn_data", "taxonomy.csv")) %>%
  clean_names() %>%
  mutate(family = str_to_sentence(family_name)) %>%
  select(family, genus_name, species_name) %>%
  unite(col = "scientific_name", c(genus_name, species_name), sep = " ")

iucn_data <- iucn_data %>%
  left_join(iucn_taxonomy, by = "scientific_name")

iucn_data_non_longline <- read_csv(here("data", "iucn_data", "assessments.csv")) %>%
  clean_names() %>%
  filter(str_detect(systems, "Marine") & !str_detect(threats, "longline"))

sim_results <- read_csv(here::here(basedir, "data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  filter(!is.na(mort_scenario)) %>%
  filter(mort_scenario == "BAU" | mort_scenario == "Median Mortality") %>%
  mutate(f_mort = ((100 * f) - (100 * f * (1 - mid_avm) * (1 - mid_prm))) / 100) %>%
  select(scientific_name, f, f_mort) %>%
  distinct()

sim_results <- read_csv(here::here("data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  filter(!is.na(mort_scenario)) %>%
  filter(mort_scenario == "BAU" | mort_scenario == "Median Mortality") %>%
  mutate(f_mort = ((100 * f) - (100 * f * (1 - mid_avm) * (1 - mid_prm))) / 100) %>%
  select(scientific_name, f, f_mort) %>%
  distinct()

top_shark_mort <- read_csv(here("data", "saup_eez_high_seas_shark_mortality.csv")) %>%
  group_by(scientific_name) %>%
  summarise(mort_sum = sum(mortality)) %>%
  arrange(desc(mort_sum))

new_dat <- read_csv(here::here("data", "iucn_fishbase_list.csv")) %>%
  select(scientific_name, common_name)

sim_results <- left_join(sim_results, new_dat)

# write_csv(sim_results, here::here("data", "f_sim_results.csv"))



sim_results <- read_csv(here::here("data", "f_sim_results.csv"))

# data wrangling for the plot
sim_results_iucn <- sim_results %>%
  filter(scientific_name %in% iucn_data$scientific_name) %>%
  left_join(iucn_data, by = "scientific_name")

sim_results_top <- left_join(top_shark_mort, sim_results_iucn) %>%
  filter(!is.na(f))

sim_results_iucn_pct <- sim_results_iucn %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  mutate(percent_diff = round(percent_diff, 4))

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
non_threat <- sim_results_iucn %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  mutate(percent_diff = round(percent_diff, 4)) %>%
  filter(redlist_category %in% c("DD", "NT", "LC")) %>%
  rowid_to_column("id") %>%
  mutate(id = fct_reorder(as.factor(id), percent_diff)) %>%
  mutate(redlist_category = as.factor(redlist_category)) %>%
  arrange(id) %>%
  select(-scientific_name) %>%
  rename(scientific_name = common_name)



# calculate average mortality reductions per threat status
threatended <- sim_results_iucn_pct %>%
  filter(redlist_category %in% c("VU", "EN", "CR")) %>%
  mutate(mean_percent_diff = mean(percent_diff, na.rm = TRUE))

non_threat_mean <- non_threat %>%
  mutate(mean_percent_diff = mean(percent_diff, na.rm = TRUE))

sim_results_iucn_pct_sum_stats <- sim_results_iucn_pct %>%
  group_by(redlist_category) %>%
  summarise(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    sd_percent_diff = sd(percent_diff, na.rm = TRUE),
    var_percent_diff = var(percent_diff, na.rm = TRUE)
  )

# get average mortality reductions per iucn status
sim_results_iucn_pct_plot_m_ave <- sim_results_iucn_pct %>%
  summarise(
    mean_percent_diff = mean(percent_diff, na.rm = TRUE),
    sd_percent_diff = sd(percent_diff, na.rm = TRUE)
  )

sim_results_iucn_pct_plot_m_ave2 <- sim_results_iucn_pct_threat %>%
  filter(redlist_category != "DD") %>%
  summarise(mean_percent_diff = mean(percent_diff, na.rm = TRUE)) %>%
  slice_head(n = 1)

# get average mortality reductions per main fished family
diff_fam <- sim_results_iucn_pct %>%
  group_by(family) %>%
  summarise(mean_diff = mean(percent_diff))

# 50% check
sim_results_iucn_pct_50 <- sim_results_iucn_pct %>%
  filter(percent_diff >= 50)



# Set a number of empty bars to add at the end of each group
empty_bar <- 7
to_add <- data.frame(matrix(NA, empty_bar * nlevels(sim_results_iucn_pct_threat$redlist_category), ncol(sim_results_iucn_pct_threat)))
colnames(to_add) <- colnames(sim_results_iucn_pct_threat)
to_add$redlist_category <- rep(levels(sim_results_iucn_pct_threat$redlist_category), each = empty_bar)
sim_results_iucn_pct_threat <- rbind(sim_results_iucn_pct_threat, to_add)
sim_results_iucn_pct_threat <- sim_results_iucn_pct_threat %>% arrange(redlist_category)
sim_results_iucn_pct_threat$id <- seq(1, nrow(sim_results_iucn_pct_threat))

# get the name and the y position of each label
label_data_m <- sim_results_iucn_pct_threat
number_of_bar <- nrow(label_data_m)
angle <- 90 - 360 * (label_data_m$id - 0.5) / number_of_bar
label_data_m$hjust <- ifelse(angle < -90, 1, 0)
label_data_m$angle <- ifelse(angle < -90, angle + 180, angle)

# Not sure how to use the code below
redlist_breaks <- sim_results_iucn_pct_threat %>%
  group_by(redlist_category) %>%
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

max_value <- max(sim_results_iucn_pct_threat$percent_diff, na.rm = T)

y_max <- 20 * ceiling(max_value / 20)

v <- c(20, 40, 60, 80, 100)

redlist_breaks <- redlist_breaks %>%
  mutate(v = list(v)) %>%
  unnest(cols = c(v))

prop <-
  ggplot(data = sim_results_iucn_pct_threat) +
  geom_segment(
    data = redlist_breaks %>%
      filter(v != 100),
    aes(x = end, y = v, xend = start, yend = v),
    color = "grey", linewidth = 0.3, inherit.aes = FALSE
  ) +
  annotate("text",
           x = rep(max(sim_results_iucn_pct_threat$id, length(v))),
           y = v - 5, label = paste0(head(v), "%"), color = "grey", size = 5, angle = 0, fontface = "bold", hjust = 0.7
  ) +
  geom_col(
    aes(
      x = id,
      y = percent_diff,
      fill = redlist_category
    ),
    show.legend = F,
    width = 0.8
  ) +
  ylim(-40, max(sim_results_iucn_pct_threat$percent_diff)) +
  scale_fill_manual(values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(
    data = label_data_m,
    aes(x = id, y = percent_diff + 20, label = scientific_name, angle = angle),
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
    aes(x = title, y = -18, label = redlist_category, color = redlist_category),
    show.legend = F,
    hjust = 1, alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("#E31A1C", "#FD8D3C", "#FED976")) +
  # geom_text(data = redlist_breaks,
  #             aes(x = title, y = 48, label = redlist_category),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE) +
  labs(fill = "Redlist Category") + # y = 80
  theme_void(base_size = 14) +
  labs(
    x = "",
    y = ""
  )
# prop

ggsave(prop, file = paste0("fig4.pdf"), path = here::here("figs"), height = 20, width = 20)





# Set a number of empty bars to add at the end of each group
empty_bar <- 7
to_add <- data.frame(matrix(NA, empty_bar * nlevels(non_threat$redlist_category), ncol(non_threat)))
colnames(to_add) <- colnames(non_threat)
to_add$redlist_category <- rep(levels(non_threat$redlist_category), each = empty_bar)
non_threat <- rbind(non_threat, to_add)
non_threat <- non_threat %>% arrange(redlist_category)
non_threat$id <- seq(1, nrow(non_threat))

# get the name and the y position of each label
label_data_m <- non_threat
number_of_bar <- nrow(label_data_m)
angle <- 90 - 360 * (label_data_m$id - 0.5) / number_of_bar
label_data_m$hjust <- ifelse(angle < -90, 1, 0)
label_data_m$angle <- ifelse(angle < -90, angle + 180, angle)

# Not sure how to use the code below
redlist_breaks <- non_threat %>%
  group_by(redlist_category) %>%
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

max_value <- max(non_threat$percent_diff, na.rm = T)

y_max <- 20 * ceiling(max_value / 20)

v <- c(20, 40, 60, 80, 100)

redlist_breaks <- redlist_breaks %>%
  mutate(v = list(v)) %>%
  unnest(cols = c(v))

prop <- ggplot(data = non_threat) +
  geom_segment(
    data = redlist_breaks %>%
      filter(v != 100),
    aes(x = end, y = v, xend = start, yend = v),
    color = "grey", linewidth = 0.3, inherit.aes = FALSE
  ) +
  annotate("text",
           x = rep(max(non_threat$id, length(v))),
           y = v - 5, label = paste0(head(v), "%"), color = "grey", size = 5, angle = 0, fontface = "bold", hjust = 0.7
  ) +
  geom_col(
    aes(
      x = id,
      y = percent_diff,
      fill = redlist_category
    ),
    show.legend = F,
    width = 0.8
  ) +
  ylim(-40, max(non_threat$percent_diff)) +
  scale_fill_manual(values = c("grey", "#91CF60", "#1A9850")) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(
    data = label_data_m,
    aes(x = id, y = percent_diff + 20, label = scientific_name, angle = angle),
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
    aes(x = title, y = -18, label = redlist_category, color = redlist_category),
    show.legend = F,
    hjust = 1, alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("grey", "#91CF60", "#1A9850")) +
  # geom_text(data = redlist_breaks,
  #             aes(x = title, y = 48, label = redlist_category),  colour = "black", alpha = 0.8, size = 5, fontface = "bold", inherit.aes = FALSE) +
  labs(fill = "Redlist Category") + # y = 80
  theme_void(base_size = 14) +
  labs(
    x = "",
    y = ""
  )
# prop

ggsave(prop, file = paste0("propeller_plot_non_threat.pdf"), path = here::here("figs", "supp"), height = 20, width = 20)



# Figure 5 and S5-10 ----------------------------------------------------------------

iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>%
  janitor::clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") |
           str_detect(scientific_name, "Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus")) %>% # list of genera to keep in the filtering
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

sim_results_common <- read_csv(here::here("data", "f_sim_results.csv")) %>%
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
  left_join(sim_results_common) %>%
  mutate(common_name = case_when(
    scientific_name == "Alopias vulpinus" ~ "Common thresher",
    scientific_name == "Squalus acanthias" ~ "Spiny dogfish",
    TRUE ~ common_name
  ))

sim_results_count <- sim_results_new %>%
  filter(t == 200 & mort_scenario == "Low Mortality" & n_div_k <= 0.5) %>%
  select(scientific_name) %>%
  distinct()

sim_results_total_count <- sim_results_new %>%
  select(scientific_name) %>%
  distinct()


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
  ))

length(unique(no_cq$scientific_name)) # 282

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
  geom_rect(data = no_cq_cr, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.5, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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

ggsave(p1, file = paste0("initial_sim_cr.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p2 <- ggplot() +
  geom_rect(data = no_cq_en, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.85, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p2, file = paste0("initial_sim_en.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p3 <- ggplot() +
  geom_rect(data = no_cq_vu, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.85, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p3, file = paste0("initial_sim_vu.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p4 <- ggplot() +
  geom_rect(data = no_cq_nt, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.65, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p4, file = paste0("initial_sim_nt.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

p5 <- ggplot() +
  geom_rect(data = no_cq_lc, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 2, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p5, file = paste0("initial_sim_lc.pdf"), path = here::here("figs", "supp"), height = 15, width = 18)

p6 <- ggplot() +
  geom_rect(data = no_cq_dd, aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.38, fill = as.factor(redlist_category))) +
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
  theme_bw(base_size = 14) +
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
    strip.text.x = element_text(color = "black", face = "bold.italic"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

ggsave(p6, file = paste0("initial_sim_dd.pdf"), path = here::here("figs", "supp"), height = 12, width = 12)

# Species Sub Plots -------------------------------------------------------

species_sub <- c(
  "Prionace glauca", "Carcharhinus limbatus", "Isurus oxyrinchus", "Squalus acanthias", "Alopias vulpinus",
  "Pseudocarcharias kamoharai", "Carcharhinus falciformis", "Sphyrna mokarran",
  "Carcharhinus hemiodon", "Squatina squatina", "Sphyrna corona", "Galeorhinus galeus"
)

no_cq_sub <- no_cq %>%
  filter(sci %in% species_sub) %>%
  mutate(scientific_name = case_when(
    scientific_name == "Blue shark" ~ "Blue shark (NT)",
    scientific_name == "Crocodile shark" ~ "Crocodile shark (LC)",
    scientific_name == "Common thresher" ~ "Common thresher (VU)",
    scientific_name == "Silky shark" ~ "Silky shark (VU)",
    scientific_name == "Shortfin mako" ~ "Shortfin mako (EN)",
    scientific_name == "Spiny dogfish" ~ "Spiny dogfish (VU)",
    scientific_name == "Pondicherry shark" ~ "Pondicherry shark (CR)",
    scientific_name == "Scalloped bonnethead" ~ "Scalloped bonnethead (CR)",
    scientific_name == "Angelshark" ~ "Angelshark (CR)",
    scientific_name == "Great hammerhead" ~ "Great hammerhead (CR)",
    scientific_name == "Blacktip shark" ~ "Blacktip shark (VU)",
    scientific_name == "Tope shark" ~ "Tope shark (CR)"
  ))

tag_text <- data.frame(
  t = c(180, 180, 180),
  n_div_k = c(0.98, 0.98, 0.98),
  label = c("A", "B", "C"),
  scientific_name = factor(c(
    "Blue shark (NT)", "Crocodile shark (LC)", "Common thresher (VU)", "Silky shark (VU)",
    "Shortfin mako (EN)", "Spiny dogfish (VU)", "Pondicherry shark (CR)", "Scalloped bonnethead (CR)",
    "Angelshark (CR)", "Great hammerhead (CR)", "Blacktip shark (VU)", "Tope shark (CR)"
  ))
)

p <- ggplot() +
  geom_hline(
    yintercept = 0.5,
    color = "gray",
    linetype = "dashed"
  ) +
  geom_rect(
    data = no_cq_sub,
    aes(xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.23, fill = as.factor(redlist_category))
  ) +
  geom_line(data = no_cq_sub, aes(t, n_div_k, color = mort_scenario, group = total_mort), linewidth = 2) +
  geom_text(
    data = tag_text %>%
      filter(scientific_name %in% c("Prionace glauca", "Isurus oxyrinchus", "Squatina squatina")),
    aes(x = t, y = n_div_k, label = label),
    color = "black",
    fontface = "bold",
    size = 5
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~ factor(scientific_name,
                      levels = c(
                        "Blue shark (NT)", "Crocodile shark (LC)", "Common thresher (VU)", "Silky shark (VU)",
                        "Shortfin mako (EN)", "Spiny dogfish (VU)", "Pondicherry shark (CR)", "Scalloped bonnethead (CR)",
                        "Angelshark (CR)", "Great hammerhead (CR)", "Blacktip shark (VU)", "Tope shark (CR)"
                      )
  )) +
  theme_bw(base_size = 20) +
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
p

ggsave(p, file = paste0("fig5.pdf"), path = here::here("figs"), height = 10, width = 20)


percent_calc <- sim_results %>%
  filter(scientific_name %in% iucn_data$scientific_name) %>%
  mutate(f_mort = ((100 * f) - (100 * f * (1 - mid_avm) * (1 - mid_prm))) / 100) %>%
  select(scientific_name, f, f_mort, mid_avm, mid_prm) %>%
  distinct() %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  mutate(
    mean_diff = mean(percent_diff, na.rm = TRUE),
    sd_diff = sd(percent_diff, na.rm = TRUE)
  )

species_of_interest <- c(
  "Galeocerdo cuvier", "Carcharhinus dussumieri",
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
  "Carcharhinus limbatus", "Galeorhinus galeus"
)

subset_percent <- percent_calc %>%
  filter(scientific_name %in% species_of_interest) %>%
  select(scientific_name, percent_diff)

write_csv(subset_percent, here::here("data", "table1.1.csv"))

percent_over_30 <- percent_calc %>%
  filter(percent_diff >= 50)

41 / 282 * 100

sim_200 <- sim_results %>%
  filter(scientific_name %in% iucn_data$scientific_name) %>%
  filter(t == 200) %>%
  filter(mort_scenario == "Median Mortality") %>%
  select(n_div_k, scientific_name) %>%
  filter(scientific_name %in% species_sub)

sim_over <- sim_200 %>%
  filter(n_div_k >= 0.5)

sim_under <- sim_200 %>%
  filter(n_div_k < 0.5)


# Figure S11 and Dryad data--------------------------------------------------------------

iucn_data <- read_csv(here::here("data", "iucn_data", "assessments.csv")) %>%
  janitor::clean_names() %>%
  filter(str_detect(systems, "Marine") & str_detect(threats, "longline") |
           str_detect(scientific_name, "Squatina|Isogomphodon|Carcharhinus|Eusphyra|Orectolobus|Pristiophorus|Mustelus")) %>% # list of genera to keep in the filtering
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

sim_results1 <- read_csv(here::here("data", "simulation_results_msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 1)
sim_results1_5 <- read_csv(here::here("data", "simulation_results.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 1.5)
sim_results2 <- read_csv(here::here("data", "simulation_results_2msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 2)
sim_results3 <- read_csv(here::here("data", "simulation_results_3msy.csv")) %>%
  filter(scenario != "CQ") %>%
  mutate(fp = 3)

sim_results <- list(sim_results1_5, sim_results1, sim_results2, sim_results3) %>%
  reduce(full_join) %>%
  filter(scientific_name %in% iucn_data$scientific_name)

rm(sim_results1_5, sim_results1, sim_results2, sim_results3)
gc()

eq <- sim_results %>%
  filter(t == 200) %>%
  filter(!is.na(mort_scenario)) %>%
  mutate(mort_scenario = fct_relevel(mort_scenario, "Low Mortality", after = Inf)) %>%
  mutate(scientific_name = fct_reorder(scientific_name, n_div_k))

p <- ggplot(eq) +
  geom_line(aes(fp, n_div_k, group = scientific_name), color = "grey") +
  geom_hline(
    yintercept = 0.5,
    color = "red",
    alpha = 0.5,
    linetype = "dashed"
  ) +
  facet_wrap(~mort_scenario, nrow = 4, scales = "free_y") +
  theme_bw(base_size = 14) +
  labs(
    x = "Fishing Pressure",
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

ggsave(p, file = paste0("fishing_sensitivity.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)


# results sum -------------------------------------------------------------

percent_calc <- sim_results %>%
  mutate(f_mort = ((100 * f) - 100 * f * mid_avm * mid_prm) / 100) %>%
  select(scientific_name, f, f_mort, fp) %>%
  distinct() %>%
  mutate(percent_diff = (f - f_mort) / f * 100) %>%
  ungroup() %>%
  group_by(fp) %>%
  mutate(mean_diff = mean(percent_diff, na.rm = TRUE)) %>%
  ungroup()

#
# onex = percent_calc %>%
#   filter(fp == 1)
#
# threex = percent_calc %>%
#   filter(fp == 3)
#
# percent_under_25= threex %>%
#   filter(pct_change <= 25)
#
# 434/466*100

sim_200 <- sim_results %>%
  filter(t == 200) %>%
  filter(mort_scenario == "Median Mortality") %>%
  select(n_div_k, scientific_name, fp) %>%
  filter(n_div_k >= 0.5) %>%
  group_by(fp) %>%
  summarize(per = n() / 282 * 100)

output <- eq %>%
  left_join(percent_calc) %>%
  select(-pop.array, -t, -mean_diff, -total_mort, -scenario, -quota) %>%
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
    fishing_mort_bau = f,
    fishing_mort_rb = f_mort,
    msy_multiple_fishing = fp
  ) %>%
  select(
    scientific_name, msy_multiple_fishing, fishing_mort_bau, fishing_mort_rb, percent_mort_diff,
    mort_scenario, simulation_avm, simulation_prm, n_div_k,
    pred_avm_25, pred_avm_mean, pred_avm_75, pred_prm_25, pred_prm_mean, pred_prm_75
  )

write_csv(output, here::here("figs", "table_s5.csv"))