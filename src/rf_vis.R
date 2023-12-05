library(tidyverse)
library(patchwork)
set.seed(42)

full_predictions <- read_csv(here::here("data", "full_model_predictions.csv"))

mean_predictions =  full_predictions %>% 
  mutate(group = case_when(
    str_detect(family, "Mobulidae|Dasyatiade|Gymnyridae|Myliobatidae|Torpedinidae|Rhinobatidae|Rhinidae|Aetobatidae|Rajidae|Pristidae") ~ "Batoids",
    str_detect(scientific_name, "Himantura|Dasyatis|Gymnura|trygon|Bathytoshia|rays|raja|Rhinoptera|Sympterygia|Pastinachus|Urobatis|Glaucostegus|Hypanus") ~ "Batoids",
    TRUE ~ "Sharks"
  )) %>% 
  group_by(scientific_name) %>% 
  mutate(avm_pred = mean(avm_pred),
            prm_pred = mean(prm_pred),
            group = as.factor(group)) %>% 
  mutate(group = fct_relevel(group, c("Sharks", "Batoids")))

# write_csv(mean_predictions, here::here("data", "full_pred_table.csv"))

predictions = mean_predictions %>% 
  pivot_longer(cols = c("avm_pred", "prm_pred"), names_to = "estimate_type", values_to = "mortality_prop") %>% 
  mutate(estimate_type = case_when(
    estimate_type == "avm_pred" ~ "AVM",
    estimate_type == "prm_pred" ~ "PRM"
  ))

p1 = ggplot(predictions) +
  geom_point(aes(max_size_cm, mortality_prop, color = reproductive_mode)) +
  # geom_smooth(method = "loess", aes(max_size_cm, mortality_prop, color = reproductive_mode), se = FALSE) +
  theme_bw() +
  labs(y = "Mortality",
       x = "Size (cm)",
       color = "Reproductive Mode") +
  facet_wrap(~ estimate_type, scales = "free_x") +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  scale_color_viridis_d()

p2 = ggplot(predictions) +
  geom_point(aes(median_depth, mortality_prop, color = reproductive_mode)) +
  # geom_smooth(method = "loess", aes(median_depth, mortality_prop, color = reproductive_mode), se = FALSE) +
  theme_bw() +
  labs(y = "Mortality",
       x = "Median Depth",
       color = "Reproductive Mode") +
  facet_wrap(~ estimate_type, scales = "free_x") +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  scale_color_viridis_d()


p3 = ggplot(predictions %>% filter(estimate_type == "AVM")) +
  geom_boxplot(aes(mortality_prop, ventilation_method, fill = group), outlier.alpha = 0) +
  geom_jitter(aes(mortality_prop, ventilation_method, color = group), alpha = 0.1) +
  theme_bw() +
  labs(x = "Mortality",
       y = "Ventilation Method",
       fill = "",
       color = "") +
  facet_wrap(~ estimate_type, scales = "free_x") +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

p4 = ggplot(predictions %>% filter(estimate_type == "PRM")) +
  geom_boxplot(aes(mortality_prop, habitat_associated, fill = group), outlier.alpha = 0) +
  geom_jitter(aes(mortality_prop, habitat_associated, color = group), alpha = 0.1) +
  theme_bw() +
  labs(x = "Mortality",
       y = "Habitat",
       fill = "",
       color = "") +
  facet_wrap(~ estimate_type, scales = "free_x") +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

plot = p1  / p2 / (p3 + p4) + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
plot

ggsave(plot, file = paste0("preds_predictors.pdf"), path = here::here("figs"), height = 12, width = 10)

mort_subset <- mean_predictions %>%
  group_by(family) %>% 
  summarize(fam_mean = mean(avm_pred)) %>% 
  arrange(fam_mean) %>%
  distinct(family) %>%
  pull(family)

p5 = ggplot(data = predictions) +
  geom_boxplot(aes(x = fct_rev(factor(family, levels = mort_subset)),
                y = mortality_prop,
                group = family),
            show.legend = F,
            color = "gray30",
            linewidth = 0.6,
            alpha = 0.5) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     limits = c(0.05, 0.8),
                     expand = c(0.01, 0.01)) +
  labs(x = "Family",
       y = "Mean estimated mortality proportion") +
  scale_shape(guide = 'none') +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  facet_grid(rows = vars(factor(group, levels = c("Sharks", "Batoids"))),
             cols = vars(estimate_type),
             scales = "free_y",
             space = "free_y")

ggsave(p5, file = paste0("family_pred.pdf"), path = here::here("figs"), height = 12, width = 10)


predictions_fig = ggplot(data = predictions) +
  geom_line(aes(x = fct_rev(factor(scientific_name, levels = mort_subset)),
                y = mortality_prop,
                group = scientific_name),
            show.legend = F,
            color = "gray30",
            linewidth = 0.6,
            alpha = 0.5) +
  coord_flip() +
  theme_bw() +
  geom_point(aes(x = fct_rev(factor(scientific_name, levels = mort_subset)),
                 y = mortality_prop,
                 color = estimate_type),
             size = 1.5,
             alpha = 0.9,
             show.legend = F) +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     limits = c(0.05, 0.8),
                     expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("#414487FF", "#22A884FF")) +
  labs(x = "",
       y = "Mean estimated mortality proportion",
       color = "Estimate type") +
  scale_shape(guide = 'none') +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.texty = element_text(face = "italic"),
        axis.title = element_text(size = 11, color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.x = unit(5, "mm")) +
  facet_grid(rows = vars(factor(group, levels = c("Sharks", "Batoids"))),
             scales = "free_y",
             space = "free_y")
# predictions_fig
# 
# ggsave(predictions_fig, file = paste0("predictions_fig.pdf"), path = here::here("figs"), height = 20, width = 15)
