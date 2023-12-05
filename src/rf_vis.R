library(tidyverse)
library(patchwork)
set.seed(42)

full_predictions <- read_csv(here("data", "full_model_predictions.csv"))

mean_predictions =  full_predictions %>% 
  mutate(group = case_when(
    str_detect(family, "Mobulidae|Dasyatiade|Gymnyridae|Myliobatidae|Torpedinidae|Rhinobatidae|Rhinidae|Aetobatidae|Rajidae|Pristidae") ~ "Batoids",
    str_detect(scientific_name, "Himantura|Dasyatis|Gymnura|trygon|Bathytoshia|rays|raja|Rhinoptera|Sympterygia|Pastinachus|Urobatis|Glaucostegus|Hypanus") ~ "Batoids",
    TRUE ~ "Sharks"
  )) %>% 
  group_by(scientific_name) %>% 
  summarize(avm_pred = mean(avm_pred),
            prm_pred = mean(prm_pred),
            group = as.factor(group)) %>% 
  mutate(group = fct_relevel(group, c("Sharks", "Batoids")))

predictions = mean_predictions %>% 
  pivot_longer(cols = c("avm_pred", "prm_pred"), names_to = "estimate_type", values_to = "mortality_prop")

mort_subset <- mean_predictions %>% 
  arrange(avm_pred) %>% 
  select(scientific_name, avm_pred) %>% 
  distinct(scientific_name) %>% 
  pull(scientific_name) 

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
predictions_fig

ggsave(predictions_fig, file = paste0("predictions_fig.pdf"), path = here::here("figs"), height = 20, width = 15)
