library(tidyverse)

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
  filter(scenario != "CQ")%>% 
  mutate(fp = 3)

sim_results = list(sim_results1_5, sim_results1, sim_results2, sim_results3) %>% 
  reduce(full_join) 

rm(sim_results1_5, sim_results1, sim_results2, sim_results3)
gc()

eq = sim_results %>% 
  filter(t == 200) %>% 
  filter(!is.na(mort_scenario)) %>% 
  mutate(mort_scenario = fct_relevel(mort_scenario, "Low Mortality", after = Inf)) %>% 
  mutate(scientific_name = fct_reorder(scientific_name, n_div_k))

p = ggplot(eq) +
  geom_line(aes(fp, n_div_k, color = scientific_name)) +
  geom_hline(yintercept = 0.5,
             color = "red",
             alpha = 0.5,
             linetype = "dashed") +
  facet_wrap(~mort_scenario, nrow = 4, scales = "free_y") +
  theme_bw()+
  labs(x = "Fishing Pressure",
       y = "N/K") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black"),
        legend.position = "none") +
  scale_color_viridis_d()
p

ggsave(p, file = paste0("fishing_sensitivity.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)
  

# results sum -------------------------------------------------------------

percent_calc = sim_results %>% 
  filter(t == 200) %>% 
  filter(mort_scenario == "BAU" | mort_scenario == "Median Mortality") %>% 
  mutate(n_div_k = n_div_k - 1) %>% 
  arrange(scientific_name, mort_scenario, fp)%>% 
  select(scientific_name, mort_scenario, n_div_k, fp, avm, prm) %>% 
  group_by(scientific_name, fp) %>% 
  mutate(lag = lag(n_div_k)) %>% 
  filter(mort_scenario == "Median Mortality") %>% 
  mutate(pct_change = (lag - n_div_k) / lag * 100) %>% 
  ungroup() %>% 
  group_by(fp) %>% 
  mutate(mean = mean(pct_change)) %>% 
  ungroup() %>% 
  mutate(mort = 1 - avm * prm)

onex = percent_calc %>% 
  filter(fp == 1)

threex = percent_calc %>% 
  filter(fp == 3)

percent_under_25= threex %>% 
  filter(pct_change <= 25)

434/466*100
