library(tidyverse)

sim_results2 <- read_csv(here::here("data", "simulation_results.csv")) %>% 
  mutate(fp = 0.2)%>% 
  filter(scenario != "CQ")
sim_results1 <- read_csv(here::here("data", "simulation_results_1.csv")) %>% 
  mutate(fp = 0.1)%>% 
  filter(scenario != "CQ")
sim_results3 <- read_csv(here::here("data", "simulation_results_3.csv")) %>% 
  mutate(fp = 0.3)%>% 
  filter(scenario != "CQ")
sim_results4 <- read_csv(here::here("data", "simulation_results_4.csv")) %>% 
  mutate(fp = 0.4)%>% 
  filter(scenario != "CQ")
sim_results5 <- read_csv(here::here("data", "simulation_results_5.csv")) %>% 
  mutate(fp = 0.5) %>% 
  filter(scenario != "CQ")
sim_results05 <- read_csv(here::here("data", "simulation_results_05.csv")) %>% 
  mutate(fp = 0.05)%>% 
  filter(scenario != "CQ")

sim_results = list(sim_results05, sim_results5, sim_results4, sim_results3,
                   sim_results1, sim_results2) %>% 
  reduce(full_join) 

rm(sim_results05, sim_results5, sim_results4, sim_results3, sim_results1, sim_results2)
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

ggsave(p, file = paste0("fishing_sensitivity.pdf"), path = here::here("figs"), height = 10, width = 8)
  