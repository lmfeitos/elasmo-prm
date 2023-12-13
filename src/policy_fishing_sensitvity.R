library(tidyverse)

sim_results2 <- read_csv(here::here("data", "simulation_results.csv")) %>% 
  mutate(fp = 0.2)
sim_results1 <- read_csv(here::here("data", "simulation_results_1.csv")) %>% 
  mutate(fp = 0.1)
sim_results3 <- read_csv(here::here("data", "simulation_results_3.csv")) %>% 
  mutate(fp = 0.3)
sim_results4 <- read_csv(here::here("data", "simulation_results_4.csv")) %>% 
  mutate(fp = 0.4)
sim_results5 <- read_csv(here::here("data", "simulation_results_5.csv")) %>% 
  mutate(fp = 0.5)
sim_results05 <- read_csv(here::here("data", "simulation_results_05.csv")) %>% 
  mutate(fp = 0.05)

sim_results = list(sim_results05, sim_results5, sim_results4, sim_results3,
                   sim_results1, sim_results2) %>% 
  reduce(full_join) 

rm(sim_results05, sim_results5, sim_results4, sim_results3, sim_results1, sim_results2)