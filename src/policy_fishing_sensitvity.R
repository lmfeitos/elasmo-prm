library(tidyverse)

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
  reduce(full_join) %>% 
  filter(scientific_name %in% iucn_data$scientific_name)

rm(sim_results1_5, sim_results1, sim_results2, sim_results3)
gc()

eq = sim_results %>% 
  filter(t == 200) %>% 
  filter(!is.na(mort_scenario)) %>% 
  mutate(mort_scenario = fct_relevel(mort_scenario, "Low Mortality", after = Inf)) %>% 
  mutate(scientific_name = fct_reorder(scientific_name, n_div_k))

p = ggplot(eq) +
  geom_line(aes(fp, n_div_k, group = scientific_name), color = "grey") +
  geom_hline(yintercept = 0.5,
             color = "red",
             alpha = 0.5,
             linetype = "dashed") +
  facet_wrap(~mort_scenario, nrow = 4, scales = "free_y") +
  theme_bw(base_size = 14)+
  labs(x = "Fishing Pressure",
       y = "N/K") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(color = "black", face = "bold.italic"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "none") 
p

ggsave(p, file = paste0("fishing_sensitivity.pdf"), path = here::here("figs", "supp"), height = 10, width = 8)
  

# results sum -------------------------------------------------------------

percent_calc = sim_results %>% 
  mutate(f_mort = ((100 * f) - 100 * f * mid_avm * mid_prm) / 100) %>% 
  select(scientific_name, f, f_mort, fp) %>% 
  distinct()%>% 
  mutate(percent_diff = (f - f_mort) / f * 100) %>% 
  ungroup() %>% 
  group_by(fp) %>% 
  mutate(mean_diff = mean(percent_diff, na.rm=TRUE)) %>% 
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

sim_200 = sim_results %>% 
  filter(t == 200) %>% 
  filter(mort_scenario == "Median Mortality") %>% 
  select(n_div_k, scientific_name, fp) %>% 
  filter(n_div_k >= 0.5) %>% 
  group_by(fp) %>% 
  summarize(per = n() / 282*100)

output = eq  %>% 
  left_join(percent_calc)%>% 
  select(-pop.array, -t, -mean_diff, -total_mort, -scenario, -quota) %>% 
  rename(simulation_avm = avm,
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
         msy_multiple_fishing = fp) %>% 
  select(scientific_name, msy_multiple_fishing, fishing_mort_bau, fishing_mort_rb, percent_mort_diff, 
         mort_scenario, simulation_avm, simulation_prm, n_div_k, 
         pred_avm_25, pred_avm_mean, pred_avm_75, pred_prm_25, pred_prm_mean, pred_prm_75)

write_csv(output, here::here("figs", "table_s5.csv"))
