library(tidyverse)

predictions = read_csv(here::here("data", "full_model_predictions.csv")) 

r_growth = read_csv(here::here("data", "intrinsic_pop_growth_rates_full.csv")) %>% 
  select(scientific_name, mean_r) %>% 
  distinct()

pred_r = left_join(predictions, r_growth) %>% 
  distinct() %>% 
  mutate(mid_avm = case_when(
    !is.na(avm_mort) ~ avm_mort, 
    is.na(avm_mort) ~ avm_pred
  )) %>% 
  mutate(mid_prm = case_when(
    !is.na(prm_mort) ~ prm_mort, 
    is.na(prm_mort) ~ prm_pred
  )) %>% 
  rename(r_value = mean_r) %>% 
  filter(!is.na(r_value))

write_csv(pred_r, here::here("data", "simulation_data.csv"))
