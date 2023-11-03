library(tidyverse)

avm_longline = read_csv(here::here("data", "avm_model_predictions.csv")) %>% 
  rename(avm_75 = .pred_75,
         avm_25 = .pred_25,
         avm_50 = .pred_50,
         avm_pred = .pred, 
         avm_mort = mortality_prop) %>% 
  select(avm_pred, avm_75, avm_25, avm_50, scientific_name, avm_mort)
prm_longline = read_csv(here::here("data", "prm_model_predictions.csv")) %>% 
  rename(prm_75 = .pred_75,
         prm_25 = .pred_25,
         prm_50 = .pred_50,
         prm_pred = .pred, 
         prm_mort = mortality_prop) %>% 
  select(prm_pred, prm_75, prm_25, prm_50, scientific_name, prm_mort)

predictions = list(avm_longline, prm_longline) %>% 
  reduce(full_join) %>% 
  distinct() %>% 
  drop_na() 

r_growth = read_csv(here::here("data", "intrinsic_pop_growth_rates.csv")) %>% 
  select(scientific_name, mean_r)

pred_r = left_join(predictions, r_growth) %>% 
  distinct() %>% 
  drop_na() 

write_csv(pred_r, here::here("data", "simulation_data.csv"))
