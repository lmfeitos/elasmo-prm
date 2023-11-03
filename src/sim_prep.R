library(tidyverse)

predictions = read_csv(here::here("data", "full_model_predictions.csv")) 

r_growth = read_csv(here::here("data", "intrinsic_pop_growth_rates.csv")) %>% 
  select(scientific_name, mean_r)

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
  mutate(r_value = case_when(
    !is.na(mean_r) ~ mean_r,
    is.na(mean_r) ~ mean(r_growth$mean_r)
  ))

write_csv(pred_r, here::here("data", "simulation_data.csv"))
