library(tidyverse)
library(FishLife)

predictions_full = read_csv(here::here("data", "full_model_predictions.csv")) 

r_growth = read_csv(here::here("data", "intrinsic_pop_growth_rates_full.csv")) %>% 
  select(scientific_name, mean_r) %>% 
  distinct()

fish_data = FishLife::FishBase_and_RAM$beta_gv %>% 
  as.data.frame()

data_filled = data.frame()

predictions = predictions_full %>% 
  filter(!scientific_name %in% c("Centroscymnus owstoni", "Carcharhinus limbatus/tilstoni", 
                                 "Bythaelurus bachi", "Bythaelurus naylori",
                                 "Bythaelurus tenuicephalus", "Carcharhinus obsoletus",
                                 "Centrophorus lesliei", "Centrophorus longipinnis",
                                 "Centroselachus crepidater", "Cephaloscyllium formosanum",
                                 "Chiloscyllium hasselti", "Etmopterus alphus",
                                 "Etmopterus brosei", "Etmopterus lailae", 
                                 "Etmopterus marshae", "Etmopterus sheikoi",
                                 "Hexanchus vitulus", "Mustelus andamanensis",
                                 "Parmaturus angelae", "Planonasus indicus", 
                                 "Pliotrema annae", "Pliotrema kajae",
                                 "Scymnodon ichiharai", "Scymnodon plunketi",
                                 "Squalus acutipinnis", "Squalus albicaudus",
                                 "Squalus bahiensis", "Squalus bassi", "Squalus boretzi",
                                 "Squalus clarkae", "Squalus hawaiiensis", "Squalus lobularis",
                                 "Squalus mahia", "Squalus margaretsmithae",
                                 "Squalus quasimodo", "Squatina david",
                                 "Stegostoma tigrinum", "Acroteriobatus annulatus",
                                 "Acroteriobatus blochii",  "Acroteriobatus leucospilus",
                                 "Acroteriobatus ocellatus", "Acroteriobatus omanensis", 
                                 "Acroteriobatus salalah", "Acroteriobatus variegatus",
                                 "Glaucostegus cemiculus", "Glaucostegus obtusus", 
                                 "Glaucostegus thouin", "Pseudobatos buthi",
                                 "Pseudobatos glaucostigmus","Pseudobatos horkelii",
                                 "Pseudobatos lentiginosus","Pseudobatos leucorhynchus",
                                 "Pseudobatos percellens", "Pseudobatos planiceps", 
                                 "Pseudobatos prahli", "Pseudobatos productus", 
                                 "Rhinobatos austini", "Rhinobatos borneensis", 
                                 "Rhinobatos ranongensis"))

for(i in 1:nrow(predictions)) {
  species = Match_species(genus_species = predictions$scientific_name[i])
  
  data_filled = rbind(fish_data[species[[1]],], data_filled)
}

predictions$fish_r = data_filled$r


pred_r = left_join(predictions_full, r_growth) %>% 
  distinct() %>% 
  left_join(predictions)%>% 
  mutate(mid_avm = case_when(
    !is.na(avm_mort) ~ avm_mort, 
    is.na(avm_mort) ~ avm_pred
  )) %>% 
  mutate(mid_prm = case_when(
    !is.na(prm_mort) ~ prm_mort, 
    is.na(prm_mort) ~ prm_pred
  )) %>% 
  mutate(r_value = case_when(
    mean_r < 0 ~ fish_r,
    !is.na(mean_r) ~ mean_r,
    !is.na(fish_r) ~ fish_r
  ))  %>% 
  filter(!is.na(r_value)) %>% 
  mutate(dif_r = mean_r - fish_r)

write_csv(pred_r, here::here("data", "simulation_data.csv"))
