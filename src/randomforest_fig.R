library(tidyverse)

predictions = read_csv(here::here("data", "full_model_predictions.csv"))
trawl = read_csv(here::here("data", "trawl_model_predictions.csv")) 
gillnet = read_csv(here::here("data", "gillnet_model_predictions.csv")) 

