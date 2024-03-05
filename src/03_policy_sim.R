library(tidyverse)
library(patchwork)
set.seed(42)

### Scenarios

## Focus on Longline

# 1 Full Retention "FR"
# 2 Retention Ban ("RB")

sim_data <- read_csv(here::here("data", "simulation_data.csv")) %>%
  select(prm_75, prm_25, avm_25, avm_75, scientific_name, r_value, mid_avm, mid_prm) %>% 
  group_by(scientific_name) %>% 
  mutate(prm_75 = max(prm_75),
         prm_25 = min(prm_25),
         mid_prm = mean(mid_prm),
         avm_75 = max(avm_75),
         avm_25 = min(avm_25),
         mid_avm = mean(mid_avm),
         r_value = mean(r_value)) %>% 
  distinct() %>% 
  mutate(msy = r_value / 2 ) %>%
  mutate(f = 1.5 * msy)
    

# Functions ---------------------------------------------------------------

sim <- function(t, N_0, K, r, avs, prs, q, f, quota, scenario) {
  
  # set possible survival when thrown back to 0 as none are thrown back in FR
  if (scenario == "FR") {
    avs <- 0
    prs <- 0
  }

  # there is not quota so set it to 0
  if (scenario != "CQ") {
    quota <- 0
  }

  pop.array <- array(numeric(), dim = c(t)) # create population array
  pop.array[1] <- N_0 # initialize population

  for (i in 2:t) {
    dt <- 1

    # population growth - fishing + returned bycatch
    dN.dt = (r * pop.array[i - 1] * (1 - (pop.array[i - 1] / K))) - (q * f * pop.array[i - 1]) + ((q * f * pop.array[i - 1] - quota) * avs * prs) * dt
    pop.array[i] <- pop.array[i - 1] + dN.dt
  }

  # ((q * f * pop.array[i - 1]) * (1 - quota)) * avs * prs) # by percent
  # ((q * f * pop.array[i - 1]) - quota) *  avs * prs) # based on count

  pop_df <- as.data.frame(pop.array) %>%
    mutate(t = 1:t) %>% # add time column
    mutate(scenario = scenario) %>%
    filter(pop.array > 0)

  return(pop_df)
}

# Set Up ------------------------------------------------------------------

t <- 200
K <- 100
# r = 0.2 #set for each shark
N_0 <- 100
# avs = 0.8 # at vessel survival; higher = better for shark (test quantile range from RF)
# prs = 0.8 # post release survival; higher = better for shark (test quantile range from RF)
q <- 1 # catchability, set to 1 for ease of fishing pressure, could vary
# f <- 0.2 # vary for sensitivity
# quota <- 0 # in count
quota.array = c(5) # in count

# shark sim loop ----------------------------------------------------------

## empty dataframe
sim_results <- data.frame()

for (i in 1:nrow(sim_data)) {
  ## set up mortality grid for shark species
  avms <- c(sim_data$avm_25[i], sim_data$mid_avm[i], sim_data$avm_75[i])
  prms <- c(sim_data$prm_25[i], sim_data$mid_prm[i], sim_data$prm_75[i])
  morts <- expand.grid(avms, prms) %>% 
    distinct()
  names(morts) <- c("avm", "prm")

  species <- sim_data$scientific_name[i]
  r <- sim_data$r_value[i]
  
  f <- sim_data$f[i]

  ## single business as usual scenario
  bau <- sim(t, N_0, K, r, 1 - morts$avm[1], 1 - morts$prm[1], q, f, quota, "FR") %>%
    mutate(
      avm = 1,
      prm = 1,
      scientific_name = species, 
      quota = 0, 
      f = f
    )

  sim_results <- rbind(sim_results, bau)

  ## loop through mortality values for retention ban and catch quota
  for (j in 1:nrow(morts)) {
    rb <- sim(t, N_0, K, r, 1 - morts$avm[j], 1 - morts$prm[j], q, f, quota, "RB") %>%
      mutate(
        avm = morts$avm[j],
        prm = morts$prm[j],
        scientific_name = species,
        quota = 0,
        f = f
      )
    
    sim_results <- rbind(sim_results, rb)
    
    # for (k in 1:length(quota.array)) {
    #   cq <- sim(t, N_0, K, r, 1 - morts$avm[j], 1 - morts$prm[j], q, f, quota.array[k], "CQ") %>%
    #     mutate(
    #       avm = morts$avm[j],
    #       prm = morts$prm[j],
    #       scientific_name = species,
    #       quota = quota.array[k]
    #     )
    # 
    #   sim_results <- rbind(sim_results, cq)
    # }

  }
}

join_data = sim_data %>% 
  select(scientific_name, avm_75, prm_75, avm_25, prm_25, mid_avm, mid_prm)

sim_results <- sim_results %>%
  mutate(total_mort = avm * prm) %>%
  full_join(join_data) %>%
  mutate(n_div_k = pop.array/K)

species_max_mort = sim_results %>% 
  filter(scenario != "FR") %>% 
  group_by(scientific_name) %>% 
  summarize(max_mort = max(total_mort))
species_min_mort = sim_results %>% 
  filter(scenario != "FR") %>% 
  group_by(scientific_name) %>% 
  summarize(min_mort = min(total_mort))
species_median_mort = sim_results %>% 
  filter(scenario != "FR") %>% 
  select(scientific_name, avm, mid_avm, prm, mid_prm, total_mort) %>% 
  distinct() %>%
  filter(avm == mid_avm & prm == mid_prm) %>% 
  group_by(scientific_name) %>% 
  summarize(med_mort = max(total_mort))

summarized = list(species_max_mort, species_median_mort, species_min_mort, sim_results) %>% 
  reduce(full_join) %>% 
  mutate(mort_scenario = case_when(
    scenario == "FR" ~ "Full Retention",
    total_mort == med_mort ~ "Median Mortality",
    total_mort == min_mort ~ "Low Mortality",
    total_mort == max_mort ~ "High Mortality"
  )) %>% 
  select(-med_mort, -min_mort, -max_mort)

write_csv(summarized, here::here("data", "simulation_results.csv"))
