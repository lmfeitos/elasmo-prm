library(tidyverse)
library(patchwork)
set.seed(42)

### Scenarios

## Focus on Longline

# 1 BAU "BAU"
# 2 Retention Ban ("RB")
# 3 Catch Quota ("CQ") usually in weight needs editing - also possibly not added at the beginning inform based on real world. Could we simulate an optimal quota? Average

sim_data <- read_csv(here::here("data", "simulation_data.csv"))

# Functions ---------------------------------------------------------------

sim <- function(t, N_0, K, r, avs, prs, q, f, quota, scenario) {
  
  # set possible survival when thrown back to 0 as none are thrown back in BAU
  if (scenario == "BAU") {
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
f <- 0.2 # vary for sensitivity
quota <- 10 # in count
quota.array = c(1, 5, 10, 20) # in count

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

  ## single business as usual scenario
  bau <- sim(t, N_0, K, r, 1 - morts$avm[1], 1 - morts$prm[1], q, f, quota, "BAU") %>%
    mutate(
      avm = 1,
      prm = 1,
      scientific_name = species, 
      quota = 0
    )

  sim_results <- rbind(sim_results, bau)

  ## loop through mortality values for retention ban and catch quota
  for (j in 1:nrow(morts)) {
    rb <- sim(t, N_0, K, r, 1 - morts$avm[j], 1 - morts$prm[j], q, f, quota, "RB") %>%
      mutate(
        avm = morts$avm[j],
        prm = morts$prm[j],
        scientific_name = species,
        quota = 0
      )
    
    sim_results <- rbind(sim_results, rb)
    
    for (k in 1:length(quota.array)) {
      cq <- sim(t, N_0, K, r, 1 - morts$avm[j], 1 - morts$prm[j], q, f, quota.array[k], "CQ") %>%
        mutate(
          avm = morts$avm[j],
          prm = morts$prm[j],
          scientific_name = species,
          quota = quota.array[k]
        )
      
      sim_results <- rbind(sim_results, cq)
    }

  }
}

sim_results <- sim_results %>%
  mutate(total_mort = avm * prm) %>%
  full_join(sim_data) %>%
  mutate(mort_scenario = case_when(
    scenario == "BAU" ~ "BAU",
    avm == avm_75 & prm == prm_75 ~ "High Mortality",
    avm == avm_25 & prm == prm_25 ~ "Low Mortality",
    avm == mid_avm & prm == mid_prm ~ "Median Mortality",
    TRUE ~ "In-Between Mortality"
  )) %>%
  mutate(mort_scenario = fct_relevel(mort_scenario, c("BAU", "Low Mortality", "In-Between Mortality", "Median Mortality", "High Mortality"))) %>% 
  mutate(n_div_k = pop.array/K)

no_cq = sim_results %>% 
  filter(scenario != "CQ")

p <- ggplot(no_cq, aes(t, n_div_k)) +
  geom_line(aes(color = scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~scientific_name,
             labeller = label_wrap_gen(30)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black"))
p

ggsave(p, file = paste0("initial_sim_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)

sim_sub = sim_results %>% 
  filter(t == 10 | t == 200) %>% 
  mutate(t = as.factor(t)) %>% 
  distinct()

no_cg_sub = sim_sub %>% 
  filter(scenario != "CQ")

errors_mort = no_cg_sub %>% 
  select(t, scenario, avm, prm, scientific_name, mort_scenario, pop.array, n_div_k, total_mort) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(scientific_name, mort_scenario) %>% 
  filter(total_mort == min(total_mort) | total_mort == max(total_mort)) %>% 
  filter(mort_scenario %in% c("BAU", "Low Mortality", "High Mortality")) %>% 
  distinct() %>% 
  ungroup()

p2 = ggplot(errors_mort) +
  geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_point(aes(t, n_div_k, color = mort_scenario)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~scientific_name,
             labeller = label_wrap_gen(30)) +
  facet_wrap(~scientific_name) +
  theme_bw() +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario"
  ) + 
  scale_color_viridis_d()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black"))
p2

ggsave(p2, file = paste0("timepoints_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)


# Species Sub Plots -------------------------------------------------------
species_sub = c("Lamna nasus", "Alopias vulpinus", "Carcharhinus galapagensis", "Carcharhinus limbatus", "Galeocerdo cuvier", "Isurus oxyrinchus", "Rhincodon typus", "Sphyrna mokarran", "Squalus acanthias")

no_cq = no_cq %>% 
  filter(scientific_name %in% species_sub)

p <- ggplot(no_cq, aes(t, n_div_k)) +
  geom_line(aes(color = scenario, group = total_mort)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~scientific_name,
             labeller = label_wrap_gen(30)) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black"))
p

ggsave(p, file = paste0("species_full_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)

errors_mort = errors_mort %>% 
  filter(scientific_name %in% species_sub)

p2 = ggplot(errors_mort) +
  geom_line(aes(t, n_div_k, color = mort_scenario, group = total_mort)) +
  geom_point(aes(t, n_div_k, color = mort_scenario)) +
  geom_hline(yintercept = 0.5,
             color = "gray",
             alpha = 0.5,
             linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_wrap(~scientific_name,
             labeller = label_wrap_gen(30)) +
  facet_wrap(~scientific_name) +
  theme_bw() +
  labs(
    x = "Time",
    y = "N/K",
    color = "Scenario"
  ) + 
  scale_color_viridis_d()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold.italic"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 10, color = "black"))
p2

ggsave(p2, file = paste0("species_timepoints_0_2.pdf"), path = here::here("figs"), height = 15, width = 20)

# species plots -----------------------------------------------------------
# Galeocerdo_cuvier <- sim_results %>%
#   filter(scientific_name == "Galeocerdo cuvier")
# 
# ggplot(Galeocerdo_cuvier, aes(t, pop.array)) +
#   geom_line(aes(color = scenario, group = total_mort)) +
#   theme_bw()
# 
# Carcharhinus_limbatus <- sim_results %>%
#   filter(scientific_name == "Carcharhinus limbatus")
# 
# ggplot(Carcharhinus_limbatus, aes(t, pop.array)) +
#   geom_line(aes(color = scenario, group = total_mort)) +
#   theme_bw()
# 
# Isurus_oxyrinchus <- sim_results %>%
#   filter(scientific_name == "Isurus oxyrinchus")
# 
# ggplot(Isurus_oxyrinchus, aes(t, pop.array)) +
#   geom_line(aes(color = scenario, group = total_mort)) +
#   theme_bw()
# 
# Alopias_vulpinus <- sim_results %>%
#   filter(scientific_name == "Alopias vulpinus")
# 
# ggplot(Alopias_vulpinus, aes(t, pop.array)) +
#   geom_line(aes(color = scenario, group = total_mort)) +
#   theme_bw()

# Simulation Tests --------------------------------------------------------
# test_bau = sim(t, N_0, K, r, avs, prs, q, f, quota, "BAU")
# test_rb = sim(t, N_0, K, r, avs, prs, q, f, quota, "RB")
# test_cq = sim(t, N_0, K, r, avs, prs, q, f, quota, "CQ")
#
# test_sim = list(test_bau, test_rb, test_cq) %>%
#   reduce(full_join)
#
# ggplot(test_sim, aes(t, pop.array, color = scenario)) +
#   geom_line(linewidth = 2) +
#   theme_bw()
