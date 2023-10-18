library(tidyverse)
library(patchwork)
set.seed(42)

### Scenarios

# 1 BAU "BAU"
# 2 Retention Ban ("RB")
# 3 Catch Quota ("CQ") usually in weight needs editing - also possibly not added at the beginning 

# Functions ---------------------------------------------------------------

sim <- function(t, N_0, K, r, avs, prs, q, f, quota, scenario) {
  
  # set possible survival when thrown back to 0 as none are thrown back in BAU and QC
  if(scenario == "BAU") {
    avs = 0
    prs = 0
  }
  
  # there is not quota so set it to 0
  if(scenario != "CQ") {
    quota = 0
  }
  
  pop.array <- array(numeric(), dim = c(t)) # create population array
  pop.array[1] <- N_0 # initialize population
  
  for (i in 2:t) {
    dt = 1
    
    # population growth - fishing + returned bycatch 
    dN.dt = (r * pop.array[i - 1] * (1 - (pop.array[i - 1] / K))) - ceiling(q * f * pop.array[i - 1]) + floor(((q * f * pop.array[i - 1]) * (1 - quota)) *  avs * prs) * dt
    pop.array[i] = pop.array[i - 1] + dN.dt

  }
  
  #ifelse(pop.array[i - 1] > K / 4, floor(((q * f * pop.array[i - 1]) - quota) *  avs * prs), floor((q * f * pop.array[i - 1]) *  avs * prs)) * dt
  
  pop_df <- as.data.frame(pop.array) %>%
    mutate(t = 1:t) %>%  # add time column
    mutate(scenario = scenario) %>% 
    filter(pop.array > 0)
  
  return(pop_df)
}



# Set Up ------------------------------------------------------------------

t = 200
K = 100 
r = 0.8 #set for each shark
N_0 = 100
avs = 0.5 # at vessel survival; higher = better for shark (test quantile range from RF)
prs = 0.5 # post release survival; higher = better for shark (test quantile range from RF)
q = 1 # catchability, set to 1 for ease of fishing pressure, could vary
f = 0.7 #vary for sensitivity
quota = 0.5 # in percentage of catch; vary for sensitivity

test_bau = sim(t, N_0, K, r, avs, prs, q, f, quota, "BAU")
test_rb = sim(t, N_0, K, r, avs, prs, q, f, quota, "RB")
test_cq = sim(t, N_0, K, r, avs, prs, q, f, quota, "CQ")

test_sim = list(test_bau, test_rb, test_cq) %>% 
  reduce(full_join)

ggplot(test_sim, aes(t, pop.array, color = scenario)) +
  geom_line(linewidth = 2) +
  theme_bw()
