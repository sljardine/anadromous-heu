library(here) #directories
library(grid)
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(scales)
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/07_penalty.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "anadromous_heu_inputs.RData"))

# create penalty variables ----
culverts_cmb <- culverts_cmb %>% 
  mutate(
    up_count_norm = up_count/max(up_count),
    dn_count_norm = dn_count/max(dn_count)
    )

# define preferences ----
n_factors <- 3 #species, temp, forest

alpha <- 0.3
gamma <- (1-alpha)/n_factors #weight on everything else

gammas <- seq(0.5, 5, 0.5)

sr_fronts <- list()
sr_fronts_up <- list()
sr_fronts_dn <- list()
sr_fronts_updn <- list()

res <- 20 #defines betas

# no penalty benchmark ----
points_pi <- culverts_cmb %>% 
  mutate(
    h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7
  )

sr_fronts <- get_pi_front(
  points = points_pi %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  E = E,
  resolution = res
)

sr_fronts <- sr_fronts %>% mutate(penalty = "none") 

# penalty frontiers ----
for (i in 1 : length(gammas)) {
  
  gamma <- gammas[i]
  
  points_pi <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7
    )
  
  points_pi_up <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7 - gamma * up_count_norm
    )
  
  points_pi_dn <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7 - gamma * dn_count_norm
    )
  
  points_pi_updn <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7 - gamma/2 * up_count_norm - gamma/2 * dn_count_norm
    )
  
  # frontiers ----
  ## up penalty ----
  sr_front_up <- get_pi_front(
    points = points_pi_up %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  sr_front_up <- sr_front_up %>% mutate(penalty = "# up") 
  
  sr_fronts_up[[i]] <- sr_front_up
  
  ## down penalty ----
  sr_front_dn <- get_pi_front(
    points = points_pi_dn %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  sr_front_dn <- sr_front_dn %>% mutate(penalty = "# down") 
  
  sr_fronts_dn[[i]] <- sr_front_dn
  
  ## up and down penalty ----
  sr_front_updn <- get_pi_front(
    points = points_pi_updn %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  sr_front_updn <- sr_front_updn %>% mutate(penalty = "# up & down") 
  
  sr_fronts_updn[[i]] <- sr_front_updn
  
}

# budget grid ----
budget_grd <- seq(
  from = sum(points_pi$cost) / res, 
  to = sum(points_pi$cost), 
  by = sum(points_pi$cost) / res
) / sum(points_pi$cost) 

budget_grd <- as.data.frame(budget_grd)

save(
  sr_fronts, sr_fronts_up, sr_fronts_dn, sr_fronts_updn, 
  sr_fronts, sr_fronts_up, sr_fronts_dn, sr_fronts_updn,
  budget_grd,
  file = here("data", "processed", "penalty_results.RData")
)

# figures ----
load(here("data", "processed", "penalty_results.RData"))
penalty_lev <- 5 #choose from 1 and 5 to replicate manuscript results

ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1, linetype = "twodash") +
  geom_line(
    data = sr_fronts %>% mutate(penalty = fct_relevel(penalty, c("none", "# up", "# down", "# up & down"))), 
    aes(x = expenditures/max(expenditures), y = habitat_tot, color = penalty, linetype = penalty),
    linewidth = 2
  ) + 
  geom_line(
    data = sr_fronts_dn[[penalty_lev]], 
    aes(x = expenditures/max(sr_fronts$expenditures), y = habitat_tot, color = penalty, linetype = penalty),
    linewidth = 2
  ) +
  geom_line(
    data = sr_fronts_up[[penalty_lev]], 
    aes(x = expenditures/max(sr_fronts$expenditures), y = habitat_tot, color = penalty, linetype = penalty),
    linewidth = 2
  ) + 
  geom_line(
    data = sr_fronts_updn[[penalty_lev]], 
    aes(x = expenditures/max(sr_fronts$expenditures), y = habitat_tot, color = penalty, linetype = penalty),
    linewidth = 2
  ) + 
  scale_color_manual(values = c("#562824", "#d8655c", "#f38d85", "#ebb2ad")) +
  scale_y_continuous(label = comma, n.breaks = 5, limits = c(0, 18000)) +
  labs(
    tag = "a)",
    y = "total habitat (km)", 
    x = "expenditures (normalized)") +
  theme_classic(base_size = 18) 

ggsave(
  filename = here("output", paste0("penalty_", gammas[penalty_lev], ".pdf")),
  units = "in",
  width = 6,
  height = 3,
)
