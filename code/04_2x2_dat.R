library(here) #directories
library(grid)
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/04_2x2_dat.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "heuristic_inputs.RData"))

# define preferences ----
n_factors <- 3 #species, temp, forest

alphas <- seq(0.05, 1, 0.05)
opt_fronts1 <- list()
opt_fronts2 <- list()
heu_fronts1 <- list()
heu_fronts2 <- list()
res <- 20

for (i in 1 : length(alphas)) {
  
  alpha <- alphas[i]  
  gamma <- (1-alpha)/n_factors #weight on everything else
  
  #opt_ab
  points_opt1 <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hmarg_length + gamma * hmarg_nat + gamma * hmarg_low_temp + gamma * n_salmon_7 * hmarg_length
    )
  
  #opt_ap
  points_opt2 <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hmarg_norm + gamma * natural_percent + gamma * opt_temp_marg + gamma * n_salmon_7
    )
  
  #heu_ab
  points_pi1 <- culverts_cmb %>%
    mutate(
      h_obj = alpha * hfull_length + gamma * hfull_nat + gamma * hfull_low_temp + gamma * hfull_species
    )

  #heu_ap
  points_pi2 <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7
    )
  
  # frontiers ----
  ## opt_ab ----
  opt_front1 <- get_pareto_front(
    points = points_opt1 %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    D = D,
    E = E,
    resolution = res
  )
  
  opt_front1 <- opt_front1 %>% mutate(method = "OPT_b") 
  opt_fronts1[[i]] <- opt_front1
  
  ## opt_ap ----
  opt_front2 <- get_pareto_front(
    points = points_opt2 %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    D = D,
    E = E,
    resolution = res
  )
  
  opt_front2 <- opt_front2 %>% mutate(method = "OPT_p") 
  opt_fronts2[[i]] <- opt_front2
  
  ## sr_pb ----
  heu_front1 <- get_pi_front(
    points = points_pi1 %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  heu_front1 <- heu_front1 %>% mutate(method = "SR_b") 
  
  heu_fronts1[[i]] <- heu_front1
  
  ## sr_pp ----
  heu_front2 <- get_pi_front(
    points = points_pi2 %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  heu_front2 <- heu_front2 %>% mutate(method = "SR_p") 
  
  heu_fronts2[[i]] <- heu_front2
  
}

# budget grid ----
budget_grd <- seq(
  from = sum(points_opt1$cost) / res, 
  to = sum(points_opt1$cost), 
  by = sum(points_opt1$cost) / res
) / sum(points_opt1$cost) 

budget_grd <- as.data.frame(budget_grd)

save(
  alphas, opt_fronts1, opt_fronts2, heu_fronts1, heu_fronts2, budget_grd,
  file = here("data", "processed", "2x2_results.RData")
)
