library(here) #directories
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/03_2x2_dat.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "anadromous_heu_inputs.RData"))

# define preferences ----
n_factors <- 3 #species, temp, forest

alphas <- seq(0.05, 1, 0.05)
opt_aa_fronts <- list()
sr_pa_fronts <- list()
sr_pp_fronts <- list()
res <- 20

for (i in 1 : length(alphas)) {
  
  alpha <- alphas[i]  
  gamma <- (1-alpha)/n_factors  #weight on everything else
  
  #OPTaa
  points_opt_aa <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hmarg_length + gamma * hmarg_nat + gamma * hmarg_low_temp + gamma * n_salmon_7 * hmarg_length
    )
  
  #SRpa
  points_sr_pa <- culverts_cmb %>%
    mutate(
      h_obj = alpha * hfull_length + gamma * hfull_nat + gamma * hfull_low_temp + gamma * hfull_species
    )

  #SRpp
  points_sr_pp <- culverts_cmb %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7
    )
  
  # frontiers ----
  ## OPTaa ----
  opt_aa_front <- get_pareto_front(
    points = points_opt_aa %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    D = D,
    E = E,
    resolution = res
  )
  
  opt_aa_front <- opt_aa_front %>% mutate(method = "OPT_b") 
  opt_aa_fronts[[i]] <- opt_aa_front

  ## SRpa ----
  sr_pa_front <- get_pi_front(
    points = points_sr_pa %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  sr_pa_front <- sr_pa_front %>% mutate(method = "SR_b") 
  
  sr_pa_fronts[[i]] <- sr_pa_front
  
  ## SRpp ----
  sr_pp_front <- get_pi_front(
    points = points_sr_pp %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E,
    resolution = res
  )
  
  sr_pp_front <- sr_pp_front %>% mutate(method = "SR_p") 
  
  sr_pp_fronts[[i]] <- sr_pp_front
  
}

# budget grid ----
budget_grd <- seq(
  from = sum(points_opt_aa$cost) / res, 
  to = sum(points_opt_aa$cost), 
  by = sum(points_opt_aa$cost) / res
) / sum(points_opt_aa$cost) 

budget_grd <- as.data.frame(budget_grd)

save(
  alphas, opt_aa_fronts, sr_pa_fronts, sr_pp_fronts, budget_grd,
  file = here("data", "processed", "2x2_results.RData")
)
