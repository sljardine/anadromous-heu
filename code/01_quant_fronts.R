library(here) #directories
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/01_quant_fronts.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "anadromous_heu_inputs.RData"))

# define preferences ----
points_opt_ai <- culverts_cmb %>% mutate(h_obj = hmarg_length)
points_sr_ii <- culverts_cmb %>% mutate(h_obj = hmarg_length)
points_sr_pi <- culverts_cmb %>% mutate(h_obj = hfull_length)

res <- 100 #fineness of budget grid

# frontiers ----
## optimization frontier (OPTai) ----
front <- get_pareto_front(
  points = points_opt_ai %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  D = D,
  E = E,
  resolution = res
)

front <- front %>% mutate(method = "OPT") 

## heuristic frontier (SRii) ----
heu1_front_a <- get_pi_front(
  points = points_sr_ii %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  E = E,
  resolution = res
)

heu1_front_a <- heu1_front_a %>% mutate(method = "SR") 

## heuristic frontier (SRip) ----
heu2_front_a <- get_pi_front(
  points = points_sr_pi %>% 
    mutate(
      owner = 1,
      area = 1
      ), 
  E = E,
  resolution = res
)

heu2_front_a <- heu2_front_a %>% mutate(method = "SR")  

# combine and save frontier data ----
fronts <- front %>% 
  bind_rows(., heu1_front_a) %>% 
  mutate(obj = "hmarg") %>% 
  bind_rows(., heu2_front_a) %>% 
  mutate(
    exp_norm = expenditures/max(expenditures),
    hab_norm = habitat_tot/max(habitat_tot),
    obj = ifelse(is.na(obj), "hfull", obj)
    )

save(
  front, fronts,
  file = here("data", "processed", "quantity_frontier_dat.RData")
)
