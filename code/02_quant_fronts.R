# case area frontiers with coordination scenarios
library(here) #directories
library(leaflet) #interactive maps
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(sf) #spatial data proc
library(tidyverse) #data proc
library(htmltools) #leaflet title
here::i_am("code/02_quant_fronts.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "heuristic_inputs.RData"))

# define preferences ----
points_opt <- culverts_cmb %>% mutate(h_obj = hmarg_length)
points_sr1 <- culverts_cmb %>% mutate(h_obj = hmarg_length)
points_sr2 <- culverts_cmb %>% mutate(h_obj = hfull_length)

res <- 100 #fineness of budget grid

# frontiers ----
## opt: sole owner ----
front_a <- get_pareto_front(
  points = points_opt %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  D = D,
  E = E,
  resolution = res
)

front_a <- front_a %>% mutate(scenario = "a) full coordination", method = "OPT") 

## heu: sole owner ----
heu1_front_a <- get_pi_front(
  points = points_sr1 %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  E = E,
  resolution = res
)

heu1_front_a <- heu1_front_a %>% mutate(scenario = "a) full coordination", method = "SR") 

heu2_front_a <- get_pi_front(
  points = points_sr2 %>% 
    mutate(
      owner = 1,
      area = 1
      ), 
  E = E,
  resolution = res
)

heu2_front_a <- heu2_front_a %>% mutate(scenario = "a) full coordination", method = "SR")  

# plot frontiers ----
fronts_a <- front_a %>% 
  bind_rows(., heu1_front_a) %>% 
  mutate(obj = "hmarg") %>% 
  bind_rows(., heu2_front_a) %>% 
  mutate(
    exp_norm = expenditures/max(expenditures),
    hab_norm = habitat_tot/max(habitat_tot),
    obj = ifelse(is.na(obj), "hfull", obj)
    )

fronts_a %>% filter(round(exp_norm, 2) == 0.25, method == "OPT")

save(
  front_a, fronts_a,
  file = here("data", "processed", "quant_coord_dat.RData")
)
