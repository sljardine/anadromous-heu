# case area frontiers with coordination scenarios
library(here) #directories
library(leaflet) #interactive maps
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(scales) #format axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
library(htmltools) #leaflet title
here::i_am("code/09_coord_robustness.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "heuristic_inputs.RData"))

# holdout landowners ----
holdout_owner <- 4 # private landowner
sum_cost <- sum(culverts_cmb$cost)
points <- culverts_cmb %>% dplyr::mutate(cost = ifelse(owner_type_code == holdout_owner, sum_cost, cost))

# define preferences ----
points_opt <- points %>% mutate(h_obj = hmarg_length)
points_sr <- points %>% mutate(h_obj = hfull_length)

res <- 50 #fineness of budget grid

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
  resolution = res,
  holdout_owner = 4
)

front_a <- front_a %>% mutate(coordination = "full", method = "OPT") 

## heu: sole owner ----
heu_front_a <- get_pi_front(
  points = points_sr %>% 
    mutate(
      owner = 1,
      area = 1
    ), 
  E = E,
  resolution = res,
  holdout_owner = 4
)

heu_front_a <- heu_front_a %>% mutate(coordination = "full", method = "SR") 

## opt: het areas ----
front_b <- get_pareto_front(
  points = points_opt %>% 
    mutate(
      owner = 1,
      area = huc8
    ), 
  D = D,
  E = E,
  resolution = res,
  holdout_owner = 4
)

front_b <- front_b %>% mutate(coordination = "area", method = "OPT") %>% 
  select(-budget)

## pi: het areas ----
heu_front_b <- get_pi_front(
  points = points_sr %>% 
    mutate(
      owner = 1,
      area = huc8
    ), 
  E = E,
  resolution = res,
  holdout_owner = 4
)

heu_front_b <- heu_front_b %>% mutate(coordination = "area", method = "SR") %>% 
  select(-budget)



# plot frontiers ----
fronts_a <- front_a %>% 
  bind_rows(., heu_front_a) %>% 
  mutate(
    exp_norm = expenditures/max(expenditures),
    hab_norm = habitat_tot/max(habitat_tot)
    )

fronts_b <- front_b %>% 
  bind_rows(., heu_front_b) %>% 
  mutate(
    exp_norm = expenditures/max(expenditures),
    hab_norm = habitat_tot/max(habitat_tot)
  )  

    

ggplot() +
  geom_vline(aes(xintercept = 0.3), linewidth = 1, linetype = "twodash") +
  geom_line(
    data = fronts_a %>% filter(method == "OPT") %>% mutate(scenario = "OPTb; full"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = scenario),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_b %>% filter(method == "OPT") %>% mutate(scenario = "OPTb; area"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = scenario),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_a %>% filter(method == "SR") %>% mutate(scenario = "SRb; full"),
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = scenario),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_b %>% filter(method == "SR") %>% mutate(scenario = "SRb; area"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = scenario),
    linewidth = 2
  ) +
  scale_color_manual(values = c("#003342", "#4ca6c1", "#562824", "#d8655c")) +
  scale_y_continuous(label = comma, n.breaks = 5, limits = c(0, 18000)) +
  labs(y = "total habitat (km)", x = "expenditures (normalized)", color = "SR\ncoordination") +
  theme_classic(base_size = 18) 




# # save everything ----
# save(
#   front_a, front_b, front_c, front_d, fronts_a, fronts_b, fronts_c, fronts_d,
#   file = here("data", "processed", "quant_coord_dat.RData")
# )
