# case area frontiers
library(colorspace)
library(here) #directories
library(ggradar) #radar plots
library(scales) #axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/04_2x2_radar.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "2x2_results.RData"))
load(here("data", "processed", "quantity_frontier_dat.RData"))

# radar plot data (select alpha = 0.3, beta = 0.25) ----
alphas[6]
budget_grd[5, 1]
opt_aa_fronts_sel <- opt_aa_fronts[[6]][5, ]
sr_pa_fronts_sel <- sr_pa_fronts[[6]][5, ]
sr_pp_fronts_sel <- sr_pp_fronts[[6]][5, ]

opt_aa_fronts_sel <- opt_aa_fronts_sel %>% mutate(method = "OPTaa") %>% select(-status)
sr_pa_fronts_sel <- sr_pa_fronts_sel %>% mutate(method = "SRpa") %>% select(-stranded)
sr_pp_fronts_sel <- sr_pp_fronts_sel %>% mutate(method = "SRpp") %>% select(-stranded)

radar_dat_a <- opt_aa_fronts_sel %>% 
  rbind(., sr_pa_fronts_sel) %>% 
  rbind(., sr_pp_fronts_sel) %>% 
  select(
    method, habitat_tot, habitat_natural, habitat_species, habitat_low_temp, method
  ) %>% 
  rename(
    total = habitat_tot,
    natural = habitat_natural,
    species = habitat_species,
    cool = habitat_low_temp
  )

fronts_SRpi <- fronts %>% 
  filter(method == "SR", obj == "hfull") %>% 
  select(-status, -exp_norm, - obj, -hab_norm, -stranded)

fronts_betas <- fronts_SRpi$budget/max(fronts_SRpi$budget)
which.min(abs(fronts_betas - budget_grd[5, ]))

radar_dat_b <- fronts %>% 
  filter(method == "SR", obj == "hfull") %>% 
  select(-stranded, -status, -exp_norm, - obj, -hab_norm) %>% 
  slice(25) %>% 
  rbind(., sr_pp_fronts_sel) %>% 
  select(
    method, habitat_tot, habitat_natural, habitat_species, habitat_low_temp
  ) %>% 
  rename(
    total = habitat_tot,
    natural = habitat_natural,
    species = habitat_species,
    cool = habitat_low_temp,
  ) %>% 
  mutate(method = ifelse(method == "SR", "SRpi", "SRpp"))

# summarize radar data ----
radar_dat_a %>%
  pivot_longer(., -method) %>%
  pivot_wider(., names_from = method) %>%
  mutate(
    SRpp_OPTaa = round(SRpp/OPTaa, 2),
    SRpa_OPTaa = round(SRpa/OPTaa, 2),
  ) %>% 
  select(
    name, SRpp_OPTaa, SRpa_OPTaa
  )

# radar plots ----
#radar a ----

ggradar(
  radar_dat_a,
  background.circle.colour = "white",
  axis.line.colour = "#2F4858",
  gridline.min.colour = "#2F4858",
  gridline.mid.colour = "#2F4858",
  gridline.max.colour = "#2F4858",
  group.colours = c("#0081a7", "#a34474", "#CA5B6E"),
  legend.title = "",
  group.point.size = 0,
  legend.position = "right",
  plot.title = "a)",
  group.line.width = 1.5,
  plot.extent.x.sf = 1.5,
  base.size = 18
)

ggsave(
  filename = here("output", "radar_a.pdf"),
  units = "in",
  width = 8,
  height = 4,
)

##radar b ----

ggradar(
  radar_dat_b,
  background.circle.colour = "white",
  axis.line.colour = "#2F4858",
  gridline.min.colour = "#2F4858",
  gridline.mid.colour = "#2F4858",
  gridline.max.colour = "#2F4858",
  group.colours = c("#f07167", "#CA5B6E"),
  legend.title = "",
  group.point.size = 0,
  legend.position = "right",
  plot.title = "b)",
  group.line.width = 2,
  plot.extent.x.sf = 1.5,
  base.size = 18
)

ggsave(
  filename = here("output", "radar_b.pdf"),
  units = "in",
  width = 8,
  height = 4,
)
