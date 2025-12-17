# case area frontiers
library(colorspace)
library(here) #directories
library(ggradar)
library(scales) #axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/05_2x2_radar.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "2x2_results.RData"))
load(here("data", "processed", "quant_coord_dat.RData"))

# radar plot data (select alpha = 0.3, beta = 0.25) ----
alphas[6]
budget_grd[5, 1]
opt_fronts1_sel <- opt_fronts1[[6]][5, ]
opt_fronts2_sel <- opt_fronts2[[6]][5, ]
heu_fronts1_sel <- heu_fronts1[[6]][5, ]
heu_fronts2_sel <- heu_fronts2[[6]][5, ]

opt_fronts1_sel <- opt_fronts1_sel %>% mutate(method = "OPTaa")
opt_fronts2_sel <- opt_fronts2_sel %>% mutate(method = "OPTap")

heu_fronts1_sel <- heu_fronts1_sel %>% mutate(method = "SRpa") %>% select(-stranded)
heu_fronts2_sel <- heu_fronts2_sel %>% mutate(method = "SRpp") %>% select(-stranded)

opt_fronts1_sel <- opt_fronts1_sel %>% select(-status)
opt_fronts2_sel <- opt_fronts2_sel %>% select(-status)

radar_dat_a <- opt_fronts1_sel %>% 
  rbind(., heu_fronts1_sel) %>% 
  rbind(., heu_fronts2_sel) %>% 
  select(
    method, habitat_tot, habitat_natural, habitat_species, habitat_low_temp, method
  ) %>% 
  rename(
    total = habitat_tot,
    natural = habitat_natural,
    species = habitat_species,
    cool = habitat_low_temp
  )

fronts_a_SRp <- fronts_a %>% 
  filter(method == "SR", obj == "hfull") %>% 
  select(-status, -exp_norm, -scenario, - obj, -hab_norm, -stranded)

fronts_a_betas <- fronts_a_SRp$budget/max(fronts_a_SRp$budget)
which.min(abs(fronts_a_betas - budget_grd[5, ]))

radar_dat_b <- fronts_a %>% 
  select(-stranded) %>% 
  filter(method == "SR", obj == "hfull") %>% 
  select(-status, -exp_norm, -scenario, - obj, -hab_norm) %>% 
  slice(25) %>% 
  rbind(., heu_fronts2_sel) %>% 
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
##radar a ----

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

#bar plot ----
bar_dat_a <- radar_dat_a %>% 
  pivot_longer(., -method)

ggplot(
  bar_dat_a %>% 
    filter(name %in% c("cool", "natural", "species", "total")) %>% 
    mutate(name = fct_relevel(name, c("total", "natural", "species", "cool"))), 
  aes(fill = method, y = value/15536, x = name)
) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#0081a7", "#a34474", "#CA5B6E")) +
  labs(y = "habitat gains", x = "") +
  theme_classic()

