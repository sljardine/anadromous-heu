library(here) #directories
library(sf)
library(tidyverse)

# load data ----
load(here("data", "processed", "2x2_results.RData"))
load(here("data", "processed", "quant_coord_dat.RData"))

# line plot data (select alpha = 0.3) ----
alphas[6]
heu_fronts2_alph3 <- heu_fronts2[[6]]
heu_fronts1_alph3 <- heu_fronts1[[6]]

# line plot ----
ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1, linetype = "twodash") +
  geom_line(
    data = heu_fronts2_alph3 %>% mutate(scenario = "SRpp"), 
    aes(x = expenditures/max(front_a$expenditures), y = stranded),
    color = "#2F4858",
    linewidth = 2
  ) +
  labs(
    y = "stranded investments", 
    x = "expenditures (normalized)") +
  theme_classic(base_size = 18) 

ggsave(
  filename = here("output", "stranded.pdf"),
  units = "in",
  width = 5,
  height = 3,
)
