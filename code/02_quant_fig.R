library(here) 
library(scales) #axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
library(htmltools) #leaflet title
library(ggbrace) 

here::i_am("code/02_quant_fig.R")

# load data ----
load(here("data", "processed", "quantity_frontier_dat.RData"))

# plot gains from connectivity proxy ----
ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1) +
  geom_line(
    data = fronts %>% filter(obj == "hmarg") %>% filter(method == "SR") %>% mutate(approach = "SRii"),
    aes(x = expenditures/max(front$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts %>% filter(obj == "hfull") %>% mutate(approach = "SRpi"),
    aes(x = expenditures/max(front$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts %>% filter(obj == "hmarg", method == "OPT") %>% mutate(approach = "OPTai"),
    aes(x = expenditures/max(front$expenditures), y = habitat_tot,  color = approach),
    linewidth = 2
  ) +
  scale_color_manual(
    values = c("#0081a7", "#f07167",  "#FED9B7"), 
    breaks = c("OPTai", "SRpi", "SRii")
    ) +
  scale_y_continuous(label = comma, n.breaks = 5, limits = c(0, 20000)) +
  labs(
    y = "total habitat (km)",
    x = "expenditures (normalized)") +
  theme_classic(base_size = 18)

ggsave(
  filename = here("output", "quant_fronts.pdf"),
  units = "in",
  width = 6,
  height = 3,
)

