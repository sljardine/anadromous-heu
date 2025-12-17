library(here) 
library(scales) #axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
library(htmltools) #leaflet title
library(ggbrace) 


here::i_am("code/03_quant_fig.R")

# load data ----
load(here("data", "processed", "quant_coord_dat.RData"))

# plot gains from connectivity proxy ----
ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg") %>% filter(method == "SR") %>% mutate(approach = "SRii"),
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_a %>% filter(obj == "hfull") %>% mutate(approach = "SRpi"),
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg", method == "OPT") %>% mutate(approach = "OPTai"),
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot,  color = approach),
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


# presentation figs ----
## parameters ----
beta <- 0.25
hab_bet_opt <- fronts_a %>% filter(round(exp_norm, 2) == beta, method == "OPT") %>% select(habitat_tot) 
hab_bet_sr_ignore <- fronts_a %>% filter(round(exp_norm, 2) == beta, method == "SR", obj == "hmarg") %>% select(habitat_tot) 
hab_bet_sr_proxy <- fronts_a %>% filter(round(exp_norm, 2) == beta, method == "SR", obj == "hfull") %>% select(habitat_tot) 
hab_bet_sr_ignore/hab_bet_opt * 100
hab_bet_sr_proxy/hab_bet_opt * 100

# figures ----
ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg") %>% filter(method == "SR") %>% mutate(approach = "SRii"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) + 
  geom_line(
    data = fronts_a %>% filter(obj == "hfull") %>% mutate(approach = "SRpi"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg", method == "OPT") %>% mutate(approach = "OPTai"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot,  color = approach),
    linewidth = 2
  ) + 
  scale_color_manual(values = c("#0081a7", "#2F4858", alpha("#f07167", 0.25))) +
  scale_y_continuous(label = comma, n.breaks = 5, limits = c(0, 18000)) +
  geom_segment(
    aes(x = 0.25 , y = 11565.1, xend = 0.25, yend = 15551.12), 
    size = 1.5, color = "red", arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = 0.45, y = 15000, label = "+34%"), size = 5, color = "red") +
  labs(
    y = "total habitat (km)", 
    x = "expenditures (normalized)"
  ) +
  theme_classic(base_size = 18) 

ggsave(
  filename = here("output", "quant_coord_SRb.pdf"),
  units = "in",
  width = 6,
  height = 3,
)

ggplot() +
  geom_vline(aes(xintercept = 0.25), linewidth = 1) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg") %>% filter(method == "SR") %>% mutate(approach = "SRii"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) + 
  geom_line(
    data = fronts_a %>% filter(obj == "hfull") %>% mutate(approach = "SRpi"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot, color = approach),
    linewidth = 2
  ) +
  geom_line(
    data = fronts_a %>% filter(obj == "hmarg", method == "OPT") %>% mutate(approach = "OPTai"), 
    aes(x = expenditures/max(front_a$expenditures), y = habitat_tot,  color = approach),
    linewidth = 2
  ) + 
  scale_color_manual(values = c("#0081a7", alpha("#2F4858", 0.25), "#f07167")) +
  scale_y_continuous(label = comma, n.breaks = 5, limits = c(0, 18000)) +
  geom_segment(
    aes(x = 0.25 , y = 14745.66, xend = 0.25, yend = 15551.12), 
    size = 1.5, color = "red", arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(aes(x = 0.45, y = 15000, label = "+5%"), size = 5, color = "red") +
  labs(
    y = "total habitat (km)", 
    x = "expenditures (normalized)"
  ) +
  theme_classic(base_size = 18) 

ggsave(
  filename = here("output", "quant_coord_SRc.pdf"),
  units = "in",
  width = 6,
  height = 3,
)
