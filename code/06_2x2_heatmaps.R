library(colorspace)
library(here) #directories
library(grid) #graphics
library(scales) #axis labels
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/06_2x2_heatmaps.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "2x2_results.RData"))

# heatmap dat ----
## total habitat ----
htd_per <- get_hm_dat(
  opt_aa_fronts, sr_pp_fronts, attribute = "habitat_tot", obj_dfn = "multi"
)
htd_per <- htd_per %>%
  mutate(ex = ifelse(Y == 0.3 & X > 0.24 & X < 0.26, 1, 0)) %>%
  dplyr::arrange(ex)

## species of interest habitat ----
hsoid_per <- get_hm_dat(
  opt_aa_fronts, sr_pp_fronts, attribute = "habitat_species", obj_dfn = "multi"
)
hsoid_per <- hsoid_per %>%
  mutate(ex = ifelse(Y == 0.3 & X > 0.24 & X < 0.26, 1, 0)) %>%
  dplyr::arrange(ex)

## cool habitat ----
hltd_per <- get_hm_dat(
  opt_aa_fronts, sr_pp_fronts, attribute = "habitat_low_temp", obj_dfn = "multi"
)
hltd_per <- hltd_per %>%
  mutate(ex = ifelse(Y == 0.3 & X > 0.24 & X < 0.26, 1, 0)) %>%
  dplyr::arrange(ex)

## natural habitat ----
hnd_per <- get_hm_dat(
  opt_aa_fronts, sr_pp_fronts, attribute = "habitat_natural", obj_dfn = "multi"
)
hnd_per <- hnd_per %>%
  mutate(ex = ifelse(Y == 0.3 & X > 0.24 & X < 0.26, 1, 0)) %>%
  dplyr::arrange(ex)

# heatmaps ----
##diff total habitat ----
cap <- 150
htd_per <- htd_per %>%
  mutate(z_cap = round(ifelse(Z > cap, cap, Z), 4))
ggplot(
  htd_per, aes(X, Y, fill = z_cap, color = as.factor(ex))
  ) + 
  geom_tile(linewidth = 1) +
  scale_fill_continuous_divergingx(
    palette = 'RdBu',
    mid = 0,
    limits = c(-1, cap)
  ) +
  scale_colour_manual(values = c("transparent", "black")) +
  labs(
    x = expression(beta), 
    y = expression(alpha), 
    fill = "% \u0394 H", 
    tag = "a)"
  ) +
  guides(color = "none") +
  theme_classic(base_size = 18) +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold")
  )

ggsave(
  filename = here(
    "output", "SRpp_v_OPTaa_heat_H.pdf"
  ),
  units = "in",
  width = 5,
  height = 3,
  device = cairo_pdf, 
  family = "Arial Unicode MS"
)

##diff soi ----
hsoid_per <- hsoid_per %>% 
  mutate(z_cap = round(ifelse(Z > cap, cap, Z), 4))
ggplot(hsoid_per, aes(X, Y, fill = z_cap, color = as.factor(ex))) + 
  geom_tile(linewidth = 1) +
  scale_fill_continuous_divergingx(
    palette = "RdBu",
    mid = 0,
    limits = c(-1, cap)
  ) +
  scale_colour_manual(values = c("transparent", "black")) +
  labs(
    x = expression(beta), 
    y = expression(alpha), 
    fill = "% \u0394 Hs", 
    tag = "d)"
  ) +
  guides(color = "none") +
  theme_classic(base_size = 18) +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold")
  )

ggsave(
  filename = here(
    "output", "SRpp_v_OPTaa_heat_Hs.pdf"
  ),
  units = "in",
  width = 5,
  height = 3,
  device = cairo_pdf, 
  family = "Arial Unicode MS"
)

##diff cool water habitat ----
hltd_per <- hltd_per %>% 
  mutate(z_cap = round(ifelse(Z > cap, cap, Z), 4))
ggplot(hltd_per, aes(X, Y, fill = z_cap, color = as.factor(ex))) + 
  geom_tile(linewidth = 1) +
  scale_fill_continuous_divergingx(
    palette = "RdBu",
    mid = 0,
    limits = c(-1, cap)
  ) +
  scale_colour_manual(values = c("transparent", "black")) +
  labs(
    x = expression(beta), 
    y = expression(alpha), 
    fill = "% \u0394 Hc", 
    tag = "c)"
  ) +
  guides(color = "none") +
  theme_classic(base_size = 18) +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold")
  )

ggsave(
  filename = here(
    "output", "SRpp_v_OPTaa_heat_Hc.pdf"
  ),
  units = "in",
  width = 5,
  height = 3,
  device = cairo_pdf, 
  family = "Arial Unicode MS"
)

##diff total natural habitat ----
hnd_per <- hnd_per %>% 
  mutate(z_cap = round(ifelse(Z > cap, cap, Z), 4))
ggplot(hnd_per, aes(X, Y, fill = z_cap, color = as.factor(ex))) + 
  geom_tile(linewidth = 1) +
  scale_fill_continuous_divergingx(
    palette = 'RdBu',
    mid = 0,
    limits = c(-1, cap)
  ) +
  scale_colour_manual(values = c("transparent", "black")) +
  labs(
    x = expression(beta), 
    y = expression(alpha), 
    fill = "% \u0394 Hn", 
    tag = "b)"
  ) +
  guides(color = "none") +
  theme_classic(base_size = 18) +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold")
  )

ggsave(
  filename = here(
    "output",  "SRpp_v_OPTaa_heat_Hn.pdf"
  ),
  units = "in",
  width = 5,
  height = 3,
  device = cairo_pdf, 
  family = "Arial Unicode MS"
)

max(htd_per$Z)
max(hsoid_per$Z)
max(hltd_per$Z)
max(hnd_per$Z)

