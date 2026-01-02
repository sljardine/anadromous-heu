library(here) #directories
library(gurobi) #gurobi solver
library(Matrix) #matrix
library(sf) #spatial data proc
library(tidyverse) #data proc
here::i_am("code/08_huc8_boxplots.R")

# source functions ----
source(here("code", "helper_funs.R"))

# load data ----
load(here("data", "processed", "anadromous_heu_inputs.RData"))

# define preferences ----
n_factors <- 3 #species, temp, forest
alpha <- 0.3
gamma <- (1-alpha)/n_factors #weight on everything else
beta <- 0.25

# check connectivity by huc8 ----
huc8 <- culverts_cmb %>% pull(huc8)
D_ind <- which(D != 0, arr.ind = T)
row_huc8 <- huc8[D_ind[, 1]]
col_huc8 <- huc8[D_ind[, 2]]
sum(row_huc8 == col_huc8)
sum(!row_huc8 == col_huc8)

E_ind <- which(E != 0, arr.ind = T)
row_huc8_E <- huc8[E_ind[, 1]]
col_huc8_E <- huc8[E_ind[, 2]]
sum(row_huc8_E == col_huc8_E)
sum(!row_huc8_E == col_huc8_E)

# preallocate result space ----
rel_perf1 <- list()
rel_perf2 <- list()
D_full <- D
E_full <- E

# loop over HUC8s ----
huc8s <- unique(culverts_cmb$huc8)

for (i in 1 : length(huc8s)) {
  
  points <- culverts_cmb %>% filter(huc8 == huc8s[i]) %>% mutate(id = row_number())
  sel <- culverts_cmb$huc8 == huc8s[i]
  
  #OPTaa
  points_opt_aa <- points %>%
    mutate(
      h_obj = alpha * hmarg_length + gamma * hmarg_nat + gamma * hmarg_low_temp + gamma * n_salmon_7 * hmarg_length
    ) 
  
  #SRpa
  points_sr_pa <- points %>%
    mutate(
      h_obj = alpha * hfull_length + gamma * hfull_nat + gamma * hfull_low_temp + gamma * hfull_species
    )
  
  #SRpp
  points_sr_pp <- points %>% 
    mutate(
      h_obj = alpha * hfull_norm + gamma * natural_percent + gamma * opt_temp_full + gamma * n_salmon_7
    )
  
  # frontiers ----
  ## opt_aa ----
  opt_out <- solve_opt(
    points = points_opt_aa %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    D = D_full[sel, sel],
    E = E_full[sel, sel],
    budget = beta * sum(points$cost)
  )
  
  opt_out <- data.frame(
    habitat_tot = opt_out$habitat_tot, 
    habitat_natural = opt_out$habitat_natural, 
    habitat_low_temp = opt_out$habitat_low_temp, 
    habitat_species = opt_out$habitat_species
  ) 
  
  ## sr_pa ----
  sr_pa_out <- solve_pi(
    points = points_sr_pa %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E_full[sel, sel],
    budget = beta * sum(points$cost)
  )
  
  sr_pa_out <- data.frame(
    habitat_tot = sr_pa_out$habitat_tot, 
    habitat_natural = sr_pa_out$habitat_natural, 
    habitat_low_temp = sr_pa_out$habitat_low_temp, 
    habitat_species = sr_pa_out$habitat_species
  ) %>% mutate(method = "SRpa") 
  
  heu_rel_perf1 <- sr_pa_out %>% 
    mutate(
      huc8 = huc8s[i], 
      hab_tot_rp = habitat_tot/opt_out$habitat_tot,
      hab_nat_rp = habitat_natural/opt_out$habitat_natural,
      hab_cool_rp = habitat_low_temp/opt_out$habitat_low_temp,
      hab_spec_rp = habitat_species/opt_out$habitat_species
    )
  
  
  rel_perf1[[i]] <- heu_rel_perf1
  
  ## sr_pp ----
  sr_pp_out <- solve_pi(
    points = points_sr_pp %>% 
      mutate(
        owner = 1,
        area = 1
      ), 
    E = E_full[sel, sel],
    budget = beta * sum(points$cost)
  )
  
  sr_pp_out <- data.frame(
    habitat_tot = sr_pp_out$habitat_tot, 
    habitat_natural = sr_pp_out$habitat_natural, 
    habitat_low_temp = sr_pp_out$habitat_low_temp, 
    habitat_species = sr_pp_out$habitat_species
  ) %>% mutate(method = "SRpp") 
  
  heu_rel_perf2 <- sr_pp_out %>% 
    mutate(
      huc8 = huc8s[i],
      hab_tot_rp = habitat_tot/opt_out$habitat_tot,
      hab_nat_rp = habitat_natural/opt_out$habitat_natural,
      hab_cool_rp = habitat_low_temp/opt_out$habitat_low_temp,
      hab_spec_rp = habitat_species/opt_out$habitat_species
    )
  
  rel_perf2[[i]] <- heu_rel_perf2
  
}


# extract dataframe ----
rp1 <- bind_rows(rel_perf1, .id = "column_label")
rp2 <- bind_rows(rel_perf2, .id = "column_label")

rp <- rp1 %>% 
  rbind(., rp2) %>% 
  select(
    huc8, hab_tot_rp, hab_nat_rp, hab_cool_rp, hab_spec_rp, method
    )

rp_long <- rp %>% 
  pivot_longer(cols = -c(method, huc8)) %>% 
  mutate(
    habitat = case_when(
      name == "hab_tot_rp" ~ "H",
      name == "hab_nat_rp" ~ "Hn",
      name == "hab_cool_rp" ~ "Hc",
      name == "hab_spec_rp" ~ "Hs"
    ),
    habitat = fct_relevel(habitat, c("H", "Hn", "Hc", "Hs"))) 

# plot ----
p <- ggplot() +
  geom_boxplot(
    data = rp_long, 
    aes(x = habitat, y = value, color = method, fill = method),
    outlier.colour = "#2F4858",
    outlier.fill = "#2F4858",
    outlier.size = 3
  ) +
  ylim(0, 1) +
  scale_fill_manual(values = c("#fed9b7", "#f07167")) +
  scale_color_manual(values = c("#fed9b7", "#f07167")) +
  labs(y = "performance (SR/OPT)") +
  theme_classic(base_size = 18) +
  theme(legend.position = "bottom")

dat <- ggplot_build(p)$data[[1]]

p + geom_segment(
  data = dat, 
  aes(x = xmin, xend = xmax, y = middle, yend = middle), 
  colour = "#2F4858", 
  linewidth = 0.5
)

ggsave(
  filename = here("output", "huc8.pdf"),
  units = "in",
  width = 6,
  height = 5,
)
