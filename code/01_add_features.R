library(here) #directories
library(nhdplusTools) #loading nhd
library(sf) #geocomp
library(tidyverse) #data proc

# load data ----
load(here("data", "processed", "upstream_inputs_gm.RData"))
clean_lines <- st_read(here("data", "processed", "clean_lines_gm.gpkg"))
lines_simp <- lines_simp_gm %>% 
  left_join(., clean_lines %>% st_drop_geometry(), by = "COMID") %>% 
  select(StreamOrde, StreamLeve, TerminalPa, REACHCODE, COMID, geom)

# rename objects ----
culverts_cmb <- culverts_cmb_gm
D <- D_gm
di <- colSums(D)
downstream_line_ids <- downstream_line_ids_gm
E <- E_gm
lines_ds <- lines_ds_gm
marginal_line_ids <- marginal_line_ids_gm
rm(culverts_cmb_gm, D_gm, downstream_line_ids_gm, E_gm, 
    lines_ds_gm, lines_simp_gm, marginal_line_ids_gm, 
    welcome_map_points
  )

# add hucs to lines and lines_simp ----
lines_simp <- lines_simp %>% 
  mutate(
    huc4 = str_trunc(REACHCODE, 4, ellipsis = ""),
    huc6 = str_trunc(REACHCODE, 6, ellipsis = ""),
    huc8 = str_trunc(REACHCODE, 8, ellipsis = ""),
    huc10 = str_trunc(REACHCODE, 10, ellipsis = ""),
    huc12 = str_trunc(REACHCODE, 12, ellipsis = "")
    )

# add hucs and terminal paths ----
huc_term <- lines_simp %>% 
  select(
    COMID, huc4, huc6, huc8, huc10, huc12, TerminalPa, 
    StreamOrde, StreamLeve) %>% 
  st_drop_geometry()

# drop some unused variables ----
culverts_cmb <- culverts_cmb %>% 
  select(
    potential_species, hmarg_length, hfull_length, hmarg_length_natural_percent,
    hmarg_length_hist_temp, comid, hfull_length_hist_temp, 
    hfull_length_natural_percent, cost, site_id, owner_type_code, dn_count, up_count
    ) 

# add common variables ----
culverts_cmb <- culverts_cmb %>% 
  left_join(., huc_term, by = join_by(comid == COMID)) %>% 
  mutate(
    id = row_number(),
    chinook = grepl("chinook", potential_species, ignore.case = TRUE),
    coho = grepl("coho", potential_species, ignore.case = TRUE),
    steelhead = grepl("steelhead", potential_species, ignore.case = TRUE),
    chum = grepl("chum", potential_species, ignore.case = TRUE),
    sockeye = grepl("sockeye", potential_species, ignore.case = TRUE),
    bull_trout = grepl("bull trout", potential_species, ignore.case = TRUE),
    cutthroat = grepl("cutthroat", potential_species, ignore.case = TRUE),
    n_salmon_7 = (chinook + coho + steelhead + chum + sockeye + bull_trout + cutthroat)/7, #for PI
    temp_marg = ifelse(
      !is.na(hmarg_length_hist_temp), 
      hmarg_length_hist_temp, 
      mean(hmarg_length_hist_temp, na.rm = TRUE)
    ), 
    low_temp = temp_marg <= 18 & temp_marg >= 8.5, #for outcome eval from Mayer et al. for coho
    hmarg_length_natural_percent = ifelse(!is.na(hmarg_length_natural_percent), hmarg_length_natural_percent/100, 0),
    hmarg_nat = hmarg_length * hmarg_length_natural_percent,
    hmarg_low_temp = hmarg_length * low_temp,
    hfull_norm = hfull_length / max(hfull_length), #for PI
    hmarg_norm = hmarg_length / max(hmarg_length), #for PI
    high_nat = hmarg_length_natural_percent > quantile(hmarg_length_natural_percent, 0.75),
    cool_nat = hmarg_length_natural_percent * low_temp,
    cool_species = low_temp * n_salmon_7,
    nat_species = hmarg_length_natural_percent * n_salmon_7,
    temp_full = ifelse(
      !is.na(hfull_length_hist_temp), 
      hfull_length_hist_temp, 
      mean(hfull_length_hist_temp, na.rm = TRUE)
    ), #for PI
    opt_temp_full = exp(-0.1 * (temp_full - 12.35) ^ 2), #for PI
    temp_marg = ifelse(
      !is.na(hmarg_length_hist_temp), 
      hmarg_length_hist_temp, 
      mean(hmarg_length_hist_temp, na.rm = TRUE)
    ), #for PI
    opt_temp_marg = exp(-0.1 * (temp_marg - 12.35) ^ 2), #for PI
    natural_percent = ifelse(
      !is.na(hfull_length_natural_percent), hfull_length_natural_percent/100, 0
    ) #for PI
  )

# calculate hfull values from hmarg ----
hmarg_length <- culverts_cmb %>% pull(hmarg_length)
hmarg_nat_per <- culverts_cmb %>% pull(hmarg_length_natural_percent)
low_temp <- culverts_cmb %>% pull(low_temp)
n_salmon_7 <- culverts_cmb %>% pull(n_salmon_7)

hfull <- purrr::map(
  1 : nrow(culverts_cmb), 
  function(x) sum(E[x, ] * hmarg_length) + hmarg_length[x]
) %>% base::unlist()
hfull_nat <- purrr::map(
  1 : nrow(culverts_cmb), 
  function(x) sum(E[x, ] * hmarg_length * hmarg_nat_per) + hmarg_length[x] * hmarg_nat_per[x]
) %>% base::unlist()
hfull_low_temp <- purrr::map(
  1 : nrow(culverts_cmb), 
  function(x) sum(E[x, ] * hmarg_length * low_temp) + hmarg_length[x] * low_temp[x]
) %>% base::unlist()
hfull_species <- purrr::map(
  1 : nrow(culverts_cmb), 
  function(x) sum(E[x, ] * hmarg_length * n_salmon_7) + hmarg_length[x] * n_salmon_7[x]
) %>% base::unlist()

culverts_cmb <- culverts_cmb %>% 
  cbind(., hmarg_nat_per) %>% 
  cbind(., hfull) %>% 
  cbind(., hfull_nat) %>% 
  cbind(., hfull_low_temp) %>% 
  cbind(., hfull_species)

# Save everything ----
save(
  lines_simp, lines_ds, culverts_cmb, marginal_line_ids, 
  downstream_line_ids, D, di, E, 
  file = here("data", "processed", "heuristic_inputs.RData")
  )
     