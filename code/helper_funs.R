# solve the optimization problem assuming owner/area independence 
solve_opt <- function(
  #Inputs
  points, #points with variables: hmarg_length, cost, and wria_number
  budget, #budget constraint
  D, #marginal connectivity matrix
  E #full connectivity matrix
){
  
  opt_i <- function(area_i, owner_i){
    # inputs
    sel_i <- points$area == area_i & points$owner == owner_i
    if (sum(sel_i) > 2)  {
      
      v_i <- points %>% dplyr::pull(h_obj)
      sum_cost <- sum(points$cost)
      brc_i <- points %>% 
        mutate(cost = ifelse(sel_i == TRUE, cost, sum_cost)) %>% 
        pull(cost)
      nb_i <- length(v_i)
      D_i <- D
      di_i <- colSums(D_i)

      hydro_con <- diag(nb_i) - t(D_i)

      model <- list()
      
      model$A          <- rbind(hydro_con, brc_i)
      model$obj        <- points %>% dplyr::pull(h_obj)
      model$modelsense <- 'max'
      model$rhs        <- c(1 - di_i, budget)
      model$sense      <- rep("<=", nb_i + 1)
      model$vtype      <- 'B'
      
      params <- list(OutputFlag = 0)
      
      result <- gurobi(model, params)

      #now return the site IDs for projects in solution
      soln_i <-  points %>% filter(result$x == 1) %>% pull(id)
      status_i <- result$status
      
    } else if (sum(sel_i) == 1) { 
      
        soln_i <- points %>% filter(sel_i) %>% filter(cost < budget) %>% pull(id)
        status_i <- NA
        
    } else {
      
        soln_i <- NA
        status_i <- NA
        
      }
    
  return(list(soln_i = soln_i, status_i = status_i))
  }
  
  owners <- points %>% pull(owner) %>% unique()
  areas <- points %>% pull(area) %>% unique()
  oa_combs <- crossing(owners, areas)
  
  solns <- purrr::map2(
    oa_combs$areas,
    oa_combs$owners,
    ~ opt_i(.x, .y)
  ) 
  
  solns_cmb <- map(solns, ~.x$soln_i) %>% 
    unlist() %>% 
    data.frame(soln_i = .) 
  prtf_sel <- solns_cmb %>% pull(soln_i)
  
  status_cmb <- map(solns, ~.x$status_i) %>% 
    unlist() %>% 
    data.frame(status_i = .) 
  status <- status_cmb %>% pull(status_i)
    
  prtf_sel <- prtf_sel[!is.na(prtf_sel)] %>% 
    as.matrix()
  
  h_inc <- purrr::map(
    1 : nrow(prtf_sel), 
    function(x) {ifelse(sum(E[, prtf_sel[x]]) == 0, 1,
      ifelse(sum(which(E[, prtf_sel[x]] == 1) %in% 
      prtf_sel) == length(which(E[, prtf_sel[x]] == 1)), 1, 0))}
    ) %>% 
    do.call("rbind", .)   
  
  prtf_site_ids <- points$site_id[prtf_sel]
  h_unlock <- points$site_id %in% prtf_site_ids[h_inc == 1]
  habitat_tot <- sum(points$hmarg_length[prtf_sel] * h_inc)
  habitat_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$n_salmon_7[prtf_sel])
  habitat_cool_nat <- sum(points$hmarg_length[prtf_sel] * h_inc * points$cool_nat[prtf_sel])
  habitat_cool_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$cool_species[prtf_sel])
  habitat_nat_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$nat_species[prtf_sel])
  habitat_natural <- sum(points$hmarg_nat[prtf_sel] * h_inc)
  habitat_low_temp <- sum(points$hmarg_length[prtf_sel] * h_inc * points$low_temp[prtf_sel])
  solution <- points$site_id %in% prtf_site_ids

  soln <- list(
    status = status,
    solution = solution,
    habitat_tot = habitat_tot, 
    habitat_species = habitat_species,
    habitat_cool_nat = habitat_cool_nat,
    habitat_cool_species = habitat_cool_species,
    habitat_nat_species = habitat_nat_species,
    habitat_natural = habitat_natural,
    habitat_low_temp = habitat_low_temp
    )
  
  return(soln)
}

# solve rank and score
solve_pi <- function(
  #Inputs
  points, #points with variables: h_obj, hmarg_length, and cost
  budget, #budget constraint for each owner
  E, #connectivity matrix
  huc_lev = NULL,
  huc_id = NULL
){
  
  # set benefit to negative number if not in the selected huc
  if(!is.null(huc_lev) & !is.null(huc_id)){
    points <- points %>% 
      mutate(h_obj = 
        case_when(
         huc_lev == 4 & !huc4 == huc_id ~ -1e3,
         huc_lev == 6 & !huc6 == huc_id ~ -1e3,
         huc_lev == 8 & !huc8 == huc_id ~ -1e3,
         huc_lev == 10 & !huc10 == huc_id ~ -1e3,
         huc_lev == 12 & !huc12 == huc_id ~ -1e3,
         TRUE ~ h_obj
        )
      )
   }
  
  pi_lists <- points %>%
    select(owner, area, h_obj, cost, site_id, id) %>% 
    st_drop_geometry() %>% 
    group_by(owner, area) %>% 
    arrange(owner, -h_obj) %>% 
    mutate(
      Budget = cumsum(cost), 
      rank = row_number(),
      in_plan = ifelse(Budget <= budget, TRUE, FALSE),
      expend = sum(cost * in_plan),
      excess = budget - expend,
      can_afford = ifelse(cost < excess & in_plan == FALSE, TRUE, NA),
      add_to_plan = suppressWarnings(min(can_afford * rank, na.rm = TRUE))
        ) %>% 
    ungroup() 
  
  while (sum(is.na(pi_lists$can_afford)) < nrow(pi_lists))  {
    pi_lists <- pi_lists %>% 
      group_by(owner, area) %>% 
      mutate(
        expend = sum(cost * in_plan),
        excess = budget - expend,
        can_afford = ifelse(cost < excess & in_plan == FALSE, TRUE, NA),
        add_to_plan = suppressWarnings(min(can_afford * rank, na.rm = TRUE)),
        in_plan = ifelse(in_plan == TRUE | rank == add_to_plan, TRUE, FALSE)
      )
    
  }
  
  lists_sel <- pi_lists %>% filter(in_plan)
  prtf_sel <- as.matrix(lists_sel[['id']]) 
  h_inc <- lapply(1 : nrow(prtf_sel), 
    FUN = function(x) ifelse(sum(E[, prtf_sel[x]]) == 0, 1,
      ifelse(sum(which(E[, prtf_sel[x]] == 1) %in% 
        prtf_sel) == length(which(E[, prtf_sel[x]] == 1)), 1, 0))) %>% 
    do.call("rbind", .)   
  h_inc_ids <- points$site_id[prtf_sel]
  h_unlock <- points$site_id %in% h_inc_ids[h_inc == 1]
  habitat_tot <- sum(points$hmarg_length[prtf_sel] * h_inc)
  habitat_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$n_salmon_7[prtf_sel])
  habitat_natural <- sum(points$hmarg_nat[prtf_sel] * h_inc)
  habitat_low_temp <- sum(points$hmarg_length[prtf_sel] * h_inc * points$low_temp[prtf_sel])
  habitat_cool_nat <- sum(points$hmarg_length[prtf_sel] * h_inc * points$cool_nat[prtf_sel])
  habitat_cool_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$cool_species[prtf_sel])
  habitat_nat_species <- sum(points$hmarg_length[prtf_sel] * h_inc * points$nat_species[prtf_sel])
  solution <- points$site_id %in% lists_sel$site_id
  stranded <- sum(solution == TRUE & h_unlock == FALSE)
  
  soln <- list(
    solution = solution,
    habitat_tot = habitat_tot, 
    habitat_species = habitat_species,
    habitat_natural = habitat_natural,
    habitat_low_temp = habitat_low_temp,
    habitat_cool_nat = habitat_cool_nat,
    habitat_cool_species = habitat_cool_species,
    habitat_nat_species = habitat_nat_species,
    h_unlock = h_unlock,
    stranded = stranded
  )
    return(soln)
  }


# Obtain Pareto Frontier
get_pareto_front <- function(
    points, #culverts
    D,
    E,
    resolution = 50, #fineness of budget grid
    holdout_owner = NULL
){
  
  #n owners
  n_o <- length(unique(points$owner))
  
  #n areas
  n_a <- length(unique(points$area))
  
  #budgets
  if(n_o * n_a == 1){
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = sum(points$cost), 
      by = sum(points$cost) / resolution
    )
  } else if(is.null(holdout_owner)) {
    max_budgets <- points %>% 
      group_by(owner, area) %>% 
      summarise(max_budgets = sum(cost)) %>% 
      st_drop_geometry() %>% 
      pull(max_budgets)
    max_budget <- max(max_budgets)
    
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = max_budget * (n_o * n_a), 
      length = resolution
    )  
  } else {
    max_budgets <- points %>% 
      filter(!owner_type_code == holdout_owner) %>% 
      group_by(owner, area) %>% 
      summarise(max_budgets = sum(cost)) %>% 
      st_drop_geometry() %>% 
      pull(max_budgets)
    max_budget <- max(max_budgets)
    
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = max_budget * (n_o * n_a), 
      length = resolution
    )
  }
  
  #solutions
  solns <- purrr::map(1 : length(budget_grd), 
    function (x) {
      solve_opt(
        points = points, 
        budget = budget_grd[x]/(n_o * n_a),
        D = D,
        E = E)
      }
  )
  
  # status
  status <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$status[4]
    }
  ) %>%
    unlist()
  
  #objective vals
  habitat_tot <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_tot
  }
  ) %>% 
    unlist()
  
  
  habitat_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_species
    }
  ) %>% 
    unlist()
  
  habitat_cool_nat <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_cool_nat
    }
  ) %>% 
    unlist()
  
  habitat_cool_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_cool_species
    }
  ) %>% 
    unlist()
  
  habitat_nat_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_nat_species
    }
  ) %>% 
    unlist()
  
  habitat_natural <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_natural
    }
  ) %>% 
    unlist()
  
  habitat_low_temp <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_low_temp
    }
  ) %>% 
    unlist()
  
  #expenditures
  exp <- purrr::map(
    1 : length(solns),
      function (x) {
      sum(points[solns[[x]]$solution, ]$cost)
   }
  ) %>% 
    unlist() 
  
  #Pareto frontier
  front <- data.frame(
    status = status,
    budget = budget_grd, 
    habitat_tot = habitat_tot, 
    habitat_species = habitat_species,
    habitat_cool_nat = habitat_cool_nat,
    habitat_cool_species = habitat_cool_species,
    habitat_nat_species = habitat_nat_species,
    habitat_natural = habitat_natural,
    habitat_low_temp = habitat_low_temp,
    expenditures = exp
    )
  
  return(front)
  
}

# Obtain PI Frontier
get_pi_front <- function(
    points, #culverts
    E,
    resolution = 50, #fineness of budget grid
    holdout_owner = NULL
 ){
  
  #n owners
  n_o <- length(unique(points$owner))
  
  #n areas
  n_a <- length(unique(points$area))
  
  #budgets
  if(n_o * n_a == 1){
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = sum(points$cost), 
      by = sum(points$cost) / resolution
    )
  } else if(is.null(holdout_owner)) {
    max_budgets <- points %>% 
      group_by(owner, area) %>% 
      summarise(max_budgets = sum(cost)) %>% 
      st_drop_geometry() %>% 
      pull(max_budgets)
    max_budget <- max(max_budgets)
    
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = max_budget * (n_o * n_a), 
      length = resolution
    )  
  } else {
    max_budgets <- points %>% 
      filter(!owner_type_code == holdout_owner) %>% 
      group_by(owner, area) %>% 
      summarise(max_budgets = sum(cost)) %>% 
      st_drop_geometry() %>% 
      pull(max_budgets)
    max_budget <- max(max_budgets)
    
    budget_grd <- seq(
      from = sum(points$cost) / resolution, 
      to = max_budget * (n_o * n_a), 
      length = resolution
    )
  }
  
  #solutions
  solns <- purrr::map(
    1 : length(budget_grd), 
    function (x) {
      solve_pi(
        points = points, 
        budget = budget_grd[x]/(n_o * n_a),
        E = E)
      }
  )  
  
  #objective vals
  habitat_tot <- purrr::map(
    1 : length(solns),
    function (x) {solns[[x]]$habitat_tot}
  ) %>% 
    unlist()
  
  habitat_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_species
    }
  ) %>% 
    unlist()
  
  habitat_cool_nat <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_cool_nat
    }
  ) %>% 
    unlist()
  
  habitat_cool_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_cool_species
    }
  ) %>% 
    unlist()
  
  habitat_nat_species <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_nat_species
    }
  ) %>% 
    unlist()
  
  habitat_natural <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_natural
    }
  ) %>% 
    unlist()
  
  habitat_low_temp <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$habitat_low_temp
    }
  ) %>% 
    unlist()
  
  #expenditures
  exp <- purrr::map(
    1 : length(solns),
    function (x) {sum(points[solns[[x]]$solution, ]$cost)}
  ) %>% 
    unlist() 
  
  #stranded
  stranded <- purrr::map(
    1 : length(solns),
    function (x) {
      solns[[x]]$stranded
    }
  ) %>% 
    unlist()
  
  
  #pi frontier
  pi_front <- data.frame(
    budget = budget_grd, 
    habitat_tot = habitat_tot, 
    habitat_species = habitat_species, 
    habitat_cool_nat = habitat_cool_nat, 
    habitat_nat_species = habitat_nat_species, 
    habitat_cool_species = habitat_cool_species, 
    habitat_natural = habitat_natural,
    habitat_low_temp = habitat_low_temp,
    expenditures = exp,
    stranded = stranded
    )
  
  return(pi_front)
  
}


# Map the optimization solution
leaflet_soln <- function(
    points, #culverts
    lines, #lines with linestring geometries if glify = TRUE
    dslines, #lines with linestring geometries downstream of culvs
    soln, #output from solve_opt or solve_pi()
    marginal_line_ids, #comids for all lines marginally upstream of each point
    downstream_line_ids,
    h_unlock = NULL, #output from solve_pi()
    huc_lev = NULL,
    huc_id = NULL
){
  
  #Replace solution with culverts contributing to habitat 
  if(!is.null(h_unlock)){
    selected <- soln
    soln <- soln == TRUE & h_unlock == TRUE
  }
  
  if(sum(soln) == 0){
    
    blocked_lines <- marginal_line_ids %>% base::unlist()
    
    leaflet_lines <- lines %>% 
      mutate(in_huc = case_when(
        huc_lev == 4 & !huc4 == huc_id ~ 0,
        huc_lev == 6 & !huc6 == huc_id ~ 0,
        huc_lev == 8 & !huc8 == huc_id ~ 0,
        huc_lev == 10 & !huc10 == huc_id ~ 0,
        huc_lev == 12 & !huc12 == huc_id ~ 0,
        TRUE ~ 1
      )) %>% 
      filter(COMID %in% blocked_lines, in_huc == 1)
    
    m <- leaflet(
      points, 
      options = leafletOptions(minZoom = 7))  %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale")  %>%
      addScaleBar("bottomleft")  %>% 
      addGlPolylines(
        data = leaflet_lines %>%  filter(FCODE != 55800),
        color = "#d46666", 
        opacity = 0.5
      )
    
  } else {
    #Crop lines to wrias in solution
    soln_wrias <- unique(points[soln, ]$wria_number)
    in_soln_wrias <- points$wria_number %in% soln_wrias
    
    #Get blocked lines and line ids
    blocked_lines <- marginal_line_ids[in_soln_wrias] %>% unlist()
    ds_blocked_lines <- downstream_line_ids[in_soln_wrias] %>% base::unlist()
    
    leaflet_lines <- lines %>% filter(COMID %in% blocked_lines)
    ds_leaflet_lines <- dslines %>% dplyr::filter(COMID %in% ds_blocked_lines)
    
    #Defined unblocked line ids
    soln_stream_ids <- marginal_line_ids[soln] %>% unlist()
    ds_stream_ids <- downstream_line_ids[soln] %>% base::unlist()
    
    #Create Leaflet map object
    m <- leaflet::leaflet(points, 
                          options = leaflet::leafletOptions(minZoom = 7))  %>%
      leaflet::addProviderTiles("CartoDB.Positron", group = "Grayscale")  %>%
      leaflet::addScaleBar("bottomleft")  %>% 
      leafgl::addGlPolylines(data = leaflet_lines %>%  
                               filter(FCODE != 55800, !COMID %in% soln_stream_ids),
                             color = "#d46666", 
                             opacity = 0.5
      ) %>% 
      leafgl::addGlPolylines(
        data = leaflet_lines %>% 
          filter(COMID %in% soln_stream_ids),
        color = "#3cdd78", 
        opacity = 0.5
      ) %>% 
      leafgl::addGlPolylines(
        data = ds_leaflet_lines %>%
          dplyr::filter(COMID %in% ds_stream_ids & !COMID %in% soln_stream_ids),
        color = "#b0b0b0",
        opacity = 0.25
      ) 
    
  }
  
  pal <- leaflet::colorNumeric(c("#d9a1a0", "#91afeb"), 0 : 1)
  
  if(!is.null(h_unlock)) {
    m <- m %>% 
      leaflet::addCircleMarkers(
        lng = ~ site_longitude,
        lat = ~ site_latitude,
        radius = 5,
        weight = 1.5,
        color = ~pal(selected),
        fillOpacity = 1,
        opacity = 1,
        clusterOptions = leaflet::markerClusterOptions(
          iconCreateFunction = JS("function (cluster) {    
          var childCount = cluster.getChildCount();  
          if (childCount < 100) {  
          c = 'rgba(204, 252, 255, 1.0);'
          } else if (childCount < 1000) {  
          c = 'rgba(237, 192, 181, 1);'  
          } else { 
          c = 'rgba(164, 164, 243, 1);'  
          }    
         return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', 
         className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),  
          spiderfyOnMaxZoom = FALSE,
          disableClusteringAtZoom = 10
        ),
        popup = ~ popup)
    
  } else {
    m <- m %>% 
      leaflet::addCircleMarkers(
        lng = ~ site_longitude,
        lat = ~ site_latitude,
        radius = 5,
        weight = 1.5,
        color = ~pal(soln),
        fillOpacity = 1,
        opacity = 1,
        clusterOptions = leaflet::markerClusterOptions(
          iconCreateFunction = JS("function (cluster) {    
          var childCount = cluster.getChildCount();  
          if (childCount < 100) {  
          c = 'rgba(204, 252, 255, 1.0);'
          } else if (childCount < 1000) {  
          c = 'rgba(237, 192, 181, 1);'  
          } else { 
          c = 'rgba(164, 164, 243, 1);'  
          }    
         return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', 
         className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),  
          spiderfyOnMaxZoom = FALSE,
          disableClusteringAtZoom = 10
        ),
        popup = ~ popup)
  }
  
  return(m)
}

# get heatmap data
get_hm_dat <- function(
    opt_fronts, 
    heu_fronts, 
    attribute = "habitat_tot",
    obj_dfn = "multi") {
  
  attribute_opt <- sapply(opt_fronts, "[[", attribute)
  attribute_heu <- sapply(heu_fronts, "[[", attribute)
  attribute_diff <- attribute_opt - attribute_heu
  attribute_per_diff <- attribute_diff/attribute_heu * 100
  
  colnames(attribute_per_diff) <- alphas
  
  hm_dat <- attribute_per_diff %>%
    as_tibble() %>%
    bind_cols(budget_grd, .)  %>%
    rename(X = budget_grd) %>% 
    gather(key = "Y", value = "Z", -1) %>%
    mutate(
      Y = as.numeric(gsub("V", "",Y)),
      center = ifelse(Y == 0.5 & X > 0.5 & X < 0.6, 1, 0),
      ex = ifelse(Y == 0.3 & X > 0.5 & X < 0.6, 1, 0),
      obj_fn = obj_dfn
    ) %>% 
    dplyr::arrange(ex)
  
  return(hm_dat)
}
  
# get line plot data
get_lp_dat <- function(
    fronts, 
    alpha_lev = 0.3,
    attribute = "habitat_tot",
    obj_dfn = "multi"
    ) {
  
  attribute <- sapply(fronts, "[[", attribute)
   
  colnames(attribute) <- alphas
  
  lp_dat <- attribute %>%
    as_tibble() %>%
    bind_cols(budget_grd, .)  %>%
    rename(X = budget_grd) %>% 
    gather(key = "Y", value = "Z", -1) %>%
    mutate(
      Y = as.numeric(gsub("V", "",Y)),
      objective = obj_dfn
    ) %>% 
    filter(Y == alpha_lev)
  
  return(lp_dat)
}


# get line plot data for random portfolios
get_rand_lp_dat <- function(
    front, 
    alpha_lev = 0.3,
    attribute = "habitat_tot",
    budget_grd = budget_grd
) {
  
  rand_attribute <- front %>% select(attribute)
  rand_dat <- rand_attribute %>%
    as_tibble() %>%
    bind_cols(budget_grd, .)  %>%
    rename(
      X = budget_grd,
      Z = value
    ) %>% 
    mutate(
      method = "rand",
      Y = 0.3
    ) 
  
  return(rand_lp_dat)
}

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
