# anadromous-heu

This repository provides replication materials for Improving Restoration Heuristics to Support Anadromous Fish Passage.

Input data can be further explored using the upstream [app](https://example.com) and the scripts in this repository replicate all results in the manuscript and supplementary information.

---

## Scripts Overview

| Script | Purpose |
|--------|---------|
| **helper_funs.R** | Core functions for optimization (`solve_opt`), heuristic scoring (`solve_pi`), Pareto and PI frontiers (`get_pareto_front`, `get_pi_front`), and visualization (`leaflet_soln`, `get_hm_dat`, `get_lp_dat`). Requires a Gurobi license to run optimizations. |
| **01_quant_fronts.R** | Computes Pareto and heuristic frontiers, which are input data for Figure 5. |
| **02_quant_fig.R** | Generates Figures 5 summarizing frontier performance and comparisons across multiple expenditure levels and objectives. |
| **03_2x2_dat.R** | Prepares data for “2×2” data, which are input data for Figure 6. |
| **04_2x2_radar.R** | Generates Figure 6 showing performance across implied habitat quality goals. |
| **05_stranded_fronts.R** | Generates Figure 7 showing the number of stranded investments of the SRpp approach across multiple expenditure levels. |
| **06_2x2_heatmaps.R** | Generates SI4 heatmaps illustrating how heuristic performance varies across different objective weightings (alpha) and budgets (beta). |
| **07_penalty.R** | Generates SI5, exploring the robustness of heuristics to penalties for upstream, downstream, and combined barrier conditions. |
| **08_huc8_boxplots.R** | Generates SI3, comparing Spa and Spp heuristics across HUC8 watersheds. |

---

## Usage Overview

1. Install R dependencies: `here`, `grid`, `gurobi` (requires license), `Matrix`, `sf`, and `tidyverse`.  
2. Run the analysis scripts in order (01 → 8) which will source `helper_funs.R`.  
3. See `output` folder for generated figures.
