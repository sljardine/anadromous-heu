# Replication Materials for: Improving Restoration Heuristics to Support Anadromous Fish Passage


This repository provides code to evaluate and improve heuristics for prioritizing river barrier removals to support anadromous fish passage. The scripts benchmark common heuristic approaches against optimization outcomes, highlighting how simple rules (e.g., prioritizing barriers based on total upstream habitat) can be redesigned to achieve better restoration outcomes across multiple habitat objectives.

The upstream [app](https://example.com) allows users to explore the underlying model data and run custom optimization scenarios, but the analysis in this repository runs in R and produces all benchmarking results and figures.

---

## Scripts Overview

| Script | Purpose |
|--------|---------|
| **helper_funs.R** | Core functions for optimization (`solve_opt`), heuristic scoring (`solve_pi`), Pareto and PI frontiers (`get_pareto_front`, `get_pi_front`), and visualization (`leaflet_soln`, `get_hm_dat`, `get_lp_dat`). Requires a Gurobi license to run optimizations. |
| **01_add_features.R** | Prepares and augments the culvert dataset with features like normalized upstream/downstream counts and habitat variables for analysis. |
| **02_quant_fronts.R** | Computes Pareto and heuristic frontiers, which are input data for Figure 5. |
| **03_quant_fig.R** | Generates Figures 5 summarizing frontier performance and comparisons across multiple expenditure levels and objectives. |
| **04_2x2_dat.R** | Prepares data for “2×2” data, which are input data for Figure 6. |
| **05_2x2_radar.R** | Generates Figure 6 showing performance across implied habitat quality goals. |
| **06_stranded_fronts.R** | Generates Figure 7 showing the number of stranded investments of the SRpp approach across multiple expenditure levels. |
| **07_2x2_heatmaps.R** | Generates SI4 heatmaps illustrating how heuristic performance varies across different objective weightings (alpha) and budgets (beta). |
| **08_penalty.R** | Generates SI5, exploring the robustness of heuristics to penalties for upstream, downstream, and combined barrier conditions. |
| **09_wria_analysis.R** | Generates SI3, comparing Spa and Spp heuristics across HUC8 watersheds. |


---

## Usage Overview

1. Install R dependencies: `here`, `grid`, `gurobi` (requires license), `Matrix`, `sf`, and `tidyverse`.  
2. Run the analysis scripts in order (02 → 09) which will source `helper_funs.R`. Note script 01 will not run without input data not included in this repository due to file size.
3. See `output` folder for generated figures.
