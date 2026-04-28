# OdinR

The **complete workflow** including examples can be found in vignettes/OdinR.html

## Installation from GitLab

Install the latest version directly from your GitLab repository using the `remotes` package:

```r
# Install remotes if you haven't already
if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install OdinR from GitLab
remotes::install_gitlab(
  "ada697e/odin_package",
  ref = "final_check"  # or specify a branch/tag (e.g., "v1.0.0")
)
```

## Data Preparation (Before You Run Any Code)

Before analyzing your data:

1. Create a new folder (e.g., `Odin_Analysis`).
2. Copy **all** your `RawRead.csv` and `PlateInfo.csv` files into this folder.
3. In `PlateInfo.csv`, **create or select one column** (e.g., `Sample`) to store experimental metadata.
4. Format entries like:  
   `H460_CT_2h`  
   `74158M4_0Gy_RMC6236_2h`  
   → Use **underscores**, **no spaces**, and **consistent capitalization** (R is case-sensitive!).

> **Never use spaces or mixed case** — this causes errors in `read_folder()`.

---

## Quick Start: Your First Analysis

```r
# Load the package
library(OdinR)

# Load example data (to test the workflow)
data("RawRead")
data("PlateInfo")

# Read your data (replace path with your own)
# data <- read_folder(
#   data_path = "A:/Odin_Analysis",
#   sample_column = "Sample",
#   variables = c("cell.line", "condition", "time.h")
# )

# Run the full workflow:
# 1. QC: qc_data(data)
# 2. Plot final timepoint: plot_latest_timepoint(data)
# 3. Annotate chemicals: processed_data <- annotate_chemical(data, annotation = PM_M01_M02)
# 4. Classify curves: processed_data <- classify_growth_curve_types(processed_data)
# 5. Filter bad plates: processed_data <- filter_data(processed_data, plate_type = "PM-M01", filters = list(...))
# 6. Compute AUC: processed_data <- compute_auc_ci(processed_data)
# 7. Normalize: processed_data <- compute_auc_ratio(processed_data)
# 8. Visualize: plot_auc_ratio_heatmap(processed_data, plate_type = "PM-M01", x_var = "condition")
```

**All functions are documented** — type `?function_name` in R to see help pages.

## Key Features

| Feature | Why It Matters |
|-------|----------------|
| Noise correction: `OD₅₉₀ - OD₇₄₀` | Removes background noise (valid for adherent cells only). |
| `qc_data()` plot | Spot missing files, inconsistent conditions, or technical issues early. |
| `classify_growth_curve_types()` | Identify flat, dropping, or noisy curves — filter intelligently. |
| AUC + normalization | Compare across plates and conditions fairly. |
| Built-in annotations | KEGG pathways (PM-M01/M02), drug info (PM-M11–M14). |
| Heatmaps & violin plots | Visualize biological trends without coding from scratch. |


##  Important Limitations

- **Not for bacteria or suspension cultures** — OD₇₄₀ reflects cell growth in these systems.
- **AUC does not capture curve shape** — two very different curves can have the same AUC.
- **Does not account for cell growth** — if your cells grow rapidly, this may confound results.
- **Does not interpret biological meaning** — a "hit" in a pathway does not imply activation.

> **Always interpret results in biological context** — this package helps you *analyze*, not *explain*.


##  Troubleshooting

| Issue | Solution |
|------|----------|
| `No data in qc_data()` | Check for spaces in `Sample` column; ensure all files are in the folder. |
| `Filtering didn’t work` | Double-check `plate_type` and `filters` spelling. |
| `Plot is blank` | Check if `processed_data` contains data after filtering. |
| `Package not found` | Ensure GitLab URL is correct and you have internet access. |

email: **katja.danielzik@uni-due.de**



## Contact

Have questions, feedback, or want to contribute?  
Email: [katja.danielzik@uni-due.de](mailto:katja.danielzik@uni-due.de)


## Final Tip

> **Always check `qc_data()` after reading your data.**  
> A few minutes of inspection now can save days of confusion later.



