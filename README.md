# OdinR

The **complete workflow** including examples can be found in vignettes/OdinR.html.
A flowchart of the package can be found in man/figures/OdinR_workflow_flowchart.png

## Installation from GitLab

Install the latest version directly from your GitLab repository using the `remotes` package:

```r
# Install remotes if you haven't already
if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install OdinR from GitHub
remotes::install_github(KatjaDanielzik/OdinR)
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

## Quick Start: For your first workflow please refer to the Vignette 

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
| `Plot is blank` | Check if `processed_data` contains data after filtering. 

## Contact

Have questions, feedback, or want to contribute?  
Email: [katja.danielzik@uni-due.de](mailto:katja.danielzik@uni-due.de)


## Final Tip

**Always check `qc_data()` after reading your data.**  
A few minutes of inspection now can save hours of confusion later.



