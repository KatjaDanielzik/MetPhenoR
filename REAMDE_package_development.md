# bug report by Rohit
cannot read in data -> implement robust read in of all separators via
read_delim
solved in 0.0.2

# annotate_chemical
add warning message if nothing was annotated (e.g. wrong annotation file)

# plot_auc_ratio_heatmap
warning message if x_var only has one category and therefore cannot be plotted

# plot_auc_ratio_violin
pathway_filter -> group_filter; to be consistent with other functions

# all plotting functions with x_var or color_var
default -> first of variables

# auc_ratio
check why control wells have different AUC ratios ! -> do not display control
wells

# TODO
- transfer to github
- create a docker container for the package
- publish on bioconductor (needs at least SummarizedExperiment for Integration) or CRAN?
- include Julia's data (anonymized)