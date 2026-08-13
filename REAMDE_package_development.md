# bug report by Rohit
cannot read in data -> implement robust read in of all seperators via
read_delim

# annotate_chemical
add warning message if nothing was annotated (e.g. wrong annotation file)

# plot_auc_ratio_heatmap
warning message if x_var only has one category and therefore cannot be plotted

# plot_auc_ratio_violin
pathway_filter -> group_filter; to be consistent with other functions

# TODO
- transfer to github
- create a docker container for the package
- publish on bioconductor (needs at least SummarizedExperiment for Integration) or CRAN?