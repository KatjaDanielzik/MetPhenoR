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

# classify_growth_curve_types
- do not plot "New names:"

# TODO
- authors (Willson author but not maintainer): done
- one paragraph description: done
- include licence: done
- include Julia's data (anonymized)
- transfer to github: done, add to project: done
- create a docker container for the package
- publish on CRAN?
- announce package in GRK, announce doi to co-authors and add doi to ORCID
