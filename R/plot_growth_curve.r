#' Plot Growth Curves and Plate Layout from Processed Data Object
#'
#' @param data A structured list containing:
#'   - data: raw time-series data
#'   - meta$variables: grouping variables
#'   - classification$well_summary
#'   - classification$derivative_data
#'
#' @param curve_types_to_plot Optional vector of curve types to include
#' @param store_plots Logical; store plots inside object
#'
#' @return list of plots
#' @export

plot_growth_curve <- function(
    data,
    curve_types_to_plot = NULL,
    store_plots = TRUE
) {
  
  # ==========================================================
  # Extract from structured object
  # ==========================================================
  
  if (!all(c("data", "meta", "classification") %in% names(data))) {
    stop("data must contain 'data', 'meta', and 'classification'")
  }
  
  if (!"well_summary" %in% names(data$classification)) {
    stop("Run classify_growth_curve_types() first")
  }
  
  raw_data <- data$data
  well_summary <- data$classification$well_summary
  variables <- data$meta$variables
  
  # ----------------------------------------------------------
  # Define join keys
  # ----------------------------------------------------------
  
  join_keys <- c(variables, "Plate Type")
  
  conditions <- well_summary %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))
  
  plot_list <- vector("list", nrow(conditions))
  names(plot_list) <- apply(conditions, 1, paste, collapse = "_")
  
  # ==========================================================
  # LOOP OVER CONDITIONS
  # ==========================================================
  
  for (i in seq_len(nrow(conditions))) {
    
    cond <- conditions[i, ]
    
    ws <- well_summary %>%
      dplyr::semi_join(cond, by = join_keys)
    
    # ==========================================================
    # 1️⃣  PLATE LAYOUT
    # ==========================================================
    
    plate_layout <- expand.grid(
      category = LETTERS[1:8],
      number   = 1:12
    )
    
    ws_plate <- ws %>%
      dplyr::mutate(
        category = stringr::str_extract(Well, "^[A-H]"),
        number   = as.numeric(stringr::str_extract(Well, "\\d+"))
      )
    
    plate_df <- plate_layout %>%
      dplyr::left_join(
        dplyr::select(ws_plate, category, number, curve_type),
        by = c("category", "number")
      ) %>%
      dplyr::mutate(
        curve_type = tidyr::replace_na(curve_type, "empty"),
        category  = factor(category, levels = LETTERS[8:1])
      )
    
    title_text <- paste(cond, collapse = " | ")
    
    p_plate <- ggplot2::ggplot(plate_df,
                               ggplot2::aes(x = number, y = category)) +
      ggplot2::geom_tile(ggplot2::aes(fill = curve_type), color = "white") +
      ggplot2::coord_fixed() +
      ggplot2::scale_x_continuous(breaks = 1:12, expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::scale_fill_manual(
        values = c(
          "OD_drop"      = "red",
          "flat_low"     = "blue",
          "growing"      = "darkgreen",
          "slow_growing" = "orange",
          "uncertain"    = "grey50",
          "empty"        = "grey95"
        )
      ) +
      ggplot2::labs(
        title = title_text,
        x = NULL,
        y = NULL,
        fill = "curve_type"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text  = ggplot2::element_text(size = 8),
        legend.position = "bottom"
      )
    
    # ==========================================================
    # 2️⃣  RAW CURVES
    # ==========================================================
    
    raw_subset <- raw_data %>%
      dplyr::semi_join(cond, by = join_keys) %>%
      dplyr::left_join(
        dplyr::select(ws, dplyr::all_of(join_keys), Well, curve_type),
        by = c(join_keys, "Well")
      )
    
    if (is.null(curve_types_to_plot)) {
      curve_types_current <- raw_subset %>%
        dplyr::pull(curve_type) %>%
        unique() %>%
        stats::na.omit() %>%
        setdiff("growing")
    } else {
      curve_types_current <- curve_types_to_plot
    }
    
    raw_subset <- raw_subset %>%
      dplyr::filter(curve_type %in% curve_types_current)
    
    # ----------------------------------------------------------
    # No data case
    # ----------------------------------------------------------
    
    if (nrow(raw_subset) == 0) {
      
      p_raw <- ggplot2::ggplot() + ggplot2::theme_void()
      
      plot_list[[i]] <- patchwork::wrap_plots(
        p_plate, p_raw,
        widths = c(1, 1.7)
      )
      
      next
    }
    
    raw_subset <- raw_subset %>%
      dplyr::mutate(
        curve_type = factor(curve_type, levels = curve_types_current)
      )
    
    p_raw <- ggplot2::ggplot(
      raw_subset,
      ggplot2::aes(
        x = Hour,
        y = diff_OD_590_740,   # 🔥 fixed column name
        group = Well,
        colour = Well
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
      ggplot2::facet_wrap(~ curve_type, scales = "free_y", drop = TRUE) +
      ggplot2::labs(
        x = "Hour",
        y = "Raw OD",
        colour = "Well"
      ) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        legend.position = "right",
        strip.background = ggplot2::element_rect(fill = "grey90"),
        strip.text = ggplot2::element_text(face = "bold"),
        legend.key.height = grid::unit(0.4, "cm"),
        legend.text = ggplot2::element_text(size = 8)
      )
    
    # ==========================================================
    # 3️⃣ Combine
    # ==========================================================
    
    plot_list[[i]] <- patchwork::wrap_plots(
      p_plate, p_raw,
      widths = c(1, 1.7)
    )
  }
  
  # ==========================================================
  # Store in object
  # ==========================================================
  
  if (store_plots) {
    data$visualization$plate_plots <- plot_list
    return(data)
  } else {
    return(plot_list)
  }
}