#' Plot Latest Timepoint per Experimental Condition
#'
#' Extracts the latest timepoint per well and experimental condition,
#' then generates plate-layout scatter plots colored by raw values.
#'
#' @param data A data frame containing plate-based measurements.
#' @param variables Character vector of experimental variables
#'   (e.g. c("cell_line", "dose", "status")).
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom tidyselect all_of
#' 
#' @return A named list of ggplot objects.
#' @export
plot_latest_timepoint <- function(data, variables) {
  
  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  
  required_cols <- c(variables, "Plate.Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Step 1: Extract latest timepoint ----
  latest_timepoint <- data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate.Type", "Well")))
    ) %>%
    dplyr::slice_max(.data$Hour, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # ---- Step 2: Convert well positions ----
  latest_timepoint <- latest_timepoint %>%
    dplyr::mutate(
      category = stringr::str_extract(.data$Well, "^[A-H]"),
      number = as.numeric(stringr::str_extract(.data$Well, "\\d+"))
    )
  
  # ---- Step 3: Create grouping variable ----
  latest_timepoint <- latest_timepoint %>%
    tidyr::unite(
      "group_id",
      tidyselect::all_of(c(variables, "Plate.Type")),
      remove = FALSE,
      sep = "_"
    )
  
  # ---- Step 4: Plot function (internal) ----
  .plot_latest_point <- function(df) {
    
    # Dynamic subtitle
    var_values <- purrr::map_chr(variables, function(v) {
      paste0(v, ": ", df[[v]][1])
    })
    
    subtitle_text <- paste(
      paste(var_values, collapse = " | "),
      paste("Plate:", unique(df$Plate.Type)),
      sep = " | "
    )
    
    ggplot2::ggplot(df, ggplot2::aes(x = .data$number, y = .data$category)) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$diff_OD_590_740),
        size = 8
      ) +
      ggplot2::scale_x_continuous(breaks = 1:12) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::scale_color_gradient(
        low = "white",
        high = "purple",
        oob = scales::squish
      ) +
      ggplot2::coord_fixed() +
      ggplot2::labs(
        title = "OD at Latest Time Point",
        subtitle = subtitle_text,
        x = NULL,
        y = NULL,
        color = "diff_OD_590_740"
      ) +
      ggplot2::theme_minimal(base_size = 14)
  }
  
  # ---- Step 5: Split and generate plots ----
  split_data <- dplyr::group_split(latest_timepoint, .data$group_id)
  
  plot_names <- latest_timepoint %>%
    dplyr::distinct(.data$group_id) %>%
    dplyr::pull(.data$group_id)
  
  plots <- purrr::map(split_data, .plot_latest_point)
  names(plots) <- plot_names
  
  return(plots)
}