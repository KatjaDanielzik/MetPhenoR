#' Plot Final Time Point per Experimental Condition
#'
#' This function extracts the final (latest) time point for each well across
#' experimental conditions and visualizes the corresponding measurements in a
#' 96-well plate layout.
#'
#' Each plot represents a single experimental condition and displays wells as
#' spatially arranged points, colored according to their measured value at the
#' final time point.
#'
#' @param data Result of 'read_folder()'. 
#' @param variables A character vector specifying the column names that define
#'   experimental conditions (e.g., \code{c("cell_line", "dose", "status")}).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Groups the data by experimental variables, plate type, and well.
#'   \item Extracts the latest time point per well.
#'   \item Converts well identifiers (e.g., "A01") into plate coordinates.
#'   \item Generates a separate plate-layout plot for each experimental condition.
#' }
#'
#' Color intensity reflects the magnitude of \code{diff_OD_590_740} at the final
#' recorded time point.
#'
#' @return A named list of \code{ggplot2} objects, where each element corresponds
#'   to a unique experimental condition.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom tidyr unite
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr map
#' @importFrom tidyselect all_of
#'
#' @export
plot_latest_timepoint <- function(data, 
                                  variables = data$meta$variables) {
  
  data_df <- data$data
  
  # ---- Input validation ----
  if (!is.data.frame(data_df)) {
    stop("'data$data' must be a data.frame.")
  }
  
  required_cols <- c(variables, "Plate Type", "Well", "Hour", "diff_OD_590_740")
  missing_cols <- setdiff(required_cols, names(data_df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # ---- Step 1: Extract latest time point per well ----
  latest_timepoint <- data_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(variables, "Plate Type", "Well")))
    ) %>%
    dplyr::slice_max(.data$Hour, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  # ---- Step 2: Convert well identifiers to plate coordinates ----
  latest_timepoint <- latest_timepoint %>%
    dplyr::mutate(
      category = stringr::str_extract(.data$Well, "^[A-H]"),
      number = as.numeric(stringr::str_extract(.data$Well, "\\d+"))
    )
  
  # ---- Step 3: Create grouping identifier ----
  latest_timepoint <- latest_timepoint %>%
    tidyr::unite(
      "group_id",
      tidyselect::all_of(c(variables, "Plate Type")),
      remove = FALSE,
      sep = "_"
    )
  
  # ---- Step 4: Internal plotting function ----
  .plot_latest_point <- function(df) {
    
    # Construct descriptive subtitle
    var_values <- purrr::map_chr(variables, function(v) {
      paste0(v, ": ", df[[v]][1])
    })
    
    subtitle_text <- paste(
      paste(var_values, collapse = " | "),
      paste("Plate:", unique(df$"Plate Type")),
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
        title = "Measurement at Final Time Point",
        subtitle = subtitle_text,
        x = NULL,
        y = NULL,
        color = "diff_OD_590_740"
      ) +
      ggplot2::theme_minimal(base_size = 14)
  }
  
  # ---- Step 5: Split data and generate plots ----
  split_data <- dplyr::group_split(latest_timepoint, .data$group_id)
  
  plot_names <- latest_timepoint %>%
    dplyr::distinct(.data$group_id) %>%
    dplyr::pull(.data$group_id)
  
  plots <- purrr::map(split_data, .plot_latest_point)
  names(plots) <- plot_names
  
  return(plots)
}
