#' Create a heatmap matrix showing the overlap between sources
#' 
#' Show overlap between different record sources, either by showing the 
#' number or the percentages of shared records between any pair of sources. 
#' 
#' @param data A tibble with one record per row, an id column and then one column
#' per source indicating whether the record was found in that source.
#' @param plot_type Either `counts` (number of shared records) or `percentages`
#' (share of overlapping records).
#' @param sort_sources Should sources be shown based on the number of records they
#' contained? If FALSE, order of data is retained.
#' @param interactive Should returned plot be interactive and enable user to export
#' records underlying each field?
#' @return The requested plot as a either a `ggplot2` object (when interactive = FALSE), which can then be 
#' further formatted or saved using [ggplot2::ggsave()], or a `plotly` object when `interactive = TRUE`
#' @export
#' @examples 
#' data <- data.frame(
#'   article_id = 1:500,
#'   source1 = rbinom(500, 1, .5) == 1,
#'   source2 = rbinom(500, 1, .2) == 1,
#'   source3 = rbinom(500, 1, .1) == 1,
#'   source4 = rbinom(500, 1, .6) == 1,
#'   source5 = rbinom(500, 1, .7) == 1
#' )
#' 
#' plot_source_overlap_heatmap(data)
#' plot_source_overlap_heatmap(data, plot_type = "percentages")

plot_source_overlap_heatmap <- function(data, plot_type = c("counts", "percentages"), sort_sources = TRUE, interactive = FALSE) {

  plot_type <- plot_type[1]
  if(!plot_type %in% c("counts", "percentages")) 
    stop("plot_type must be counts or percentages")
  
  data <- data %>% dplyr::select(tidyselect::vars_select_helpers$where(is.logical))
  
  
  source_sizes <- colSums(data)
  names(source_sizes) <- colnames(data)
  
  source_sizes <- sort(source_sizes)
  if (sort_sources) {
    sources_order <- names(source_sizes)
  } else {
    sources_order <- names(data)
  }
  
  data <- data %>% dplyr::select(dplyr::all_of(sources_order))
  
  cooc_mat <- purrr::map_dfr(names(data), function (source) {
    data[data[source] == 1,] %>% colSums()
  }) %>% dplyr::select(dplyr::all_of(names(data))) %>% as.matrix()  
  
  
  if (plot_type == "counts") {
    cooc_mat_plot <- cooc_mat
    
    cooc_mat_plot[upper.tri(cooc_mat_plot)] <- NA
    
    cooc_mat_plot %>% dplyr::as_tibble() %>% dplyr::mutate(DB1 = names(data)) %>%
      tidyr::pivot_longer(-.data$DB1, names_to = "DB2", values_to = "records") %>%
      dplyr::arrange(.data$DB1) %>%
      ggplot2::remove_missing(na.rm = TRUE) %>%
      ggplot2::ggplot(ggplot2::aes(.data$DB1, .data$DB2, fill = .data$records)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low="white") +
      ggplot2::geom_text(ggplot2::aes(label=.data$records)) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_discrete(limits = rev(sources_order), guide = ggplot2::guide_axis(angle = 45)) +
      ggplot2::scale_y_discrete(limits = sources_order) +
      ggplot2::labs(x="", y="", fill = "Records") 
    
  } else if (plot_type == "percentages") {
    
    overlap_matrix <- cooc_mat/diag(cooc_mat)
    
    labels_matrix <- overlap_matrix
    labels_matrix[TRUE] <- paste(round(overlap_matrix, 2) * 100, "%")
    
    diag(labels_matrix) <- diag(cooc_mat)
    
    labels_df <- labels_matrix %>% tibble::as_tibble() %>% dplyr::mutate(DB1 = names(data)) %>%
      tidyr::pivot_longer(-.data$DB1, names_to = "DB2", values_to = "label")
    
    diag(overlap_matrix) <- NA
    
    overlap_matrix %>% tibble::as_tibble() %>% dplyr::mutate(DB1 = names(data)) %>%
      tidyr::pivot_longer(-.data$DB1, names_to = "DB2", values_to = "records") %>%
      ggplot2::ggplot(ggplot2::aes(.data$DB1, .data$DB2, fill = .data$records)) +
      ggplot2::geom_tile(height = .9) +
      ggplot2::scale_fill_gradient(low="white", labels = scales::percent, limits = c(0, 1)) +
      ggplot2::geom_text(data = labels_df, ggplot2::aes(label=.data$label, fill = NULL)) +
      ggplot2::labs(x="", y="", fill = "Overlap", 
           caption = "Note: Percentages indicate share of records in row also found in column, 
       number of results in each database is shown on the diagonal") +
      ggplot2::scale_x_discrete(limits = rev(sources_order), guide = ggplot2::guide_axis(angle = 45)) +
      ggplot2::scale_y_discrete(limits = sources_order) +
      ggplot2::theme_light()
  }
  
}

#' Create an UpSetR upset plot showing the overlap between sources
#' 
#' Show records found in specific sets of sources to identify the unique contribution
#' of each source and of any subsets
#' 
#' @param data A tibble with one record per row, an id column and then one column
#' per source indicating whether the record was found in that source.
#' @inheritParams UpSetR::upset
#' @inheritDotParams UpSetR::upset -sets.x.label -mainbar.y.label -order.by
#' @export
#' @references Conway, J. R., Lex, A., & Gehlenborg, N. (2017). UpSetR: an R package for the visualization of intersecting sets and their properties. Bioinformatics.

#' @examples 
#' data <- data.frame(
#'   article_id = 1:500,
#'   source1 = rbinom(500, 1, .5) == 1,
#'   source2 = rbinom(500, 1, .2) == 1,
#'   source3 = rbinom(500, 1, .1) == 1,
#'   source4 = rbinom(500, 1, .6) == 1,
#'   source5 = rbinom(500, 1, .7) == 1
#' )
#' 
#' plot_source_overlap_upset(data)
#' 
#' # To start with the records shared among the greatest number of sources,
#' # use
#' 
#' plot_source_overlap_upset(data, decreasing = c(TRUE, TRUE))


plot_source_overlap_upset <- function(data, nsets = ncol(data) - 1,  sets.x.label = "Number of records", 
                                      mainbar.y.label = "Overlapping record count", order.by = c("freq", "degree"), ...) {

  if (nsets > 5) message("Plotting a large number of sources. Consider reducing nset or sub-setting the data.") 
  
    data %>% dplyr::transmute(dplyr::across(tidyselect::vars_select_helpers$where(is.logical), as.numeric)) %>%
      UpSetR::upset(nsets = nsets, order.by = order.by, sets.x.label = sets.x.label, mainbar.y.label = mainbar.y.label, ...)
}

# Hack to suppress check warning re default values below
cite_source <- cite_label <- type <- NULL

#' Create a bar chart that compares source contributions over stages
#' 
#' Create a faceted plot that shows unique contributions and duplicated records across
#' two metadata dimensions. Most typical use-case might be to show the contributions of each source
#' across different screening stages.
#' 
#' @param data A tibble with one hit per row, with variables indicating meta-data of interest.
#' @param facets Variable in data used for facets (i.e. sub-plots). Defaults to source (i.e. cite_source)
#' @param bars Variable in data used for bars. Defaults to label (i.e. cite_label)
#' @param color Color used to fill bars. Default to `unique`
#' @param center Logical. Should one color be above and one below the axis?
#' @export
#' @examples 
#' data <- data.frame(
#'   article_id = 1:100,
#'   cite_source = sample(c("DB 1", "DB 2", "DB 3"), 100, replace = TRUE),
#'   cite_label = sample(c("2020", "2021", "2022"), 100, replace = TRUE),
#'   type = c("unique", "duplicated")[rbinom(100, 1, .7) + 1]
#' )
#' 
#' plot_contributions(data, center = TRUE)

plot_contributions <- function(data, facets = cite_source, bars = cite_label, color = type, center = FALSE) {
  facets <- rlang::enquo(facets)
  bars <- rlang::enquo(bars)
  color <- rlang::enquo(color)
  
  if (!rlang::as_name(facets) %in% colnames(data)) stop("Column ", rlang::as_name(facets), " not found in data." )
  if (!rlang::as_name(bars) %in% colnames(data)) stop("Column ", rlang::as_name(bars), " not found in data." )
  if (!rlang::as_name(color) %in% colnames(data)) stop("Column ", rlang::as_name(color), " not found in data." )
  
  if (!center) {
  
    ggplot2::ggplot(data, ggplot2::aes(!!bars, fill = !!color)) + ggplot2::geom_bar() + 
           ggplot2::facet_grid(ggplot2::vars(!!facets)) + ggplot2::labs(y = "Citations")
  } else {
    vals <- unique(data %>% dplyr::select(!!color) %>% dplyr::pull())
    if(length(vals) != 2) stop("center is only implemented for two colors")
    
    data_sum <- data %>% dplyr::group_by(!!bars, !!facets, !!color) %>% dplyr::summarise(n = dplyr::n())
    
    ggplot2::ggplot(data, ggplot2::aes(!!bars, fill = !!color)) + 
      ggplot2::geom_bar(data = data_sum %>% dplyr::filter(!!color != vals[1]), ggplot2::aes(y = -.data$n), stat = "identity") + 
      ggplot2::geom_bar(data = data_sum %>% dplyr::filter(!!color == vals[1]), ggplot2::aes(y = .data$n), stat = "identity") +
      ggplot2::facet_grid(cols = ggplot2::vars(!!facets)) + ggplot2::labs(y = "Citations") + 
      ggplot2::scale_y_continuous(labels = abs)
    
  }
  
}

