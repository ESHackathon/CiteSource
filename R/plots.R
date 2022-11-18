#' Create a heatmap matrix showing the overlap between sources
#'
#' Show overlap between different record sources, either by showing the
#' number or the percentages of shared records between any pair of sources.
#'
#' @param data A tibble with one record per row, an id column and then one column
#' per source indicating whether the record was found in that source.
#' @param cells Variable to display in the cells. Should be 'source', 'label' or 'string'
#' @param facets Variable in data used for facets (i.e. sub-plots). Should be NULL, 'source', 'label' or 'string'
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
#'
plot_source_overlap_heatmap <- function(data, cells = "source", facets = NULL, plot_type = c("counts", "percentages"), sort_sources = TRUE, interactive = FALSE) {
  plot_type <- plot_type[1]
  if (!plot_type %in% c("counts", "percentages")) {
    stop("plot_type must be counts or percentages")
  }

  data <- data %>% dplyr::select(tidyselect::vars_select_helpers$where(is.logical))

  if (sort_sources) {
    source_sizes <- data %>%
      dplyr::select(tidyselect::matches(paste0(cells, "__"))) %>%
      colSums()
    names(source_sizes) <- data %>%
      dplyr::select(tidyselect::matches(paste0(cells, "__"))) %>%
      colnames()
    source_sizes <- sort(source_sizes)
    sources_order <- names(source_sizes)
  } else {
    sources_order <- data %>%
      dplyr::select(tidyselect::matches(paste0(cells, "__"))) %>%
      colnames()
  }

  if (!is.null(facets)) {
    data <- data %>%
      tidyr::pivot_longer(tidyselect::matches(paste0(facets, "__")), names_to = "facet") %>%
      dplyr::mutate(facet = stringr::str_remove(facet, paste0(facets, "__"))) %>%
      dplyr::filter(.data$value == TRUE) %>%
      split(.$facet)
  } else {
    data$facet <- "1"
    data <- list(data)
  }
  
  data <- purrr::map(data, function(df) {
    df <- df %>% dplyr::select(dplyr::all_of(sources_order))
    cooc_mat <- purrr::map_dfr(names(df), function(source) {
      df[df[source] == 1, ] %>% colSums()
    }) %>% dplyr::select(dplyr::all_of(names(df)))
  })



  if (plot_type == "counts") {
    data <- purrr::map(data, function(df) {
      df[upper.tri(df)] <- NA
      df
    })


    data <- purrr::map_dfr(data, .id = "facet", ~.x)

    data[["DB1"]] <- rep(sources_order, length(unique(data$facet)))

    sources_order <- sources_order %>% stringr::str_remove(paste0(cells, "__"))

    data <- data %>%
      tidyr::pivot_longer(tidyselect::matches(paste0(cells, "__")), names_to = "DB2", values_to = "records") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("DB"), ~ stringr::str_remove(.x, paste0(cells, "__")))) %>%
      ggplot2::remove_missing(na.rm = TRUE)

    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(.data$DB1, .data$DB2, fill = .data$records)) +
      ggplot2::theme_minimal()


    if (length(unique(data$facet)) > 1) {
      p <- p +
        ggplot2::facet_wrap(vars(facet), ncol = 1) + ggplot2::theme(aspect.ratio = 1, legend.position = "none") + ggplot2::scale_fill_gradient(low = "white")
      # Removed legends here because they do not appear in correct order - maybe worth fixing in the future
      facets <- unique(data$facet)

      for (i in seq_along(facets)) {
        p <- p + ggnewscale::new_scale_fill() + ggplot2::geom_tile(aes(fill = .data$records), data = data %>% dplyr::filter(.data$facet == facets[i])) +

          ggplot2::scale_fill_gradient(low = "white")
      }
    } else {
      p <- p +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "white")
    }

    p +
      ggplot2::geom_text(ggplot2::aes(label = .data$records)) +
      ggplot2::scale_x_discrete(limits = rev(sources_order), guide = ggplot2::guide_axis(angle = 45)) +
      ggplot2::scale_y_discrete(limits = sources_order) +
      ggplot2::labs(x = "", y = "", fill = "Records")
  } else if (plot_type == "percentages") {
    
    
    data <- purrr::map(data, function(df) {
      diag <- diag(as.matrix(df))
      df <- df / diag
      subs <- diag(nrow = nrow(df)) %>%
        as.logical() %>%
        matrix(nrow = nrow(df))
      df[subs] <- diag
      df
    })

    data <- purrr::map_dfr(data, .id = "facet", ~.x)

    data[["DB1"]] <- rep(sources_order, length(unique(data$facet)))

    sources_order <- sources_order %>% stringr::str_remove(paste0(cells, "__"))

    fmt_pct <- function(x) {
      out <- round(x, 2) * 100
      out[x > 0 & x < .01] <- "< 1" # Only true 0 to be shown as 0
      out[TRUE] <- paste0(out, "%")
      out[x > 1] <- x[x > 1] # Return numbers >1 untransformed (for diagonal)
      out
    }

    p <- data %>%
      tidyr::pivot_longer(tidyselect::matches(paste0(cells, "__")), names_to = "DB2", values_to = "shares") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("DB"), ~ stringr::str_remove(.x, paste0(cells, "__")))) %>%
      ggplot2::ggplot(ggplot2::aes(.data$DB1, .data$DB2, fill = .data$shares)) +
      ggplot2::geom_tile(height = .9) +
      ggplot2::scale_fill_gradient(low = "white", labels = scales::percent, limits = c(0, 1)) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$shares), fill = NULL)) +
      ggplot2::labs(
        x = "", y = "", fill = "Overlap",
        caption = "Note: Percentages indicate share of records in row also found in column, 
       number of results in each database is shown on the diagonal"
      ) +
      ggplot2::scale_x_discrete(limits = rev(sources_order), guide = ggplot2::guide_axis(angle = 45)) +
      ggplot2::scale_y_discrete(limits = sources_order) +
      ggplot2::theme_light()



    if (length(unique(data$facet)) > 1) {
      p <- p + ggplot2::facet_wrap(vars(facet), ncol = 1) + ggplot2::theme(aspect.ratio = 1)
    }
    p
  }
}

#' Create an UpSetR upset plot showing the overlap between sources
#'
#' Show records found in specific sets of sources to identify the unique contribution
#' of each source and of any subsets
#'
#' @param data A tibble with one record per row, an id column and then one column
#' per source indicating whether the record was found in that source.
#' @param groups Variable to use as groups. Should be 'source', 'label' or 'string' - defaults to source.
#' @inheritParams UpSetR::upset
#' @inheritDotParams UpSetR::upset -sets.x.label -mainbar.y.label -order.by
#' @export
#' @references Conway, J. R., Lex, A., & Gehlenborg, N. (2017). UpSetR: an R package for the visualization of intersecting sets and their properties. Bioinformatics.

#' @examples
#' data <- data.frame(
#'   article_id = 1:500,
#'   source__source1 = rbinom(500, 1, .5) == 1,
#'   source__source2 = rbinom(500, 1, .2) == 1,
#'   source__source3 = rbinom(500, 1, .1) == 1,
#'   source__source4 = rbinom(500, 1, .6) == 1,
#'   source__source5 = rbinom(500, 1, .7) == 1
#' )
#'
#' plot_source_overlap_upset(data)
#'
#' # To start with the records shared among the greatest number of sources, use
#'
#' plot_source_overlap_upset(data, decreasing = c(TRUE, TRUE))
#'
plot_source_overlap_upset <- function(data, groups = "source", nsets = NULL, sets.x.label = "Number of records",
                                      mainbar.y.label = "Overlapping record count", order.by = c("freq", "degree"), ...) {
  data <- data %>%
    dplyr::select(tidyselect::matches(paste0(groups, "__"))) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, paste0(groups, "__")))

  if (is.null(nsets)) {
    nsets <- ncol(data)
  }

  if (nsets > 5) message("Plotting a large number of groups. Consider reducing nset or sub-setting the data.")

  data %>% data.frame() %>% 
    dplyr::transmute(dplyr::across(tidyselect::vars_select_helpers$where(is.logical), as.numeric)) %>%
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
#' @param bar_order Character. Order of bars within each facet, any levels not specified will follow at the end. If "keep", then this is based on factor levels (or the first value) in the input data.
#' @param facet_order Character. Order of facets. Any levels not specified will follow at the end.
#' @param color_order Character. Order of values on the color scale.
#' @export
#' @examples
#' data <- data.frame(
#'   article_id = 1:100,
#'   cite_source = sample(c("DB 1", "DB 2", "DB 3"), 100, replace = TRUE),
#'   cite_label = sample(c("2020", "2021", "2022"), 100, replace = TRUE),
#'   type = c("unique", "duplicated")[rbinom(100, 1, .7) + 1]
#' )
#'
#' plot_contributions(data, center = TRUE, bar_order = c("2022", "2021", "2020"), color_order = c("unique", "duplicated"))
#'
plot_contributions <- function(data, facets = cite_source, bars = cite_label, color = type, center = FALSE, bar_order = "keep", facet_order = "keep", color_order = "keep") {
  facets <- rlang::enquo(facets)
  bars <- rlang::enquo(bars)
  color <- rlang::enquo(color)

  if (!rlang::as_name(facets) %in% colnames(data)) stop("Column ", rlang::as_name(facets), " not found in data.")
  if (!rlang::as_name(bars) %in% colnames(data)) stop("Column ", rlang::as_name(bars), " not found in data.")
  if (!rlang::as_name(color) %in% colnames(data)) stop("Column ", rlang::as_name(color), " not found in data.")




  if (!(length(color_order) == 1 && color_order == "keep")) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!color, ~ forcats::fct_relevel(.x, color_order)))
  }

  if (!(length(bar_order) == 1 && bar_order == "keep")) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!bars, ~ forcats::fct_relevel(.x, bar_order)))
  }

  if (!(length(facet_order) == 1 && facet_order == "keep")) {
    data <- data %>%
      dplyr::mutate(dplyr::across(!!facets, ~ forcats::fct_relevel(.x, facet_order)))
  }

  if (!center) {
    ggplot2::ggplot(data, ggplot2::aes(!!bars, fill = !!color)) +
      ggplot2::geom_bar() +
      ggplot2::facet_grid(ggplot2::vars(!!facets)) +
      ggplot2::labs(y = "Citations")
  } else {
    vals <- levels(data %>% dplyr::select(!!color) %>% dplyr::pull() %>% forcats::as_factor())

    if (length(vals) != 2) stop("center is only implemented for two colors, call the function with center = FALSE")

    data_sum <- data %>%
      dplyr::group_by(!!bars, !!facets, !!color) %>%
      dplyr::summarise(n = dplyr::n())

    data_sum$labelpos <- ifelse(data_sum$type == vals[1],
      .5 * data_sum$n, -.5 * data_sum$n
    ) # add label positions for geom_text

    ggplot2::ggplot(data, ggplot2::aes(!!bars, fill = !!color)) +
      ggplot2::geom_bar(data = data_sum %>% dplyr::filter(!!color != vals[1]), ggplot2::aes(y = -.data$n), stat = "identity") +
      ggplot2::geom_bar(data = data_sum %>% dplyr::filter(!!color == vals[1]), ggplot2::aes(y = .data$n), stat = "identity") +
      ggplot2::facet_grid(cols = ggplot2::vars(!!facets)) +
      ggplot2::labs(y = "Citations") +
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::guides(x = ggplot2::guide_axis(angle = 45), fill = ggplot2::guide_legend(reverse = TRUE)) + # make legend ordering the same as plot ordering
      ggplot2::geom_text(data = data_sum, ggplot2::aes(label = paste0(.data$n), y = .data$labelpos), size = 3.5)
  }
}
