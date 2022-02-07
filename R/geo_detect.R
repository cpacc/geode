#' Detect regions that form an unusual spatial aggregation of events.
#'
#' This function identifies clusters in data organized and aggregated into regions.  The
#' Kulldorff cluster detection method is used, which creates groupings of regions by
#' consecutively aggregating nearest-neighbour areas until a set proportion of the total
#' population is reached. The number of cases or events in the aggregation, compared to the
#' expected count (as calculated or user-specified) is used to calculate the likelihood based
#' on either a binomial or Poisson model. The output is the group of neighbouring regions that
#' form the *most likely cluster* and, any secondary clusters.  Optionally a map of the
#' clusters can be generated.
#'
#' @param data Name of input data for analysis. REQUIRED.
#' @param counts Name of the data column that contains the observed counts that will be tested
#' for clustering. REQUIRED.
#' @param pop Name of data column that contains population sizes to be used for each region. For
#' a cluster analysis, all regions must have a population size > 0. REQUIRED
#' @param expected_counts Name of the data column that contains the expected counts for each
#' region. Only used when method = "kulldorff_poisson" is specified. If no data column is provided,
#' the expected count for each region is calculated automatically as proportionate share of the total
#' counts based on population size. OPTIONAL.
#' @param method Statistical method to be used for cluster detection.  Available methods are
#' "kulldorff_binomial" (default) and "kulldorff_poisson". OPTIONAL.
#' @param plot Should an output plot be created showing clustered regions?  Default is TRUE.
#' If FALSE, output returns a listing of the regions included in the most likely cluster and the
#' top two secondary clusters. OPTIONAL.
#' @param legend_title Title of plot legend. If no value is given, 'Cluster' is used as
#' the default title. OPTIONAL.
#' @param plot_title Main title of plot. If omitted, no title is shown. Only applies to static
#' image maps. OPTIONAL.
#' @param interactive TRUE (default) or FALSE.  Indicating whether map should be
#' generated as an interactive view or as a static image. OPTIONAL.
#' @param transparency value between 0 and 1 (default) that defines transparency of map colours,
#' from transparent (0) to opaque (1). Intermediate values allow more or less visibility of underlying
#' (reference) layers in interactive maps. OPTIONAL.
#' @param hover_id Name of the data column containing the label or value to be shown
#' when hovering over a particular geographic unit.  For interactive maps only.
#' Default is the value given in the specified 'counts' column.  OPTIONAL.
#'
#' @return
#' @export
#'
#' @examples
#' # Cluster analysis and map of event rate based on Kulldorff binomial model ----------------
#' geo_detect(data = my_geo_data,
#'   counts = events_data_column,
#'   pop = pop_data_column,
#'   method = "kulldorff_binomial",
#'   plot = TRUE)
#'
#' # Cluster analysis and map of event rate based on Kulldorff Poisson model -----------------
#' geo_detect(data = my_geo_data,
#'   counts = events_data_column,
#'   pop = pop_data_column,
#'   expected_counts = expected_data_column,
#'   method = "kulldorff_poisson",
#'   plot = TRUE)
#'
#' # Cluster analysis and return listing of regions included in top three clusters -----------
#' geo_detect(data = my_geo_data,
#'   counts = events_data_column,
#'   pop = pop_data_column,
#'   method = "kulldorff_binomial",
#'   plot = FALSE)


geo_detect <- function(data,
                       counts,
                       pop,
                       expected_counts = NULL,
                       method = "kulldorff_binomial",
                       plot = TRUE,
                       legend_title = "Clusters",
                       plot_title = NA,
                       transparency = NA,
                       hover_id,
                       interactive = TRUE) {

  # check that mandatory arguments are defined
  if (missing(data) || missing(counts) || missing(pop)) {
    stop("Data object (data), counts and pop are required", call. = FALSE)
  }


  # check that appropriate method is specified
  method_list <- c('kulldorff_binomial', 'kulldorff_poisson')

  if (!method %in% method_list) {
    stop("Method must be one of 'kulldorff_binomial' or 'kulldorff_poisson'", call. = FALSE)
  }


  # check expected counts are included but not needed
  if (method == 'kulldorff_binomial' & !is.null(expected_counts)) {
    warning("Expected counts are not used in the Kulldorff Binomial method", call. = FALSE)
  }

  # check that transparency value (if provided) is between 0 and 1
  if (!is.na(transparency) & (transparency > 1 | transparency < 0) ) {
    stop("transparency value must be between 0 and 1", call. = FALSE)
  }

  # set partial transparency if no value provided
  if (is.na(transparency)) {
    transparency <- 0.6
  }

  # allow user specified data columns to be quoted or bare names
  counts <- as.character(rlang::ensym(counts))
  pop <- as.character(rlang::ensym(pop))


  # hover_id defaults to specified geography column if missing
  if (missing(hover_id)) {
    hover_id <- counts
  } else {
    hover_id <- as.character(rlang::ensym(hover_id))
  }


  # check that specified columns exist in dataset
  if (!any(names(data) == counts)) {
    stop("Specified counts column does not exist", call. = FALSE)
  }

  if (!any(names(data) == pop)) {
    stop("Specified pop column does not exist", call. = FALSE)
  }

  if (!any(names(data) == hover_id)) {
    stop("Specified hover_id column does not exist", call. = FALSE)
  }


  # if (missing(hover_id)) {
  #   hover_id <- deparse(substitute(counts))
  # } else {
  #   hover_id <- deparse(substitute(hover_id))
  # }
  #
  # # check that specified hover_id column exists in dataset
  # if (!hover_id %in% colnames(data)) {
  #   stop("Specified hover_id does not exist", call. = FALSE)
  # }

  # convert analysis variable(s) to numeric (will give warning if non-numeric)
  counts_n <- as.numeric(data[[counts]])
  pop_n <- as.numeric(data[[pop]])



  ## Generate analysis data

  # Use centroid of each region to define location for cluster analysis
  coords <- data %>%
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))) %>%
    dplyr::rename(cent_x = "X", cent_y = "Y") %>%
    dplyr::select(cent_x, cent_y) %>%
    sf::st_drop_geometry()


  ## Calculate expected counts if needed
  if (method == 'kulldorff_poisson' & is.null(expected_counts)) {
    warning("Expected counts have not be specified and will be calculated automatically")

    expected_counts_n <- SpatialEpi::expected(pop_n, counts_n, n.strata = 1)

  }


  if (method == 'kulldorff_poisson' & !is.null(expected_counts)) {

    expected_counts <- as.character(rlang::ensym(expected_counts))

    if (!any(names(data) == expected_counts)) {
      stop("Specified expected_counts column does not exist", call. = FALSE)
    }

    expected_counts_n <- as.numeric(data[[expected_counts]])

  }





  ## run cluster detection algorithm

  # Kulldorff Binomial method (does not use expected event counts)
  if (method == "kulldorff_binomial") {

      kb <- SpatialEpi::kulldorff(geo = coords,
                                      cases = counts_n,
                                      population = pop_n,
                                      expected.cases = NULL,
                                      pop.upper.bound = 0.1,
                                      n.simulations = 999,
                                      alpha.level = 0.05,
                                      plot = FALSE)

      # statistics associated with most likely cluster
      mlc <- kb[["most.likely.cluster"]]
      mlc_regions <- data[kb[["most.likely.cluster"]][["location.IDs.included"]], ]
      mlc_regions <- mlc_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 1)


      if (length(kb[["secondary.clusters"]]) >= 1) {
        # statistics associated with secondary cluster 1
        scl1 <- kb[["secondary.clusters"]][[1]]
        scl1_regions <- data[kb[["secondary.clusters"]][[1]][["location.IDs.included"]], ]
        scl1_regions <- scl1_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 2)
      } else {scl1_regions <- NULL}


      if (length(kb[["secondary.clusters"]]) >= 2) {
        # statistics associated with secondary cluster 2
        scl2 <- kb[["secondary.clusters"]][[2]]
        scl2_regions <- data[kb[["secondary.clusters"]][[2]][["location.IDs.included"]], ]
        scl2_regions <- scl2_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 3)
      } else {scl2_regions <- NULL}


      cluster_regions <- dplyr::bind_rows(mlc_regions, scl1_regions, scl2_regions)

  }


  ## Kulldorff Poisson method (uses expected event counts)
  if (method == "kulldorff_poisson") {

    kp <- SpatialEpi::kulldorff(geo = coords,
                                cases = counts_n,
                                population = pop_n,
                                expected.cases = expected_counts_n,
                                pop.upper.bound = 0.1,
                                n.simulations = 999,
                                alpha.level = 0.05,
                                plot = FALSE)

    # statistics associated with most likely cluster
    mlc <- kp[["most.likely.cluster"]]
    mlc_regions <- data[kp[["most.likely.cluster"]][["location.IDs.included"]], ]
    mlc_regions <- mlc_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 1)

    # statistics associated with secondary cluster 1
    scl1 <- kp[["secondary.clusters"]][[1]]
    scl1_regions <- data[kp[["secondary.clusters"]][[1]][["location.IDs.included"]], ]
    scl1_regions <- scl1_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 2)

    # statistics associated with secondary cluster 2
    scl2 <- kp[["secondary.clusters"]][[2]]
    scl2_regions <- data[kp[["secondary.clusters"]][[2]][["location.IDs.included"]], ]
    scl2_regions <- scl2_regions %>% sf::st_drop_geometry() %>% dplyr::mutate(cluster = 3)

    cluster_regions <- dplyr::bind_rows(mlc_regions, scl1_regions, scl2_regions)

  }


  ## generate map of clustered regions

  if (plot == TRUE) {

    plot <- suppressMessages(dplyr::left_join(data, cluster_regions)) %>%
      dplyr::mutate(cluster = ifelse(is.na(cluster), "Not clustered", cluster))

    outplot <- tmap::tm_shape(plot) +

      tmap::tm_polygons(col = "cluster",
                        alpha = transparency,
                        title = legend_title,
                        style = "pretty",
                        palette = "-YlOrRd",
                        n = 4,
                        id = hover_id,
                        border.alpha = 0.5) +

      tmap::tm_layout(main.title = plot_title,
                      main.title.size = 1.0,
                      main.title.position = "center",
                      frame = FALSE) +

      tmap::tmap_options(show.messages = FALSE,
                         show.warnings = FALSE,
                         check.and.fix = TRUE) +

      tmap::tmap_mode("view")

    # optionally view map in interactive mode
    if (interactive == FALSE) {
      tmap::tmap_mode("plot")
    }

    return(outplot)

  }


  return(cluster_regions)



}
