#' Calculate and plot spatial statistics from geospatial and attribute data
#'
#' Given spatial input data, this function calculates and outputs spatial
#' statistics and, optionally, a plot or map.  Currently, the function will
#' calculate the global (Moran's I) or local (Getis-Ord Gi*) spatial autocorrelation
#' of a single spatial variable or rate.  Rates are calculated automatically
#' if a denominator variable is specified.  Optional plots are Moran's I scatterplot
#' for global spatial autocorrelation and a hotspot/coldspot map showing locations
#' of significant local spatial autocorrelation.
#'
#' @param data Input data for plotting; must be a simple features (sf)
#' shape object, e.g., imported by geo_import(). REQUIRED.
#' @param var Column name of (numeric) variable to be analyzed. For calculation
#' of rates, specify numerator with var and denominator with denom. REQUIRED.
#' @param statistic Name of analysis to run on variable specified by 'var' (or
#' rate specified by var/denom).  Current options include 'global_ac' (global
#' autocorrelation using Moran's I) or local_ac' (local autocorrelation using
#' Getis-Ord Gi*). REQUIRED.
#' @param denom Column name of (numeric) variable to be used as denominator
#' in the calculation of raw rates. If included, numerator is assumed to be variable
#' specified by var argument and rates are calculated and analyzed automatically.
#' OPTIONAL.
#' @param create_plot TRUE (default) or FALSE.  Indicating whether or not
#' plot/map should be generated with statistical output. OPTIONAL.
#' @param plot_title Main title of plot. If omitted, no title is shown. Applies
#' only to static image maps. OPTIONAL.
#' @param transparency value between 0 and 1 (default = 0.6) that defines transparency
#' of map colours, from transparent (0) to opaque (1). Intermediate values allow more or
#' less visibility of underlying (reference) layers in interactive maps. OPTIONAL.
#' @param hover_id Name of the data column containing the label or value to be shown
#' when hovering over a particular geographic unit. For interactive maps only.  Default
#' is the name or value given in specified var column (or the rate, if denom is also
#' specified). OPTIONAL.
#' @param interactive TRUE (default) or FALSE.  Indicating whether map should be
#' generated as an interactive view or as a static image. OPTIONAL.

#'
#' @return
#' @export
#'
#' @examples
#'# Test for global spatial autocorrelation and generation of Moran's I scatterplot -----
#' geo_calculate(data = my_geo_data,
#'    var = rate_column_name,
#'    statistic = 'global_ac',
#'    create_plot = TRUE)
#'
#'# Calculate local spatial autocorrelation and generate hotspot-coldspot map -----
#'# (Event rates are calculated automatically as numerator (var) / denominator (denom))
#' geo_calculate(data = my_geo_data,
#'    var = event_count_column,
#'    denom = pop_size_column,
#'    statistic = 'local_ac',
#'    create_plot = TRUE)


geo_calculate <- function(data,
                          var,
                          statistic,
                          denom,
                          create_plot = TRUE,
                          plot_title = NA,
                          transparency = NA,
                          hover_id,
                          interactive = TRUE) {



  # check that mandatory arguments are defined
  if (missing(data) || missing(statistic) || missing(var)) {
    stop("Data, var and statistic are required", call. = FALSE)
  }

  # check that statistics argument is valid
  if (!statistic %in% c('local_ac', 'global_ac')) {
    stop("statistic argument must be either 'local_ac' or 'global_ac'", call. = FALSE)
  }

  # check that specified var column exists in dataset
  if (!deparse(substitute(var)) %in% colnames(data)) {
    stop("Specified var column does not exist", call. = FALSE)
  }

  # check that specified denom column exists in dataset
  if (!missing(denom) & !deparse(substitute(denom)) %in% colnames(data)) {
    stop("Specified denom column does not exist", call. = FALSE)
  }

  # check that transparency value (if provided) is between 0 and 1
  if (!is.na(transparency) & (transparency > 1 | transparency < 0) ) {
    stop("transparency value must be between 0 and 1", call. = FALSE)
  }

  # set to partial transparency if value not provided
  if (is.na(transparency)) {
    transparency <- 0.6
  }

  if (missing(hover_id)) {
    hover_id <- deparse(substitute(var))
  } else {
    hover_id <- deparse(substitute(hover_id))
  }

  # check that specified hover_id column exists in dataset
  if (!hover_id %in% colnames(data)) {
    stop("Specified hover_id does not exist", call. = FALSE)
  }

  # check that input data is an sf object
  if (!any(class(data) == 'sf')) {
    stop("Input data must be a simple features (sf) shape object.
         Import data first using geo_import()", call. = FALSE)
  }


  # convert analysis variable(s) to numeric (will give warning if non-numeric)
  var <- deparse(substitute(var))
  varn <- as.numeric(data[[var]])

  if (missing(denom)) {

    data[[var]] <- varn

  } else {

    denom <- deparse(substitute(denom))
    denomn <- as.numeric(data[[denom]])
    data[[var]] <- varn / denomn

  }


  if (statistic == 'global_ac') {

    # convert the sf data object to class sp for calculating autocorrelation
    data_sp <- sf::as_Spatial(from = data)

    # define spatial neighbours using queen adjacency rules
    data_sp_nb <- spdep::poly2nb(data_sp, queen = TRUE)

    # define spatial weights for neighbouring locations, with weights summing to 1.0
    # and ignoring locations with no neighbours
    wts <- spdep::nb2listw(data_sp_nb, style="W", zero.policy = TRUE)

    # Examine spatial lag in user specified variable
    sp_lag <- spdep::lag.listw(wts, data_sp[[var]])

    # Calculate global spatial autocorrelation (Moran's I) #
    out <- spdep::moran.mc(data_sp[[var]], wts, zero.policy = TRUE, nsim = 9999)

    stat_out <- tibble::tibble(statistic = out$method, simulations =  length(out$res),
              test_statistic = out$statistic, p_value = out$p.value)

    if (create_plot == TRUE) {

      # visualize global autocorrelation statistic using the Moran's I scatterplot
      spdep::moran.plot(data_sp[[var]], wts, zero.policy = TRUE,
                        xlab = "my x axis", ylab = "my y axis")

    }

  }

  if (statistic == 'local_ac') {

    # convert the sf data object to class sp for calculating autocorrelation
    data_sp <- sf::as_Spatial(from = data)

    # define spatial neighbours using queen adjacency rules
    data_sp_nb <- spdep::poly2nb(data_sp, queen = TRUE)

    # include each region in its set of neighbouring regions
    sp_nb_self <- spdep::include.self(data_sp_nb)

    # generate the spatial weights matrix
    nb_self_wt <- spdep::nb2listw(sp_nb_self, style = "W", zero.policy = TRUE)

    # calculate local spatial autocorrelation statistic (Getis-Ord Gi*)
    localg <- spdep::localG(data_sp[[var]], nb_self_wt)

    # add calculated Gi* statistic into orginal dataset for further analysis
    data_g <- data %>%
      dplyr::mutate(localg = as.numeric(localg))

    # define breaks points in Gi* for z-scores levels of significance (99%, 95%, 90% CIs)
    breaks <- c(min(data_g$localg), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, max(data_g$localg))

    stat_out <- data_g %>%
      dplyr::mutate(cluster = cut(localg, breaks = breaks, include.lowest = TRUE,
                              labels=c("Cold spot: 99% CI", "Cold spot: 95% CI",
                                    "Cold spot: 90% CI", "Not significant",
                                    "Hot spot: 90% CI", "Hot spot: 95% CI",
                                    "Hot spot:  99% CI")))

    # define plot style
    zissou <- structure(
      list(
        bg.color = "white",
        aes.color = c(fill = "grey40", borders = "grey50",
                      symbols = "grey80", dots = "grey80",
                      lines = "white", text = "white",
                      na = "grey30", null = "grey15"),
        aes.palette = list(seq = wesanderson::wes_palette("Zissou1", 5, type = 'continuous'),
                           div = wesanderson::wes_palette("Zissou1", 5, type = 'continuous'),
                           cat = wesanderson::wes_palette("Zissou1", 5, type = 'continuous')),
        attr.color = "black",
        panel.label.color = "black",
        panel.label.bg.color = "grey40",
        main.title.color = "black",
        show.messages = FALSE
      ),
      style = "zissou"
    )


    if (create_plot == TRUE) {

      # generate a map of calculated hot and cold spots

      local_g_plot <- tmap::tm_shape(shp = stat_out) +

        tmap::tm_polygons(col = "cluster",
                          alpha = transparency,
                          style = "pretty",
                          title = '',
                          id = hover_id) +

        tmap::tm_legend(outside = TRUE) +

        tmap::tm_layout(main.title = plot_title,
                        main.title.size = 1.0,
                        main.title.position = "center",
                        frame = FALSE) +

        tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE) +

        tmap::tmap_options(zissou) +

        tmap::tmap_mode("plot")

      # optionally view map in interactive mode
      if (interactive == TRUE) {
        tmap::tmap_mode("view")
      }

      return(local_g_plot)

    }

  }

  return(stat_out)

}
