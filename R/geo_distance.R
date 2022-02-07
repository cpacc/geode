#' Calculate and maps proximity metrics based on known point locations and geographic units.
#'
#' The function examines proximity relative to user-specified point locations.  The input data
#' are i) a set of geographic units (e.g., census regions, health regions, etc.), and ii) the
#' locations of places or events of interest.  The function calculates the centroid of each
#' geographic unit and the pairwise distances to all locations of interest.  Proximity for each
#' geographic unit is calculated as the average distance to the n nearest locations.
#'
#' @param data Geographic input data; must be a simple features (sf) shape object,
#' e.g., imported by geo_import(). REQUIRED.
#' @param geography_col Name of the data column containing the features, e.g., geographic
#' units, to be plotted. REQUIRED.
#' @param location_data Name of the dataset containing point locations to be used in
#' distance calculations. Dataset must contain a column for location name or ID, titled
#' 'id', and a pair of columns for the geographic coordinates of the points (e.g., longitude
#' and latitude), titled 'x' and 'y'. Coordinate reference system (CRS) for these data is
#' automatically transformed to match that of the input geographic data.  If CRS is missing
#' for location data, it is assumed to be WGS84.  REQUIRED.
#' @param points_col Name of the data column containing the label or value uniquely identifying
#' each particular point location. REQUIRED.
#' @param n_nearest number of point locations to use in calculation of proximity (average
#' distance from centroid to n nearest locations of interest).  Default is 3.  If value is
#' set at 1, proximity for each geographic unit will simply be the distance from the centroid
#' to the nearest location. Must be specified as a positive integer value.  OPTIONAL.
#' @param levels Preferred number of shading categories representing proximity for each geographic
#' regions.  Default is 5.  Generates equally sized categories of round numbers, resulting in a
#' number of levels close to, but not necessarily exactly, the specified number of levels. OPTIONAL.
#' @param plot_title Main title of plot.  If omitted, no title is shown. OPTIONAL.
#' @param legend_title Title of plot legend.  If 'none', legend is removed. If no value
#' is given, default title 'mean_distance" is used. OPTIONAL.
#' @param plot TRUE (default) or FALSE, indicating whether or not a plot should be
#' generated. If FALSE, a dataset is returned containing the calculated proximity values
#' for each geographic unit (with new column 'mean_dist').  OPTIONAL.
#'
#' @return
#' @export
#'
#' @examples
#' # Basic example of proximity analysis using default options and outputting a map ---------
#' geo_distance(data = my_geo_data,
#'   geography_col = region_data_column,
#'   location_data = my_point_location_data,
#'   legend_title = "Distance (m)",
#'   legend_position = "right",
#'   plot_title = "Average Distance to Point Locations")
#'
#' # Proximity analysis using nearest 5 point locations and generating data output ----------
#' geo_distance(data = my_geo_data,
#'   geography_col = region_data_column,
#'   location_data = my_point_location_data,
#'   n_nearest = 5,
#'   plot = FALSE)


geo_distance <- function(data,
                         geography_col,
                         location_data,
                         points_col,
                         n_nearest = 3,
                         levels = 5,
                         plot_title = NA,
                         legend_title = NA,
                         plot = TRUE) {


  # check that mandatory arguments are defined
  if (missing(data) || missing(geography_col) || missing(location_data) || missing(points_col)) {
    stop("Data, geography_col, location_data and points_col are required", call. = FALSE)
  }

  # check that input data is an sf object
  if (!any(class(data) == 'sf')) {
    stop("Input data must be a simple features (sf) shape object.
         Import data first using geo_import()", call. = FALSE)
  }

  # allow user specified data columns to be quoted or bare names
  geography_col <- as.character(rlang::ensym(geography_col))
  points_col <- as.character(rlang::ensym(points_col))

  # check that specified geography column exists in dataset
  if (!any(names(data) == geography_col)) {
    stop("Specified geography_col does not exist", call. = FALSE)
  }

  # check that specified points_col exists in dataset
  if (!any(names(location_data) == points_col)) {
    stop("Specified points_col does not exist", call. = FALSE)
  }

  # check that location data has required columns
  if (! ('x' %in% names(location_data) & 'y' %in% names(location_data))) {
    stop("location_data must contain the columns 'x' and 'y'", call. = FALSE)
  }

  # check that n_nearest is proper numeric value
  if (!is.numeric(n_nearest) || !(n_nearest >= 1) || !(as.integer(n_nearest) == n_nearest)) {
    stop("n_nearest must be specified as a positive integer value", call. = FALSE)
  }



  # if point location data does not have CRS, set to be WGS84 (EPSG: 4326)
  location_data <- location_data %>% dplyr::select(points_col, x, y)

  if (is.na(sf::st_crs(location_data))) {
    location_data <- location_data %>% sf::st_as_sf(coords = c('x', 'y'), crs = 4326)
  } else {
    location_data <- location_data %>% sf::st_as_sf(coords = c('x', 'y'))
  }


  # harmonize CRS for geographic data and location data
  location_data <- sf::st_transform(location_data, crs = sf::st_crs(data))
  location_data <- location_data %>%
    dplyr::bind_cols(tibble::as_tibble(sf::st_coordinates(location_data))) %>%
    dplyr::rename(x = X, y = Y)


  # calculate centroid for each geographic region
  cent <- data %>% sf::st_centroid() %>% dplyr::select(geometry)


  # calculate distance between centroids and location points
  dist <- sf::st_distance(x = cent, y = location_data, by_element = FALSE)
  dist <- data.frame(dist)
  names(dist) <- location_data[[points_col]]

  dist <- dist %>%
    dplyr::mutate(region = data[[geography_col]]) %>%
    tidyr::gather(points_col, "dist", 1:nrow(location_data)) %>%
    dplyr::group_by(region) %>%
    dplyr::slice_min(order_by = dist, n = n_nearest) %>%
    dplyr::mutate(dist = dist/1000)


  # calculate average distance metric
  mean_dist <- dist %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(mean_distance = as.numeric(mean(dist)))


  # join mean dist back to original data
  together <- data %>%
    dplyr::left_join(mean_dist, by = stats::setNames("region", geography_col))



  if (plot == TRUE) {

    out_plot <- tmap::tm_shape(shp = together) +

      tmap::tm_polygons(col = "mean_distance",
                        style = "pretty",
                        n = levels,
                        title = legend_title) +

      tmap::tm_shape(shp = location_data) +
      tmap::tm_dots(col = 'black',
                    size = 0.1,
                    id = points_col,
                    legend.show = FALSE) +

      tmap::tm_legend(outside = TRUE) +

      tmap::tm_layout(main.title = plot_title,
                      main.title.size = 1.0,
                      main.title.position = "center",
                      frame = FALSE) +

      tmap::tmap_options(show.messages = FALSE,
                         show.warnings = FALSE,
                         check.and.fix = TRUE) +

      tmap::tmap_mode("view")


      return(out_plot)

    }


   # out_plot <- ggplot2::ggplot() +
   #
   #   ggplot2::geom_sf(data = together, ggplot2::aes(fill = mean_dist), show.legend = TRUE) +
   #
   #   ggplot2::geom_sf(data = location_data, colour = "red", size = 1) +
   #
   #   ggthemes::theme_map() +
   #
   #   ggplot2::theme(legend.position = legend_position)
   #
   #
   # # optionally specify new legend title
   # if (!missing(legend_title)) {
   #   out_plot <- out_plot + ggplot2::labs(fill = legend_title)
   # }
   #
   # # optionally show plot without legend
   # if (!missing(legend_title) & legend_title == 'none') {
   #   out_plot <- out_plot + ggplot2::theme(legend.position = "none")
   # }
   #
   # # optionally specify plot title (centered)
   # if (!missing(plot_title)) {
   #   out_plot <- out_plot +
   #     ggplot2::ggtitle(plot_title) +
   #     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
   # }


  return(together)


}
