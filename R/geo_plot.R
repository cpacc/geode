#' Create maps from geospatial and attribute data
#'
#' This function creates static or interactive maps (choropleth, pointmap or heatmap)
#' from geospatial and attribute data.
#'
#' @param data Input data for plotting; must be a simple features (sf)
#' shape object, e.g., imported by geo_import(). REQUIRED.
#' @param geography_col Name of the data column containing the feature,
#' e.g., geographic units, population size, etc., to be plotted. REQUIRED.
#' @param attribute_data Name of the dataset containing attributes, e.g., points,
#' to be plotted. Dataset must have a pair of columns for geographic coordinates of
#' the attributes (e.g., lat and lon) titled 'x' and 'y'. Coordinate reference
#' system (CRS) for attribute data is automatically transformed to match that of
#' input geographic data.  If CRS is missing for attribute data, it is assumed to be
#' WGS84. REQUIRED (for pointmaps and heatmaps only).
#' @param plot_type Name of one of the predefined plot types: current options
#' are 'choropleth', 'pointmap' and 'heatmap'. REQUIRED.
#' @param style Name of a defined output style.  User defined output styles can be
#' created following examples shown in the help files for tmap (see ?tmap::tmap_options).
#' For illustrative purposes, the current selection of styles are "zissou", "royal", "darjeeling",
#' "bottlerocket", "moonrise" and "isleofdogs" (from the wesanderson R pacakge); 'viridis" and
#' "magma" (from the viridis R package); and "terrain" (from base R).  If omitted, defaults to tmap
#' "white" style. OPTIONAL.
#' @param transparency value between 0 and 1 (default) that defines transparency of map colours,
#' from transparent (0) to opaque (1). Intermediate values allow more or less visibility of underlying
#' (reference) layers in interactive maps.  Applies only to choropleth maps.  OPTIONAL.
#' @param levels Preferred number of shading categories for choropleth maps when geography_col
#' is a numeric variable. Default is 5.  Generates equally sized categories of round
#' numbers, resulting in a number of levels close to, but not necessarily exactly, the specified
#' number of levels. OPTIONAL.
#' @param hover_id Name of the data column containing the label or value to be shown
#' when hovering over a particular geographic unit.  For interactive choropleth maps only.
#' Default is the name or value given in specified geography_col.  OPTIONAL.
#' @param plot_title Main title of plot.  If omitted, no title is shown. Applies only to static
#' image maps. OPTIONAL.
#' @param legend_title Title of plot legend.  If 'none', legend is removed.
#' If no value is given, feature_col name is used. OPTIONAL.
#' @param scale_bar TRUE or FALSE (default).  Indicating whether reference scale
#' bar should be shown (bottom left of plot). Applies only to static image maps. OPTIONAL.
#' @param compass TRUE or FALSE (default).  Indicating whether reference compass
#' should be shown (top right of plot). Applies only to static image maps. OPTIONAL.
#' @param interactive TRUE or FALSE (default).  Indicating whether map should be
#' generated as an interactive view or as a static image. OPTIONAL.
#'
#' @return
#' @export
#'
#' @examples
#'# simple static choropleth map with plot title and legend title ------------------
#'geo_plot(data = my_geo_data,
#'  geography_col = region_data_column,
#'  plot_type = 'choropleth',
#'  plot_title = 'My Geographic Regions',
#'  legend_title = 'Region Name')
#'
#'# interactive choropleth map with partial transparency of shaded regions ---------
#'geo_plot(data = my_geo_data,
#'  geography_col = region_data_column,
#'  plot_type = 'choropleth',
#'  transparency = 0.5,
#'  hover_id = region_popsize_column,
#'  interactive = TRUE)
#'
#'# static pointmap with plot title but no legend ----------------------------------
#'geo_plot(data = my_geo_data,
#'  geography_col = region_data_column,
#'  attribute_data = my_point_location_data,
#'  plot_type = 'pointmap',
#'  plot_title = 'My Point Locations',
#'  legend_title = 'none')

geo_plot <- function(data,
                     geography_col,
                     attribute_data = NA,
                     plot_type,
                     style = NA,
                     transparency = NA,
                     levels = 5,
                     hover_id,
                     plot_title = NA,
                     legend_title = NA,
                     scale_bar = FALSE,
                     compass = FALSE,
                     interactive = FALSE) {


  # check that mandatory arguments are defined
  if (missing(data) || missing(geography_col) || missing(plot_type)) {
    stop("Data, geography_col, and plot_type are required", call. = FALSE)
  }


  # check that input data is an sf object
  if (!any(class(data) == 'sf')) {
    stop("Input data must be a simple features (sf) shape object.
         Import data first using geo_import()", call. = FALSE)
  }

  # check that plot_type is a valid choice
  if (!plot_type %in% c('choropleth', 'pointmap', 'heatmap')) {
    stop("plot_type must be one of 'choropleth', 'pointmap' or 'heatmap'", call. = FALSE)
  }

  # check that attribute_data is specified if plot_type is pointmap or heatmap
  if (plot_type %in% c('pointmap', 'heatmap') && is.na(attribute_data) ) {
    stop("attribute_data must be specified if plot_type is 'pointmap' or 'heatmap'", call. = FALSE)
  }

  # check that transparency value (if provided) is between 0 and 1
  if (!is.na(transparency) & (transparency > 1 | transparency < 0) ) {
    stop("transparency value must be between 0 and 1", call. = FALSE)
  }


  if (missing(hover_id)) {
    hover_id <- deparse(substitute(geography_col))
  } else {
    hover_id <- deparse(substitute(hover_id))
  }

  geography_col <- deparse(substitute(geography_col))

  # check that specified geography column exists in dataset
  if (!geography_col %in% colnames(data)) {
    stop("Specified geography_col does not exist", call. = FALSE)
  }

  # check that specified hover_id column exists in dataset
  if (!hover_id %in% colnames(data)) {
    stop("Specified hover_id does not exist", call. = FALSE)
  }





  # set mapping transparency to opaque if value not provided
  if (is.na(transparency)) {
    transparency <- 1.0
    }

  n_geo_regions <- dplyr::n_distinct(data[[geography_col]])


  # if for pointmap or heatmap, transform CRS of attribute data
  if (plot_type %in% c('pointmap', 'heatmap')) {
    attribute_data <- attribute_data %>% dplyr::select(id, x, y)

    if (is.na(sf::st_crs(attribute_data))) {
      attribute_data <- attribute_data %>% sf::st_as_sf(coords = c('x', 'y'), crs = 4326)
    } else {
      attribute_data <- attribute_data %>% sf::st_as_sf(coords = c('x', 'y'))
    }

    attribute_data <- sf::st_transform(attribute_data, crs = sf::st_crs(data))
    attribute_data <- attribute_data %>%
      dplyr::bind_cols(tibble::as_tibble(sf::st_coordinates(attribute_data))) %>%
      dplyr::rename(x = X, y = Y)

  }

  # define colour styles for choropleth maps

  zissou <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey50",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = wesanderson::wes_palette("Zissou1", 21, type = 'continuous'),
                         div = wesanderson::wes_palette("Zissou1", 21, type = 'continuous'),
                         cat = wesanderson::wes_palette("Zissou1", 21, type = 'continuous')),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "zissou"
  )

  moonrise <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = rev(wesanderson::wes_palette("Moonrise1", 21, type = 'continuous')),
                         div = rev(wesanderson::wes_palette("Moonrise1", 21, type = 'continuous')),
                         cat = rev(wesanderson::wes_palette("Moonrise1", 21, type = 'continuous'))),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "moonrise"
  )

  isleofdogs <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = wesanderson::wes_palette("IsleofDogs2", 21, type = 'continuous'),
                         div = wesanderson::wes_palette("IsleofDogs2", 21, type = 'continuous'),
                         cat = wesanderson::wes_palette("IsleofDogs2", 21, type = 'continuous')),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "isleofdogs"
  )

  royal <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = wesanderson::wes_palette("Royal2", 21, type = 'continuous'),
                         div = wesanderson::wes_palette("Royal2", 21, type = 'continuous'),
                         cat = wesanderson::wes_palette("Royal2", 21, type = 'continuous')),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "royal"
  )

  darjeeling <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = rev(wesanderson::wes_palette("Darjeeling2", 21, type = 'continuous')),
                         div = rev(wesanderson::wes_palette("Darjeeling2", 21, type = 'continuous')),
                         cat = rev(wesanderson::wes_palette("Darjeeling2", 21, type = 'continuous'))),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "darjeeling"
  )

  bottlerocket <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = rev(wesanderson::wes_palette("BottleRocket2", 21, type = 'continuous')),
                         div = rev(wesanderson::wes_palette("BottleRocket2", 21, type = 'continuous')),
                         cat = rev(wesanderson::wes_palette("BottleRocket2", 21, type = 'continuous'))),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "bottlerocket"
  )

  viridis <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = viridis::viridis(n = 21),
                         div = viridis::viridis(n = 21),
                         cat = viridis::viridis(n = 21)),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "viridis"
  )

  magma <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = viridis::magma(n = 21),
                         div = viridis::magma(n = 21),
                         cat = viridis::magma(n = 21)),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "magma"
  )

  terrain <- structure(
    list(
      bg.color = "white",
      aes.color = c(fill = "grey40", borders = "grey40",
                    symbols = "grey80", dots = "grey80",
                    lines = "white", text = "white",
                    na = "grey30", null = "grey15"),
      aes.palette = list(seq = terrain.colors(n = 21),
                         div = terrain.colors(n = 21),
                         cat = terrain.colors(n = 21)),
      attr.color = "black",
      panel.label.color = "black",
      panel.label.bg.color = "grey40",
      main.title.color = "black",
      show.messages = FALSE
    ),
    style = "terrain"
  )



  if (plot_type == 'choropleth') {

    #tmap::tmap_options_reset() # reset all options before every plot

    out_plot <- tmap::tm_shape(shp = data) +

      tmap::tm_polygons(col = geography_col,
                        alpha = transparency,
                        style = "pretty",
                        n = levels,
                        id = hover_id,
                        title = legend_title) +

      tmap::tm_legend(outside = TRUE) +

      tmap::tm_layout(main.title = plot_title,
                main.title.size = 1.0,
                main.title.position = "center",
                frame = FALSE) +

      tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE) +

      tmap::tmap_mode("plot")


    # optionally show plot with user defined style
    if (missing(style)|is.na(style)) {
      out_plot <- out_plot + tmap::tmap_style("white") +
        tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE)
      } else if (style == 'zissou') {
        out_plot <- out_plot + tmap::tmap_options(zissou)
      } else if (style == 'royal') {
        out_plot <- out_plot + tmap::tmap_options(royal)
      } else if (style == 'darjeeling') {
        out_plot <- out_plot + tmap::tmap_options(darjeeling)
      } else if (style == 'bottlerocket') {
        out_plot <- out_plot + tmap::tmap_options(bottlerocket)
      } else if (style == 'moonrise') {
        out_plot <- out_plot + tmap::tmap_options(moonrise)
      } else if (style == 'isleofdogs') {
        out_plot <- out_plot + tmap::tmap_options(isleofdogs)
      } else if (style == 'viridis') {
        out_plot <- out_plot + tmap::tmap_options(viridis)
      } else if (style == 'magma') {
        out_plot <- out_plot + tmap::tmap_options(magma)
      } else if (style == 'terrain') {
        out_plot <- out_plot + tmap::tmap_options(terrain)
      }

    # optionally show plot without legend
    if (!missing(legend_title) & legend_title == 'none') {
      out_plot <- out_plot + tmap::tm_legend(show = FALSE)
    }

    # optionally show plot with scale bar
    if (scale_bar == TRUE) {
      out_plot <- out_plot + tmap::tm_scale_bar(position = c("left", "bottom"))
    }

    # optionally show plot with compass
    if (compass == TRUE) {
      out_plot <- out_plot + tmap::tm_compass(type = "4star",
                                   position = c("right", "top"),
                                   size = 1)
    }

    # optionally view map in interactive mode
    if (interactive == TRUE) {
      tmap::tmap_mode("view")
    }

    return(out_plot)

  }


  # define colour styles for pointmaps
  pretty_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_geo_regions)
  zissou_palette <- wesanderson::wes_palette(name = "Zissou1", type = "continuous", n = n_geo_regions)
  royal_palette <- wesanderson::wes_palette(name = "Royal2", type = "continuous", n = n_geo_regions)
  darjeeling_palette <- wesanderson::wes_palette(name = "Darjeeling1", type = "continuous", n = n_geo_regions)
  fantasticfox_palette <- wesanderson::wes_palette(name = "FantasticFox1", type = "continuous", n = n_geo_regions)


  if (plot_type == 'pointmap') {

    out_plot <- ggplot2::ggplot() +

      ggplot2::geom_sf(data = data, ggplot2::aes(fill = .data[[geography_col]]),
                       show.legend = TRUE) +

      ggplot2::geom_point(data = attribute_data, ggplot2::aes(x = x, y = y),
                          colour = 'black', size = 1.1) +


      ggthemes::theme_map()


    # optionally define colour styles
    if (is.na(style)) {
      out_plot <- out_plot + ggplot2::scale_fill_manual(values = pretty_palette)
    } else if (style == 'zissou') {
      out_plot <- out_plot + ggplot2::scale_fill_manual(values = zissou_palette)
    } else if (style == 'royal') {
      out_plot <- out_plot + ggplot2::scale_fill_manual(values = royal_palette)
    } else if (style == 'darjeeling') {
      out_plot <- out_plot + ggplot2::scale_fill_manual(values = darjeeling_palette)
    } else if (style == 'fantasticfox') {
      out_plot <- out_plot + ggplot2::scale_fill_manual(values = fantasticfox_palette)
    }


    # optionally specify new legend title
    if (!missing(legend_title)) {
      out_plot <- out_plot + ggplot2::labs(fill = legend_title)
    }

    # optionally show plot without legend
    if (!missing(legend_title) & legend_title == 'none') {
      out_plot <- out_plot + ggplot2::theme(legend.position = "none")
    }

    # optionally specify plot title (centered)
    if (!missing(plot_title)) {
      out_plot <- out_plot +
        ggplot2::ggtitle(plot_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }


    return(out_plot)

  }



  if (plot_type == 'heatmap') {

    out_plot <- ggplot2::ggplot() +

      ggplot2::geom_sf(data = data, fill = "white", show.legend = TRUE) +

      ggplot2::stat_density2d(data = attribute_data, ggplot2::aes(x = x, y = y,
               fill = ..level.., alpha = ..level..),
               size = 0.01, bins = 16, geom = "polygon") +

      ggplot2::scale_fill_gradient(low = "green", high = "red") +

      ggplot2::scale_alpha(range = c(0.00, 0.3), guide = FALSE) +

      ggthemes::theme_map()



    # optionally specify new legend title
    if (!missing(legend_title)) {
      out_plot <- out_plot + ggplot2::labs(fill = legend_title)
    }

    # optionally show plot without legend
    if (!missing(legend_title) & legend_title == 'none') {
      out_plot <- out_plot + ggplot2::theme(legend.position = "none")
    }

    # optionally specify plot title
    if (!missing(plot_title)) {
      out_plot <- out_plot +
        ggplot2::ggtitle(plot_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }


    return(out_plot)

  }






}





