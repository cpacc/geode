#' Import spatial or attribute data from an existing file
#'
#' This function imports spatial or attribute data from an existing source file.
#' Spatial files must be valid shapefiles (.shp), whereas attribute files can be
#' any georeferenced data from a standard csv file.  Spatial and attribute data
#' can be imported separately and then linked based on matching geographic units.
#' The source files must be local (e.g., on your computer) or reside at an
#' accessible network location.
#'
#' @param path Filepath specifying location of source file.
#' REQUIRED.
#' @param filetype Name of file type: must be either 'spatial' (.shp file) or
#' 'attribute' (.csv file). REQUIRED.
#' @param simplify TRUE or FALSE (default). If importing a shapefile, optionally create
#' a simplified dataset by removing vertices; useful for creating smaller, more manageable
#' working versions of large shapefiles. OPTIONAL.
#' @param validity_check TRUE or FALSE (default). If importing a shapefile, optionally
#' runs a validity check on each of the geometries and prints the results.  Geometries
#' may be invalid due to, for example, slivers or self-intersections; such issues may
#' be present in the imported source file, or arise during simplification. OPTIONAL.
#'
#' @return Returns a simple features (if source is a shapefile) or tibble
#' (if source is a csv file) data object
#'
#' @export
#'
#' @examples
#' # Import spatial data (shapefile) ------------------------------------------
#' geo_import(path = "C:/Users/Michael/Documents/my_shapefile.shp",
#'            filetype = 'spatial')
#'
#' # Import attribute data (csv file) -----------------------------------------
#' geo_import(path = "C:/Users/Michael/Documents/my_attribute_data.csv",
#'            filetype = 'attribute')
#'
#' # Optionally simplify imported spatial data and run validity check ---------
#' geo_import(path = "C:/Users/Michael/Documents/my_shapefile.shp",
#'            filetype = 'spatial',
#'            simplify = TRUE,
#'            validity_check = TRUE)


geo_import <- function(path,
                       filetype,
                       simplify = FALSE,
                       validity_check = FALSE) {

  # check that mandatory arguments are defined
  if (missing(path) || missing(filetype)) {
    stop("Both path and filetype are required", call. = FALSE)
  }

  # check that simplify is only used for spatial files
  if (simplify == TRUE & !filetype == 'spatial') {
    stop("Only 'spatial' filetypes can be simplified", call. = FALSE)
  }

  # check that validity_check is only used for spatial files
  if (validity_check == TRUE & !filetype == 'spatial') {
    stop("Validity check can only be used for 'spatial' filetypes", call. = FALSE)
  }

  # check that source file exists
  if (file.exists(path) == FALSE) {
    stop("Source file does not exist", call. = FALSE)
  }

  if (filetype == 'spatial') {

    # define path to source file
    dsn <- fs::path(path)

    # check that spatial source is .shp file
    if (!fs::path_ext(path) == 'shp') {
      stop("Spatial source must be a shapefile (.shp)", call. = FALSE)
    }

    # read in spatial file
    indat <- sf::st_read(dsn = dsn,
                         stringsAsFactors = FALSE,
                         quiet = TRUE)

    # optionally create simplified version of shapefile
    if (simplify == TRUE) {
      indat <- sf::st_simplify(x = indat,
                               preserveTopology = TRUE,
                               dTolerance = 1000)
    }

    # optionally perform validity check on shapefile
    if (validity_check == TRUE) {
      validity_check <- sf::st_is_valid(x = indat,
                      reason = TRUE)
      message('Validity check for each geometry: ')
      print(validity_check)
    }


  } else if (filetype == 'attribute') {

    # define path to source file
    dsn <- fs::path(path)

    # check that attribute source is .csv file
    if (!fs::path_ext(dsn) == 'csv') {
      stop("Attribute source must be a csv file", call. = FALSE)
    }

    # read in attribute file
    indat <- readr::read_csv(file = dsn,
                             guess_max = 10000)

  } else {
    stop("Filetype must be either 'spatial' or 'attribute'", call. = FALSE)
  }

  return(indat)

}
