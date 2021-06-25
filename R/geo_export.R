#' Export geospatial and/or attribute data to a file
#'
#' This function exports spatial and attribute data to file.  The output file type
#' can be a shapefile (.shp), comma separated value file (.csv), Excel workbook (.xlsx),
#' or a SAS data file (.sas7bdat).
#'
#' @param x Name of R data object to output. REQUIRED.
#' @param path Filepath specifying location and name of output file to create. REQUIRED.
#' @param filetype Type of output file to create.  Must be one of the following: "shp" (default),
#' "csv" (.csv comma separated value file), "sas" (.sas7bdat data file),
#' "excel" (.xlsx workbook file). REQUIRED.
#' @param na Character string to be used for missing values in the output file. Replaces all
#' empty values ("" and " "), and properly coded missing values (NA), with the specified
#' string. If no string is provided, default is NA. OPTIONAL.
#' @param overwrite If output dataset already exists at the path location, should R overwrite
#' (replace existing file) or not?  Default is to not overwrite an existing file. OPTIONAL.
#'
#' @return
#' @export
#'
#' @examples
#'# simple export shp file ------------------------------------------------------------------
#'geo_export(x = my_dat,
#'  path = my_outdir,
#'  filetype = "shp",
#'  overwrite = TRUE)
#'
#'# exporting when file already exists ------------------------------------------------------
#'geo_export(x = my_dat,
#'  path = my_outdir,
#'  filetype = "shp",
#'  overwrite = FALSE) # will not replace existing file
#'
#'geo_export(x = my_dat,
#'  path = my_outdir,
#'  filetype = "shp",
#'  overwrite = TRUE) # replaces existing file
#'
#'# specify new value ('unknown') to replace empty and missing values in output file --------
#'geo_export(x = my_dat,
#'  path = my_outdir,
#'  filetype = "shp",
#'  na = "unknown",
#'  overwrite = TRUE)



geo_export <- function(
  x,
  path,
  filetype,
  na = NA,
  overwrite = FALSE) {


  # check that mandatory arguments are defined
  if (missing(x) || missing(path) || missing(filetype)) {
    stop("Data object (x), output path and filetype are required", call. = FALSE)
  }


  # check that output filetype is appropriate
  if (!filetype %in% c('shp', 'csv', 'sas', 'excel')) {
    stop("Filetype must be one of shp, csv, sas or excel", call. = FALSE)
  }


  # check that R object for output exists
  if (exists(deparse(substitute(x))) == FALSE) {
    stop("R object does not exist", call. = FALSE)
  }


  # check if dataset has a geometry column
  if (!filetype == 'shp' & "geometry" %in% colnames(x)) {
    message(sprintf("Geometry column will be dropped when outputing %s filetype", filetype))
  }


  # check that output directory exists (and if not, create it)
  dir_out <- fs::path_dir(path)

  if (!dir.exists(dir_out)) {
    dir.create(dir_out)
    message("New directory created: ")
    print(dir_out)
  }


  # replace missing values with user specified string
  na_vals <- c("", " ", NA, "NA")

  x_clean <- x %>%
    as.data.frame(.) %>%
    dplyr::mutate_all(~ifelse(. %in% na_vals, NA, .)) %>%
    replace(is.na(.), na)


  # fix geometry for shapefile output
  if (any(class(x) == 'sf') & filetype == 'shp') {
    x_crs <- sf::st_crs(x)
    x_clean <- x_clean %>%
      sf::st_as_sf() %>%
      sf::st_set_geometry(.$geometry) %>%
      sf::st_set_crs(x_crs)
  }

  # drop geometry if not shapefile output
  if (any(class(x) == 'sf') & !filetype == 'shp') {
  x_clean <- x_clean %>% dplyr::select(-geometry)
  }


  # set appropriate extension and create output file

  # output shapefile
  if (filetype == 'shp') {
    path <- fs::path_ext_set(path, "shp")

    if (file.exists(path) == TRUE & overwrite == TRUE) {
      message("Output dataset already exists and will be overwritten")
    }

    if (file.exists(path) == TRUE & overwrite == FALSE) {
      stop("Output dataset already exists and will not be overwritten", call. = FALSE)
    }

    sf::st_write(obj = x_clean, dsn = path, delete_layer = overwrite)
  }


  # output csv
  if (filetype == 'csv') {
    path <- fs::path_ext_set(path, "csv")

    if (file.exists(path) == TRUE & overwrite == TRUE) {
      message("Output dataset already exists and will be overwritten")
    }

    if (file.exists(path) == TRUE & overwrite == FALSE) {
      stop("Output dataset already exists and will not be overwritten", call. = FALSE)
    }

    readr::write_csv(x = x_clean, file = path)
  }

  # output SAS
  if (filetype == 'sas') {
    path <- fs::path_ext_set(path, "sas7bdat")

    if (file.exists(path) == TRUE & overwrite == TRUE) {
      message("Output dataset already exists and will be overwritten")
    }

    if (file.exists(path) == TRUE & overwrite == FALSE) {
      stop("Output dataset already exists and will not be overwritten", call. = FALSE)
    }

    haven::write_sas(data = x_clean, path = path)
  }

  # output EXCEL
  if (filetype == 'excel') {
    path <- fs::path_ext_set(path, "xlsx")

    if (file.exists(path) == TRUE & overwrite == TRUE) {
      message("Output dataset already exists and will be overwritten")
    }

    if (file.exists(path) == TRUE & overwrite == FALSE) {
      stop("Output dataset already exists and will not be overwritten", call. = FALSE)
    }

    xlsx::write.xlsx(x = x_clean, file = path, row.names = FALSE, showNA = TRUE)
  }


}
