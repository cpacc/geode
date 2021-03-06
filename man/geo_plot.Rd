% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_plot.R
\name{geo_plot}
\alias{geo_plot}
\title{Create maps from geospatial and attribute data}
\usage{
geo_plot(
  data,
  geography_col,
  attribute_data = NA,
  points_col,
  plot_type,
  style = NA,
  transparency = NA,
  levels = 5,
  hover_id,
  plot_title = NA,
  legend_title = NA,
  scale_bar = FALSE,
  compass = FALSE,
  interactive = FALSE
)
}
\arguments{
\item{data}{Input data for plotting; must be a simple features (sf)
shape object, e.g., imported by geo_import(). REQUIRED. (Note: if input data
contains invalid geometries, a warning will be provided and the geometries
will be automatically fixed prior to plotting)}

\item{geography_col}{Name of the data column containing the feature,
e.g., geographic units, population size, etc., to be plotted. REQUIRED.}

\item{attribute_data}{Name of the dataset containing attributes, e.g., points,
to be plotted. Dataset must have a pair of columns for geographic coordinates of
the attributes (e.g., lat and lon) titled 'x' and 'y'. Coordinate reference
system (CRS) for attribute data is automatically transformed to match that of
input geographic data.  If CRS is missing for attribute data, it is assumed to be
WGS84. REQUIRED (for pointmaps and heatmaps only).}

\item{points_col}{Name of the data column containing the label or value to be shown
when hovering over a particular point location.  For interactive pointmaps only.
Default is the first column in the specified attribute dataset.  OPTIONAL.}

\item{plot_type}{Name of one of the predefined plot types: current options
are 'choropleth', 'pointmap' and 'heatmap'. REQUIRED.}

\item{style}{Name of a defined output style.  User defined output styles can be
created following examples shown in the help files for tmap (see ?tmap::tmap_options).
For illustrative purposes, the current selection of styles are "zissou", "royal", "darjeeling",
"bottlerocket", "moonrise" and "isleofdogs" (from the wesanderson R pacakge); 'viridis" and
"magma" (from the viridis R package); and "terrain" (from base R).  If omitted, defaults to tmap
"white" style. OPTIONAL.}

\item{transparency}{value between 0 and 1 (default) that defines transparency of map colours,
from transparent (0) to opaque (1). Intermediate values allow more or less visibility of underlying
(reference) layers in interactive maps.  Applies only to choropleth maps.  OPTIONAL.}

\item{levels}{Preferred number of shading categories for choropleth maps when geography_col
is a numeric variable. Default is 5.  Generates equally sized categories of round
numbers, resulting in a number of levels close to, but not necessarily exactly, the specified
number of levels. OPTIONAL.}

\item{hover_id}{Name of the data column containing the label or value to be shown
when hovering over a particular geographic unit.  For interactive choropleth maps only.
Default is the name or value given in specified geography_col.  OPTIONAL.}

\item{plot_title}{Main title of plot.  If omitted, no title is shown. Applies only to static
image maps. OPTIONAL.}

\item{legend_title}{Title of plot legend. If no value is given, geography_col name is used.
In static maps, the legend is removed if legend_title = 'none'.  OPTIONAL.}

\item{scale_bar}{TRUE or FALSE (default).  Indicating whether reference scale
bar should be shown (bottom left of plot). Applies only to static image maps. OPTIONAL.}

\item{compass}{TRUE or FALSE (default).  Indicating whether reference compass
should be shown (top right of plot). Applies only to static image maps. OPTIONAL.}

\item{interactive}{TRUE or FALSE (default).  Indicating whether map should be
generated as an interactive view or as a static image. OPTIONAL.}
}
\value{

}
\description{
This function creates static or interactive maps (choropleth, pointmap or heatmap)
from geospatial and attribute data.
}
\examples{
# simple static choropleth map with plot title and legend title ------------------
geo_plot(data = my_geo_data,
 geography_col = 'region_data_column',
 plot_type = 'choropleth',
 plot_title = 'My Geographic Regions',
 legend_title = 'Region Name')

# interactive choropleth map with partial transparency of shaded regions ---------
geo_plot(data = my_geo_data,
 geography_col = 'region_data_column',
 plot_type = 'choropleth',
 transparency = 0.5,
 hover_id = 'region_popsize_column',
 interactive = TRUE)

# static pointmap with plot title but no legend ----------------------------------
geo_plot(data = my_geo_data,
 geography_col = 'region_data_column',
 attribute_data = my_point_location_data,
 points_col = 'my_point_name_variable',
 plot_type = 'pointmap',
 plot_title = 'My Point Locations',
 legend_title = 'none')
}
