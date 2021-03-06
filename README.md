
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geode

<!-- badges: start -->
<!-- badges: end -->

## Overview

**geode** is a software tool for mapping and analysis of geospatial
data, designed in consultation with partners across the Canadian
provincial and territorial cancer agencies. Development of geode was
motivated by a need for local information on cancer screening.

The flexible design of geode allows for a wide variety of geospatial
work. The tool supports mapping and analysis of any geospatial boundary
data available via shapefiles, any point locations identifiable as x-y
positions (e.g., longitude and latitude), and any measures that can be
assigned to regions (e.g., population size and density, age and sex
distribution, counts and rates of disease or health conditions,
socio-economic indicators, environmental health measures, etc.).

Users of this tool are expected to have a basic understanding of the
characteristics, limitations and use of geospatial data and the basic
methods used in the analysis of such data.

## Installation

You can install geode directly in R from [GitHub](https://github.com/)
with:

``` r
install.packages("remotes")
remotes::install_github("cpacc/geode")
```

Alternatively, you can download geode as a zip file and install
following the instructions in the User’s Manual under the Chapter 1
section *Downloading and installing*.

<https://github.com/cpacc/geode/blob/main/zip/geode_0.1.1.tar.7z>

To unzip the file after downloading, you will need to install the freely
available 7-Zip program from <https://www.7-zip.org/>

## Getting started

All technical details, system requirements, examples and instructions
for getting started are found in the geode User’s Manual. A PDF version
of the manual can be found at

<https://github.com/cpacc/geode/blob/main/zip/users_manual_PDF.pdf>

An HTML version of the manual is also provided at

<https://github.com/cpacc/geode/blob/main/zip/users_manual_HTML.7z>

The HTML version is contained in a zip folder. Download and unzip (using
7-zip, as above), and then click on the manual shortcut
‘users\_manual\_HTML’.

## Example data

All data files used in the examples are freely available online. For
ease-of-use, certain data files are also included here as a 7-zip file

<https://github.com/cpacc/geode/blob/main/zip/example_data.7z>

## Updates (February 2022)

A new version of geode has been posted as of February 7, 2022. This new
version contains several improvements and fixes and is meant to be more
compatible with the **ShinyGeode** interface.

*Please update your geode package directly from R, following the
installation instructions above.*

Updates included in geode version 0.1.2 include

-   full mapping functionality found in `geo_plot()` has also been added
    to `geo_distance()`, `geo_detect()` and `geo_calcualte()`
-   interactive mapping in `geo_plot()` is now available for pointmaps
    as well as choropleth maps
-   the `validity_check` option in `geo_import()` now adds the results
    as a column in the imported data object and automatically fixes
    invalid geographies; a message is provided indicating if invalid
    geographies were detected and fixed
-   the `geo_plot()` function now check for and fixes invalid
    geographies prior to plotting; a message is provided indicating if
    invalid geographies were detected and fixed
