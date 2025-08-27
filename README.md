
# GrowthCurves

This repository contains the R script needed to process the growth curves data,
as well as the files needed for the associated shiny app.

## Shiny app

To use the shiny app go to <http://zim-matlab01.embl.de:3838> and use the
panel on the left to upload the data and metadata, as well as control various
plotting options.

## R script

To use the `read_od_data()` function, copy `R/read_od_data.R` to where you would like
to use it and add `source(here::here("R/read_od_data.R"))` at the top of your R
script or Rmarkdown file.

Example rmarkdown notebook [example.qmd](example.qmd) shows how to use
functions available in `read_od_data.R` script. To try it out with the example
data, clone this repository and then:
1. Open rstudio
2. Click on **File ->  New project -> Existing Directory** and navigate to
   where you downloaded this repository.
