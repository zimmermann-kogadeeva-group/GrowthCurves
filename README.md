
# GrowthCurves

This repository contains the R script needed to process the growth curves data,
as well as the files needed for the associated shiny app.

To use the `read_od_data()` function, copy the script to where you would like
to use it and add `source("read_od_data.R")` at the top of your R script or
Rmarkdown file.

Then open the excel spreadsheet with the data with:
```{r}
read_od_data("<path_to_excel_file>", c(<sheet_names>), blank_wells=well %in% c(<blank_well_ids>)) 
```
For example:
```{r}
df_buni <- read_od_data(
  here("Data/DBT003FGH-Check-B12-EM.xlsx"), 
  sheet=c("Plate 8 - Sheet1", "Plate 6 - Sheet1", "Plate 4 - Sheet1"),
  blank_wells=well %in% c("C10", "D10")
)
```
Then this can be plotted with:
```{r}
df_buni %>%
ggplot(aes(x=time_elapsed_min, y = norm_OD, color = plate)) + 
  geom_point(size = 0.5) +
  facet_grid(row ~ col) + 
  geom_line(aes(color = plate)) + 
  theme_bw()
```
