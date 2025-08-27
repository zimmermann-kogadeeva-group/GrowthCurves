library(shiny)
library(bslib)
library(magrittr)
library(plotly)


wells <- expand.grid(col = LETTERS[1:8], row = seq(1, 12)) %>%
  dplyr::mutate(well = paste0(col, row)) %>%
  dplyr::pull(well)


ui <- page_sidebar(
  # App title ----
  title = "Growth curves",

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    accordion(
      accordion_panel(
        title = "Load data",
        fileInput(
          "file1",
          "Choose a file:",
          multiple = TRUE,
          accept = c(".xlsx", ".txt")
        ),
        selectizeInput(
          "sheet_names",
          "Choose sheets:",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          "blank_wells",
          "Blank wells:",
          choices = wells,
          multiple = TRUE
        ),
        selectizeInput(
          "normalize_over",
          "Mean blanks over:",
          selected = NULL,
          choices = c("time_elapsed_min", "row", "col", "well", "plate"),
          multiple = TRUE
        ),
        actionButton("open_given_sheets", "open given sheets"),
      ),
      accordion_panel(
        title = "Plotting options",
        radioButtons(
          inputId = "shared_axes",
          label = "Y-axis:",
          choices = list(
            "Shared" = TRUE,
            "Independant" = FALSE
          )
        ),
        radioButtons(
          inputId = "lines",
          label = "Plot type:",
          choices = list(
            "Individual lines" = TRUE,
            "Mean +/- SD" = FALSE
          )
        ),
        fileInput("file_metadata", "Metadata:", accept = c(".xlsx")),
        selectizeInput(
          "filter_col", "Column to filter by:",
          choices = NULL
        ),
        selectizeInput(
          "filter_val", "Value to filter by:",
          choices = NULL,
          multiple = TRUE,
        ),
      ),
      accordion_panel(
        title = "Fitting options",
        radioButtons(
          inputId = "fitting_opt",
          label = "Model type:",
          choices = list(
            "Exponential" = "exp",
            "Logistic" = "logistic"
          ),
          selected = character(0)
        ),
        uiOutput("further_fitting_opts"),
        actionButton("run_fit", "Fit to data"),
        actionButton("clear_fit", "Clear fitting"),
        actionButton("show_fitting_info", "Show fitting info"),
        downloadButton("download_fitting_info", "Download fitting info"),
      ),
      open = "Load data",
      multiple = FALSE,
    )
  ),
  plotlyOutput("plot")
)
