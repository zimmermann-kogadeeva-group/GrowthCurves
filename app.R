library(shiny)
library(bslib)
library(magrittr)
library(ggplot2)

source("read_od_data.R")


wells <- expand.grid(col = LETTERS[1:8], row = seq(1, 12)) %>%
  dplyr::mutate(well = paste0(col, row)) %>%
  dplyr::pull(well)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Growth curves",

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    wellPanel(
      h4("Data"),
      fileInput(
        "file1",
        "Choose a File:",
        multiple = FALSE,
        accept = c(".xlsx", ".txt")
      ),
      selectizeInput(
        "sheet_names",
        "Sheets from excel table:",
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
        selected = "time_elapsed_min",
        choices = c("time_elapsed_min", "row", "col", "well", "plate"),
        multiple = TRUE
      ),
      actionButton("open_given_sheets", "open given sheets"),
    ),
    wellPanel(
      h4("Plotting"),
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
  ),
  plotOutput("plot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  v <- reactiveValues(df_full = NULL, df_subset = NULL)

  observeEvent(input$file1, {
    req(input$file1)
    in_file <- input$file1

    # Read in the sheet names
    file_ext <- tools::file_ext(in_file$datapath)
    if (file_ext == "xlsx") {
      vars <- readxl::excel_sheets(in_file$datapath)
    } else if (file_ext == "txt") {
      idxs <- get_idxs_from_txt(in_file$datapath)$start
      vars <- seq(1, length(idxs))
    } else {
      stop(paste("Unknown format:", file_ext))
    }

    # Update select input immediately after clicking on the action button.
    updateSelectizeInput(
      session,
      "sheet_names",
      "Sheets from excel table",
      choices = vars,
      server = T
    )
  })

  observeEvent(input$open_given_sheets, {
    # Check the input has been given
    req(input$sheet_names)
    req(input$file1)

    blank_wells <- input$blank_wells
    normalize_over <- input$normalize_over

    # Read in the OD data
    v$df_full <- read_od_data(
      input$file1$datapath,
      sheets = input$sheet_names,
      blank_wells = well %in% blank_wells,
      normalize_over = normalize_over
    )
    v$df_subset <- v$df_full

    vars <- v$df_full %>%
      dplyr::select(-c(
        "time_elapsed_min",
        "OD",
        "norm_OD",
        "mean_OD_blank"
      )) %>%
      colnames()

    updateSelectizeInput(
      session,
      "filter_col",
      "Column to filter by:",
      choices = c("", vars),
      selected = NULL
    )
  })

  observeEvent(input$file_metadata, {
    if (is.null(v$df_full)) {
      return()
    }
    df <- v$df_full

    req(input$file_metadata)
    v$df_full <- add_metadata(df, input$file_metadata$datapath)

    vars <- v$df_full %>%
      dplyr::select(-c(
        "time_elapsed_min",
        "OD",
        "norm_OD",
        "mean_OD_blank"
      )) %>%
      colnames()

    updateSelectizeInput(
      session,
      "filter_col",
      "Column to filter by:",
      choices = c("", vars),
      selected = NULL
    )
  })

  observeEvent(input$filter_col, {
    # update choices for filter value
    if (is.null(v$df_full)) {
      return()
    }
    df <- v$df_full

    vars <- df %>%
      dplyr::pull(input$filter_col) %>%
      unique()

    updateSelectizeInput(
      session,
      "filter_val",
      "Value to filter by:",
      choices = c("", vars),
      selected = NULL
    )
  })

  observeEvent(input$filter_val, {
    if (is.null(v$df_full)) {
      return()
    }

    df <- v$df_full

    filter_col <- input$filter_col
    filter_val <- input$filter_val

    filter_col <- rlang::sym(filter_col)

    if (length(filter_val) == 1) {
      df_subset <- df %>% dplyr::filter(!!filter_col == filter_val)
    } else if (length(filter_val) > 1) {
      df_subset <- df %>% dplyr::filter(!!filter_col %in% filter_val)
    }

    # Make sure that filtering can be done and does not produce an empty
    # table
    if (nrow(df_subset) > 0) {
      v$df_subset <- df_subset
    }
  })

  output$plot <- renderPlot({
    if (is.null(v$df_subset)) {
      return()
    }

    df <- v$df_subset

    # Whether to plot replicates separetly or not
    if (input$lines) {
      g <- df %>%
        ggplot(aes(x = time_elapsed_min, y = norm_OD, color = plate)) +
        geom_point(size = 0.5) +
        geom_line(aes(color = plate))
    } else {
      g <- df %>%
        add_mean_and_sd() %>%
        ggplot(aes(x = time_elapsed_min)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey", alpha = 0.7) +
        geom_line(aes(y = mean_norm_OD))
    }

    g +
      ggh4x::facet_grid2(
        row ~ col,
        scales = ifelse(input$shared_axes, "fixed", "free"),
        independent = ifelse(input$shared_axes, "none", "y")
      ) +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)
