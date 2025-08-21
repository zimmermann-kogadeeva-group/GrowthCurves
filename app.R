library(shiny)
library(bslib)
library(magrittr)
library(plotly)
library(ggplot2)

source("read_od_data.R")
source("fitting.R")


wells <- expand.grid(col = LETTERS[1:8], row = seq(1, 12)) %>%
  dplyr::mutate(well = paste0(col, row)) %>%
  dplyr::pull(well)

# Define UI for app that draws a histogram ----

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
          selected = "time_elapsed_min",
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
        actionButton("clear_fit", "Clear fitting"),
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
      ),
      open = "Load data",
      multiple = FALSE,
    )
  ),
  plotlyOutput("plot")
)

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
      start <- get_metadata_txt(in_file$datapath)$start
      vars <- seq(1, length(start))
    } else {
      stop(paste("Unknown format:", file_ext))
    }

    # Update select input immediately after clicking on the action button.
    updateSelectizeInput(
      session,
      "sheet_names",
      "Choose sheets:",
      choices = vars,
      server = TRUE
    )
  })

  observeEvent(input$open_given_sheets, {
    # Check the input has been given
    req(input$sheet_names)
    req(input$file1)

    blank_wells <- input$blank_wells
    normalize_over <- input$normalize_over

    # Read in the OD data
    tryCatch(
      {
        v$df_full <- read_od_data(
          input$file1$datapath,
          sheets = input$sheet_names,
          blank_wells = well %in% blank_wells,
          normalize_over = normalize_over
        )
        v$df_subset <- v$df_full
      },
      warning = function(w) {
        print(w)
        showNotification("Warning", "", type = "warning")
        return()
      },
      error = function(e) {
        print(e)
        showNotification("Could not read the data", "", type = "error")
        return()
      }
    )

    if (is.data.frame(v$df_full)) {
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
    }
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

    # TODO: clearing this should reset the view to all
    updateSelectizeInput(
      session,
      "filter_val",
      "Value to filter by:",
      choices = c("", vars),
      selected = NULL
    )
  })

  observeEvent(input$clear_fit, {
    updateRadioButtons(session, "fitting_opt", selected = character(0))
  })

  output$further_fitting_opts <- renderUI({
    if (!is.null(input$fitting_opt)) {
      if (input$fitting_opt == "exp") {
        sliderInput(
          "exp_window_size",
          "Window size:",
          min = 0,
          max = 50,
          value = 5
        )
      } else if (input$fitting_opt == "logistic") {
        tagList(
          sliderInput(
            "logit_y0",
            "y-intercept",
            min = 0.0,
            max = 1.0,
            value = c(0.0, 0.3),
            step = 0.01
          ),
          sliderInput(
            "logit_mumax",
            "growth rate",
            min = 0.0,
            max = 10.0,
            value = c(0.2, 0.6),
            step = 0.01
          ),
          sliderInput(
            "logit_K",
            "carrying capacity",
            min = 0.0,
            max = 2.0,
            value = c(0.8, 1.5),
            step = 0.01
          )
        )
      }
    }
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
    } else {
      df_subset <- df
    }

    # Make sure that filtering can be done and does not produce an empty
    # table
    if (nrow(df_subset) > 0) {
      v$df_subset <- df_subset
    }
  })

  output$plot <- renderPlotly({
    if (is.null(v$df_subset)) {
      return()
    }

    withProgress(message = "Making plot", {
      df <- v$df_subset

      incProgress(1 / 4, detail = "Setting up base plot")

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

      incProgress(2 / 4, detail = "Setting up facets")

      g <- g +
        ggh4x::facet_grid2(
          row ~ col,
          scales = ifelse(input$shared_axes, "fixed", "free"),
          independent = ifelse(input$shared_axes, "none", "y")
        ) +
        theme_bw()

      if (!is.null(input$fitting_opt)) {
        df_pred <- data.frame()

        if (input$fitting_opt == "exp") {
          # Fitting stuff - here for the moment
          spar <- 0.5

          if (nrow(df %>% dplyr::filter(norm_OD < 0)) > 0) {
            showNotification(
              paste0(
                "Some wells and replicates have negative values!\n",
                "No fitting will be done in these cases."
              )
            )
          }
          fit_exp <- growthrates::all_easylinear(
            norm_OD ~ time_elapsed_min | row + col + plate,
            data = {
              df %>%
                dplyr::group_by(well, plate) %>%
                dplyr::filter(!any(norm_OD < 0)) %>%
                dplyr::ungroup()
            },
            h = input$exp_window_size,
            spar = spar
          )

          df_pred <- fit_exp %>%
            all_pred_linear(time_elapsed_min = "time", norm_OD = "y") %>%
            dplyr::filter(norm_OD < max(df$norm_OD))
        } else if (input$fitting_opt == "logistic") {
          p <- c(
            y0 = mean(input$logit_y0),
            mumax = mean(input$logit_mumax),
            K = mean(input$logit_K)
          )
          lower <- c(
            y0 = input$logit_y0[1],
            mumax = input$logit_mumax[1],
            K = input$logit_K[1]
          )
          upper <- c(
            y0 = input$logit_y0[2],
            mumax = input$logit_mumax[2],
            K = input$logit_K[2]
          )

          fit_logit <- growthrates::all_growthmodels(
            norm_OD ~ growthrates::grow_logistic(time_elapsed_min, parms) | row + col + plate,
            data = df,
            p = p,
            lower = lower,
            upper = upper,
          )

          df_pred <- fit_logit %>%
            all_pred_nonlinear(time_elapsed_min = "time", norm_OD = "y")
        }

        g <- g + geom_line(
          aes(group = plate),
          data = df_pred,
          color = "black",
          linewidth = 1.6
        ) +
          geom_line(data = df_pred, linetype = "dashed", linewidth = 1.0)
        # TODO: download button for df_pred
      }

      incProgress(3 / 4, detail = "Finishing figure")

      g %>%
        ggplotly() %>%
        return()
    })
  })
}

shinyApp(ui = ui, server = server)
