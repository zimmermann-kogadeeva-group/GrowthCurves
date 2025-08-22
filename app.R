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
      ),
      open = "Load data",
      multiple = FALSE,
    )
  ),
  plotlyOutput("plot")
)

update_sheets_info <- function(in_file, session) {
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
}

read_data <- function(in_file, sheet_names, blank_wells, norm_over, session) {
  # Read in the OD data
  tryCatch(
    {
      df_full <- read_od_data(
        in_file$datapath,
        sheets = sheet_names,
        blank_wells = well %in% blank_wells,
        normalize_over = norm_over
      )
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

  # Update possible selection in plotting options
  if (is.data.frame(df_full)) {
    vars <- df_full %>%
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

  return(df_full)
}

subset_data <- function(data, filter_col, filter_val) {
  filter_col <- rlang::sym(filter_col)

  if (length(filter_val) == 1) {
    df_subset <- data %>% dplyr::filter(!!filter_col == filter_val)
  } else if (length(filter_val) > 1) {
    df_subset <- data %>% dplyr::filter(!!filter_col %in% filter_val)
  } else {
    df_subset <- data
  }

  # Make sure that filtering can be done and does not produce an empty
  # table
  if (nrow(df_subset) > 0) {
    return(df_subset)
  } else {
    return(data)
  }
}

fitting_params <- function(fitting_opt) {
  if (!is.null(fitting_opt)) {
    if (fitting_opt == "exp") {
      sliderInput(
        "exp_window_size",
        "Window size:",
        min = 0,
        max = 50,
        value = 5
      )
    } else if (fitting_opt == "logistic") {
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
          max = 2.0,
          value = c(0.0, 1.0),
          step = 0.001
        ),
        sliderInput(
          "logit_K",
          "carrying capacity",
          min = 0.0,
          max = 2.0,
          value = c(0.5, 1.5),
          step = 0.01
        )
      )
    }
  }
}

run_fit_exp <- function(data, exp_window_size) {
  # Fitting stuff - here for the moment
  spar <- 0.5

  if (nrow(data %>% dplyr::filter(norm_OD < 0)) > 0) {
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
      data %>%
        dplyr::group_by(well, plate) %>%
        dplyr::filter(!any(norm_OD < 0)) %>%
        dplyr::ungroup()
    },
    h = exp_window_size,
    spar = spar
  )

  df_pred <- fit_exp %>%
    all_pred_linear(time_elapsed_min = "time", norm_OD = "y") %>%
    dplyr::filter(norm_OD < max(data$norm_OD))

  return(list(df_pred=df_pred, results=growthrates::results(fit_exp)))
}

run_fit_logistic <- function(data, y0_range, mumax_range, K_range) {
  lower <- c(
    y0 = y0_range[1],
    mumax = mumax_range[1],
    K = K_range[1]
  )
  upper <- c(
    y0 = y0_range[2],
    mumax = mumax_range[2],
    K = K_range[2]
  )
  p <- 0.5 * (lower + upper)

  fit_logit <- growthrates::all_growthmodels(
    norm_OD ~ growthrates::grow_logistic(time_elapsed_min, parms) | row + col + plate,
    data = data,
    p = p,
    lower = lower,
    upper = upper,
  )

  df_pred <- fit_logit %>%
    all_pred_nonlinear(time_elapsed_min = "time", norm_OD = "y")

  return(list(df_pred=df_pred, results=growthrates::results(fit_logit)))
}

server <- function(input, output, session) {
  v <- reactiveValues(df_full = NULL, df_subset = NULL, df_pred = NULL, fit_results = NULL)

  observeEvent(input$file1, {
    req(input$file1)
    update_sheets_info(input$file1, session)
  })

  observeEvent(input$open_given_sheets, {
    # Check the input has been given
    req(input$file1)
    req(input$sheet_names)
    req(input$blank_wells)
    req(input$normalize_over)

    v$df_full <- read_data(
      input$file1,
      input$sheet_names,
      input$blank_wells,
      input$normalize_over,
      session
    )
    v$df_subset <- v$df_full
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

  observeEvent(input$filter_val, ignoreNULL = FALSE, {
    if (!is.null(v$df_full)) {
      v$df_subset <- subset_data(v$df_full, input$filter_col, input$filter_val)
      v$df_pred <- NULL
    }
  })

  observeEvent(input$clear_fit, {
    updateRadioButtons(session, "fitting_opt", selected = character(0))
    v$df_pred <- NULL
  })

  output$further_fitting_opts <- renderUI({
    fitting_params(input$fitting_opt)
  })

  observeEvent(input$run_fit, {
    if (is.null(v$df_subset) || is.null(input$fitting_opt)) {
      return()
    }

    withProgress(message = "Fitting to data...", {
      if (input$fitting_opt == "exp" && !is.null(input$exp_window_size)) {
        fit_res <- run_fit_exp(v$df_subset, input$exp_window_size)
        v$df_pred <- fit_res$df_pred
        v$fit_results <- fit_res$results
      } else if (
        input$fitting_opt == "logistic" &&
          !is.null(input$logit_y0) &&
          !is.null(input$logit_mumax) &&
          !is.null(input$logit_K)
        ) {
        fit_res <- run_fit_logistic(v$df_subset, input$logit_y0, input$logit_mumax, input$logit_K) 
        v$df_pred <- fit_res$df_pred
        v$fit_results <- fit_res$results
      }
    })

  })

  observeEvent(input$show_fitting_info, {
    if (is.null(v$fit_results)) {
      return()
    }

    showModal(modalDialog(
      title="Fitting info",
      renderTable(v$fit_results, digits=5),
      size="l",
      easyClose=TRUE,
      footer=modalButton("Close")
    ))
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

      if (!is.null(v$df_pred) && nrow(v$df_pred) > 0) {
        fit_data <- v$df_pred %>% 
          dplyr::left_join(
            v$fit_results %>% 
            dplyr::mutate(plate = as.character(plate), col = as.character(col))
          ) %>%
          dplyr::rename(tidyr::any_of(c(growth_rate="mumax")))

        g <- g + geom_line(
          aes(group = plate),
          data = fit_data,
          color = "black",
          linewidth = 1.6
        ) +
          geom_line(aes(growth_rate=growth_rate), data = fit_data, linetype = "dashed", linewidth = 1.0)
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
