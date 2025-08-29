library(shiny)
library(bslib)
library(magrittr)

source("read_od_data.R")
source("fitting.R")


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
  blanks_expr <- if (!is.null(blank_wells)) {
    rlang::expr(well %in% !!blank_wells) # use quosure injection (!!)
  } else {
    NULL
  }

  tryCatch(
    {
      df_full <- read_od_data(
        in_file$datapath,
        sheets = sheet_names,
        blank_wells = !!blanks_expr,
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
      dplyr::select(-tidyr::any_of(c(
        "time_elapsed_min",
        "OD",
        "norm_OD"
      ))) %>%
      colnames()

    updateSelectizeInput(
      session,
      "filter_col",
      "Column to filter by:",
      choices = c("", vars),
      selected = NULL
    )
  }

  # Make sure that `norm_OD` column is in dataframe
  if (!("norm_OD" %in% colnames(df_full))) {
    df_full <- df_full %>% dplyr::mutate(norm_OD = OD)
  }

  return(df_full)
}

subset_data <- function(data, filter_col, filter_val) {
  if (is.null(filter_col) || filter_col == "") {
    return(data)
  }

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
    } else if (fitting_opt == "logistic" || fitting_opt == "gompertz") {
      tagList(
        sliderInput(
          "fit_y0",
          "y-intercept",
          min = 0.0,
          max = 1.0,
          value = c(0.0, 0.3),
          step = 0.01
        ),
        sliderInput(
          "fit_mumax",
          "growth rate",
          min = 0.0,
          max = 2.0,
          value = c(0.0, 1.0),
          step = 0.001
        ),
        sliderInput(
          "fit_K",
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
    all_pred_exp(x = "time_elapsed_min", y = "norm_OD") %>%
    dplyr::filter(norm_OD < max(data$norm_OD))

  return(list(df_pred = df_pred, fit_results = growthrates::results(fit_exp)))
}

run_fit_logistic <- function(data, y0_range, mumax_range, k_range) {
  lower <- c(
    y0 = y0_range[1],
    mumax = mumax_range[1],
    K = k_range[1]
  )
  upper <- c(
    y0 = y0_range[2],
    mumax = mumax_range[2],
    K = k_range[2]
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
    all_pred_nonlinear(x = "time_elapsed_min", y = "norm_OD")

  return(list(df_pred = df_pred, fit_results = growthrates::results(fit_logit)))
}

run_fit_gompertz <- function(data, y0_range, mumax_range, k_range) {
  lower <- c(
    y0 = y0_range[1],
    mumax = mumax_range[1],
    K = k_range[1]
  )
  upper <- c(
    y0 = y0_range[2],
    mumax = mumax_range[2],
    K = k_range[2]
  )
  p <- 0.5 * (lower + upper)

  fit_gomp <- growthrates::all_growthmodels(
    norm_OD ~ growthrates::grow_gompertz(time_elapsed_min, parms) | row + col + plate,
    data = data,
    p = p,
    lower = lower,
    upper = upper,
  )

  df_pred <- fit_gomp %>%
    all_pred_nonlinear(x = "time_elapsed_min", y = "norm_OD")

  return(list(df_pred = df_pred, fit_results = growthrates::results(fit_gomp)))
}

run_fit <- function(
    data,
    fitting_opt,
    exp_window_size,
    fit_y0,
    fit_mumax,
    fit_k) {
  if (is.null(data) || is.null(fitting_opt)) {
    return()
  }
  fit_params_cond <- (
    is.null(fit_y0) ||
      is.null(fit_mumax) ||
      is.null(fit_k)
  )

  withProgress(message = "Fitting to data...", {
    # fit either exp or logistic growth
    if (fitting_opt == "exp" && !is.null(exp_window_size)) {
      return(run_fit_exp(data, exp_window_size))
    } else if (fitting_opt == "logistic" && !fit_params_cond) {
      return(run_fit_logistic(data, fit_y0, fit_mumax, fit_k))
    } else if (fitting_opt == "gompertz" && !fit_params_cond) {
      return(run_fit_gompertz(data, fit_y0, fit_mumax, fit_k))
    }
  })
}

fitting_table <- function(fit_results) {
  if (is.null(fit_results)) {
    return()
  }

  showModal(modalDialog(
    title = "Fitting info",
    renderTable(fit_results, digits = 5),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
}
