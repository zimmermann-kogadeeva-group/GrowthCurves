library(shiny)
library(bslib)
library(magrittr)
library(plotly)
library(rlang)
library(ggplot2)

source("shiny_funcs.R")


server <- function(input, output, session) {
  v <- reactiveValues(
    df_full = NULL,
    df_subset = NULL,
    df_pred = NULL,
    fit_results = NULL,
  )

  observeEvent(input$file1, {
    req(input$file1)
    update_sheets_info(input$file1, session)
  })

  observeEvent(input$open_given_sheets, {
    # Check the input has been given
    req(input$file1)
    req(input$sheet_names)

    v$df_full <- read_data(
      input$file1,
      input$sheet_names,
      input$blank_wells,
      input$normalize_over,
      session
    )
    v$df_subset <- subset_data(v$df_full, input$filter_col, input$filter_val)
  })

  observeEvent(input$file_metadata, {
    if (is.null(v$df_full)) {
      return()
    }
    df <- v$df_full

    req(input$file_metadata)
    v$df_full <- add_metadata(df, input$file_metadata$datapath)

    vars <- v$df_full %>%
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
  })

  observeEvent(input$filter_col, {
    # update choices for filter value
    if (is.null(v$df_full) || input$filter_col == "") {
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
    pred_res <- run_fit(
      v$df_subset,
      input$fitting_opt,
      input$exp_window_size,
      input$fit_y0,
      input$fit_mumax,
      input$fit_K
    )
    v$df_pred <- pred_res$df_pred
    v$fit_results <- pred_res$fit_results
  })

  observeEvent(input$show_fitting_info, {
    fitting_table(v$fit_results)
  })

  output$download_fitting_info <- downloadHandler(
    filename = function() {
      "fitting_info.csv"
    },
    content = function(fname) {
      write.csv(v$fit_results, fname)
    }
  )

  output$plot <- renderPlotly({
    if (is.null(v$df_subset)) {
      return()
    }

    df <- v$df_subset

    withProgress(message = "Making plot", {
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

      # Check whether normalisation was done and change y-label correspondingly
      y_var <- ifelse(is.null(input$blank_wells), "OD", "norm_OD")

      g <- g +
        ggh4x::facet_grid2(
          row ~ col,
          scales = ifelse(input$shared_axes, "fixed", "free"),
          independent = ifelse(input$shared_axes, "none", "y")
        ) +
        theme_bw() +
        labs(y = y_var)

      if (!is.null(v$df_pred) && nrow(v$df_pred) > 0) {
        fit_data <- v$df_pred %>%
          dplyr::left_join(
            v$fit_results %>%
              dplyr::mutate(
                plate = as.character(plate),
                col = as.character(col)
              )
          ) %>%
          dplyr::rename(
            tidyr::any_of(c(growth_rate = "mumax"))
          )

        g <- g + geom_line(
          aes(group = plate),
          data = fit_data,
          color = "black",
          linewidth = 1.6
        ) +
          geom_line(
            aes(growth_rate = growth_rate),
            data = fit_data,
            linetype = "dashed",
            linewidth = 1.0
          )
      }

      incProgress(3 / 4, detail = "Finishing figure")

      g %>%
        ggplotly() %>%
        return()
    })
  })
}
