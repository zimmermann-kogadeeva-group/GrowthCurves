library(shiny)
library(bslib)
library(magrittr)
library(ggplot2)

source("read_od_data.R")


wells <- expand.grid(col=LETTERS[1:8], row=seq(1, 12)) %>% 
    dplyr::mutate(well=paste0(col, row)) %>% 
    dplyr::pull(well)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title="Growth curves",

    # Sidebar panel for inputs ----
    sidebar=sidebar(
        wellPanel(
            h4("Data"),
                
            fileInput("file1", "Choose a File:", multiple=F, accept=c(".xlsx")), 

            selectizeInput(
                "sheet_names", "Sheets from excel table:", choices=NULL, multiple=TRUE
            ),

            selectizeInput(
                "blank_wells", "Blank wells:", choices=wells, multiple=TRUE
            ),

            selectizeInput(
                "normalize_over", 
                "Mean blanks over:", 
                selected="time_elapsed_min",
                choices=c("time_elapsed_min", "row", "col", "well", "plate"),
                multiple=TRUE
            ),

            actionButton("open_given_sheets", "open given sheets"),
        ),

        wellPanel(
            h4("Plotting"),

            radioButtons(
                inputId="shared_axes",
                label="Y-axis:",
                choices=list(
                    "Shared"=T,
                    "Independant"=F
                )
            ),

            radioButtons(
                inputId="lines",
                label="Plot type:",
                choices=list(
                    "Individual lines"=T,
                    "Mean +/- SD"=F
                )
            ),

            fileInput("file_metadata", "Metadata:", accept=c(".xlsx")), 

            selectizeInput(
              "filter_col", "Column to filter by:", choices=NULL
            ),

            selectizeInput(
              "filter_val", "Value to filter by:", choices=NULL
            ),


        ),
    ),

    plotOutput("plot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    v <- reactiveValues(df_full=NULL, df_subset=NULL)

    sheet_choice <- observeEvent(input$file1, {

        req(input$file1)
        inFile <- input$file1

        # Read in the sheet names
        vars <- readxl::excel_sheets(inFile$datapath)

        # Update select input immediately after clicking on the action button. 
        updateSelectizeInput(
            session, 
            "sheet_names", 
            "Sheets from excel table",
            choices=vars, 
            server=T
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
            sheet=input$sheet_names, 
            blank_wells=well %in% blank_wells,
            normalize_over=normalize_over
        )
        v$df_subset <- v$df_full

        vars <- v$df_full %>% 
            dplyr::select(-c("time_elapsed_min", "OD", "norm_OD", "mean_OD_blank")) %>% 
            colnames()

        updateSelectizeInput(
            session, 
            "filter_col", 
            "Column to filter by:",
            choices=c("", vars),
            selected=NULL
        )
    })

    observeEvent(input$file_metadata, {
        if(is.null(v$df_full)) return()
        df <- v$df_full

        req(input$file_metadata)
        v$df_full <- add_metadata(df, input$file_metadata$datapath)

        vars <- v$df_full %>% 
            dplyr::select(-c("time_elapsed_min", "OD", "norm_OD", "mean_OD_blank")) %>% 
            colnames()

        updateSelectizeInput(
            session, 
            "filter_col", 
            "Column to filter by:",
            choices=c("", vars),
            selected=NULL
        )

    })

    observeEvent(input$filter_col, {
        # update choices for filter value
        if(is.null(v$df_full)) return()
        df <- v$df_full

        vars <- df %>% dplyr::pull(input$filter_col) %>% unique()

        updateSelectizeInput(
            session, 
            "filter_val", 
            "Value to filter by:",
            choices=c("", vars), 
            selected=NULL
        )
    })

    observeEvent(input$filter_val, {
        if(is.null(v$df_full)) return()
       
        df <- v$df_full

        filter_col <- input$filter_col
        filter_val <- input$filter_val
        
        # Make sure that filtering can be done and does not produce an empty
        # table
        if (!(is.null(filter_col) || filter_col == "") && 
            !(is.null(filter_val) || filter_val == "")) { 
            df_subset <- df[df[filter_col] == filter_val, ]
            if (nrow(df_subset) > 0)
                v$df_subset <- df_subset
        }
    })

    output$plot <- renderPlot({
        if(is.null(v$df_subset)) return()

        df <- v$df_subset

        # Whether to plot replicates separetly or not
        if (input$lines) {
            g <- df %>%
            ggplot(aes(x=time_elapsed_min, y=norm_OD, color=plate)) + 
                geom_point(size=0.5) +
                geom_line(aes(color=plate))
        } else {
            g <- df %>% 
            add_mean_and_sd() %>%
            ggplot(aes(x=time_elapsed_min)) + 
                geom_ribbon(aes(ymin=lb, ymax=ub), fill="grey", alpha=0.7) + 
                geom_line(aes(y=mean_norm_OD))
        }

        g + 
            ggh4x::facet_grid2(
                row ~ col, 
                scales=ifelse(input$shared_axes, "fixed", "free"), 
                independent=ifelse(input$shared_axes, "none", "y")
            ) + 
            theme_bw()
    })

}

shinyApp(ui=ui, server=server)
