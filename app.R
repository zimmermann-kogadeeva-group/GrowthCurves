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

    actionButton("open_given_sheets", "open given sheets"),
  ),

  plotOutput("plot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  sheet_choice <- observeEvent(input$file1, {

    req(input$file1)
    inFile <- input$file1

    # Read in the sheet names
    vars <- readxl::excel_sheets(inFile$datapath)

    # Update select input immediately after clicking on the action button. 
    updateSelectizeInput(session, "sheet_names", "Sheets from excel table", choices=vars, server=T)
  })

  get_sheets <- eventReactive(input$open_given_sheets, {
    req(input$sheet_names)
    req(input$file1)

    blank_wells <- input$blank_wells
    normalize_over <- input$normalize_over
    df <- read_od_data(
        input$file1$datapath, 
        sheet=input$sheet_names, 
        blank_wells=well %in% blank_wells,
        normalize_over=normalize_over
    )
    df
  })
    
  output$plot <- renderPlot({
    df <- get_sheets()

    if (input$lines) {
      g <- df %>%
      ggplot(aes(x=time_elapsed_min, y=norm_OD, color=plate)) + 
        geom_point(size=0.5) +
        geom_line(aes(color=plate))
    } else {
      df_means <- df %>%
      dplyr::group_by(time_elapsed_min, well) %>% 
      dplyr::summarize(mean_norm_OD = mean(norm_OD), sd_norm_OD = sd(norm_OD)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(lb=mean_norm_OD - sd_norm_OD, ub=mean_norm_OD + sd_norm_OD) %>%
      dplyr::full_join(df, by=c("time_elapsed_min", "well"))

      g <- df_means %>%
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
