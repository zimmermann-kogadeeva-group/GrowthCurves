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
  title = "Growth curves",

  # Sidebar panel for inputs ----
  sidebar = sidebar(
    fileInput("file1", "Choose a File:", multiple=F, accept=c(".xlsx")), 

    selectizeInput(
      "sheet_names", "Sheets from excel table:", choices = NULL, multiple = TRUE
    ),

    selectizeInput(
      "blank_wells", "Blank wells:", choices = wells, multiple = TRUE
    ),

    selectizeInput(
      "normalize_over", "Mean blanks over:", choices = c("row", "col", "well", "plate"), multiple = TRUE
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
    updateSelectizeInput(session, "sheet_names", "Sheets from excel table", choices = vars, server=T)
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
    get_sheets() %>%
    ggplot(aes(x=time_elapsed_min, y = norm_OD, color = plate)) + 
      geom_point(size = 0.5) +
      facet_grid(row ~ col) + 
      geom_line(aes(color = plate)) + 
      theme_bw()
  })

}

shinyApp(ui = ui, server = server)
