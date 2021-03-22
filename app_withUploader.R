library(shiny)
library("tidyverse")
library("mice")

source("https://raw.githubusercontent.com/pqstri/MuSC-19_tools/master/patients_exporter.R")
source("https://raw.githubusercontent.com/pqstri/MuSC-19_tools/master/visualization.R")
source("https://raw.githubusercontent.com/pqstri/MuSC-19_tools/master/outcome_vert.R")
source("https://raw.githubusercontent.com/pqstri/MuSC-19_tools/master/imputation.R")

max.file.size.MB <- 15
options(shiny.maxRequestSize = max.file.size.MB * 1024^2)

ui <- fluidPage(
    title = "MuSC-19 conversion tool",
    
    titlePanel("MuSC-19 conversion tool"),
    
    h3("To use this tool:"),
    tags$ol(
        tags$li("Access to Musc-19 as admin at https://musc-19.dibris.unige.it:4343"),
        tags$li("Go to \"Extraction\" Tab"),
        tags$li("Select report type: \"Per Patient\""),
        tags$li("Scoll to the bottom and click \"Generate report\""),
        tags$li("Keep track of the saved *.csv file (do not open and save with MS Excel)"),
        tags$li("Navigate to this page"),
        tags$li("Click the \"Browse\" button below and upload the downloaded *.csv file"),
        tags$li("Wait for the preview table to load"),
        tags$li("Click \"Download .xlsx\"")
    ),

    # br(),
    # h4("Currently on Provider's Blacklist (not exported):"),
    # 
    # tags$ul(
    #     map(MPS_format$provider_blacklist, ~ tags$li(.))
    # ),
    
    br(),
    fileInput("file", NULL, accept = c(".csv", ".tsv")),
    
    br(),
    radioButtons("options", "Select conversion type:",
                 c("Classical (imputed)" = "classical",
                   "Classical (imputed) + original variables" = "original",
                   "Event details" = "events",
                   "Classical (not imputed) + original variables" = "not_imp"),
                 selected = "original"),    
    
    br(),
    br(),
    downloadButton("download", "Download .xlsx"),
    
    br(),
    br(),
    h2("Top 10 sites"),
    tableOutput("head_sites"),
    
    br(),
    br(),
    h2("Top 10 country"),
    tableOutput("head_countries"),
    
    br(),
    plotOutput("recruitment")
    
    # br(),
    # tableOutput("summary")
)

server <- function(input, output, session) {
    
    data <- reactive({
        req(input$file)
        
        if(!endsWith(input$file$name, ".csv")) {
            validate("Invalid file; Please upload a .csv")}
        
        suppressWarnings(clean(convert(input$file$datapath)))
    })
    
    output$head_sites <- renderTable({
        dplyr::count(data(), 
                     Provider = Base_Provider,
                     Country = Demography_Country, 
                     "Site Code" = `Demography_Site Code`,
                     name = "N. of records",
                     sort = TRUE) %>% 
            head(10)
    })
    
    output$head_countries <- renderTable({
        dplyr::count(data(),
                     Country = Demography_Country,
                     name = "N. of records",
                     sort = TRUE) %>% 
            head(10)
    })
    
    output$recruitment <- renderPlot({
        # rec_plot(data())
    },
    height = 600)
    
    # output$summary <- renderTable({
    #     enrollment_summary(data())
    # })
    
    output$download <- downloadHandler(

        filename = function() {
            paste("musc19", format(Sys.time(), "_%d%b%Y"), ".xlsx", sep = "")},
        
        content = function(file) {
            final <<- 
                switch(input$options,
                       original= ioe(data(), check_original = TRUE),
                       classical = ioe(data()),
                       events = ioe(data(), debug = TRUE),
                       not_imp = {
                           f <- data()
                           left_join(prepare_export(f),
                                     compute_outcomes(f, check_original = TRUE), 
                                     by = c("PAT_ID" = "upid"))
                           }
                )

            try(closure(final))
            
            openxlsx::write.xlsx(final, file)
        }
        
        
        
    )
    
}
# Run the application
# shinyApp(ui = ui, server = server)
