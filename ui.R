ui <- fluidPage(
  

  titlePanel("US Gasoline Data (1991-2017)"),
  
  navbarPage("Individual Midterm Project", theme = shinytheme("superhero"),
             tabPanel(title = "Introduction", h3(paste("Instructions:")), h5(paste("Hello, and welcome to my app! Use the tabs above to navigate through the application and read my analysis.")), 
                      h5(paste("The data used in this analysis was US finished motor gasoline production measured in million barrels per day"))),
             
             tabPanel(title = "Full Time Series",
                      plotOutput("plot")
                      ),
             
             tabPanel(title = "Choose a Plot", 
                      selectInput("Choose", label = "Choose your plot of choice", choices = c("Decomposition", "ACF", "Seasonality")),
                      # plotOutput("choose"),
                      # checkboxInput("Interp", label = h5("Click for interpretation of the graph:"), value = FALSE),
                      # textOutput("text"),
                      
                      fluidRow(
                        splitLayout(cellWidths = c("43%", "42%", "15%"), plotOutput("choose"), verbatimTextOutput("text"),
                                    checkboxInput("Interp", label = h6("Click for interpretation of the graph:"), value = FALSE))
                      )
             ),
             
             tabPanel(title = "Transformations",
                      selectInput(
                        inputId = "select",
                        label = "Transform",
                        choices = c("Inverse", "Cube Root","Square Root", "Box Cox")),
                      fluidRow(
                        splitLayout(cellWidths = c("45%", "45%", "10%"), plotOutput("original"), plotOutput("transform"),
                                    checkboxInput(inputId = "show_transform", label = h6("Click to show transformation"), value = FALSE)))
    
                      ),
             
             tabPanel(title = "Simple Models",
                      
                      sidebarPanel( 
                                  selectInput("simple",
                                  label = "Choose a simple model",
                                  choices = c("Naive", "Seasonal Naive", "Mean", "Drift"))
                      
                      
                      ),
                      
                      mainPanel(
                        plotOutput("simple")
                                
                                )
                      ),
             
             tabPanel(title = "Exponential smoothing",
                      
                      sidebarPanel( 
                        selectInput("smoothing",
                                    label = "Choose a Exponential Model",
                                    choices = c("Holts", "Holts/Winters"))
                        
                        
                      ),
                      
                      mainPanel(
                        plotOutput("smoothing")
                        
                      )
             ),
             
             
             tabPanel(title = "ARIMA",
                      
                      sidebarPanel( 
                        selectInput("arima",
                                    label = " Auto Arima Modeling",
                                    choices = c("Auto", "Manual Parameters")
                        #uiOutput('dynamic_ui')
                        
                        ),

                      ),
                      
                      mainPanel(
                        uiOutput('dynamic_ui'),
                        plotOutput('selected')
                        
                      )
             

)

)

)

# selectInput("Manual",
#             label = "Choose your parameters (0,1,2)",
#             choices = c("Parameter 1", "Parameter 2", "Parameter 3"))
# 

  
  