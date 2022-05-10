server <- function(input, output, session){
  
  output$plot<- renderPlot({
    autoplot(us_gas_mth, BarrelsMean)
    
    
  })
  
  # output$intro <- renderText({
  #  paste("Hello, and welcome to my app", "Use the tabs above to navigate through the application and read my analysis", 
  #           "The data used in this analysis was US finished motor gasoline production",
  #        "Measured in million barrels per day", sep = "\n")
 #})
  
  output$choose <- renderPlot({
    if(input$Choose == "Decomposition"){
      us_gas_mth %>%
        model(classical_decomposition(BarrelsMean, type = 'multiplicative')) %>%
        components() %>%
        autoplot
    }
    else if(input$Choose == "ACF"){
      (autoplot(ACF(us_gas_mth, BarrelsMean)))
    }
    else if (input$Choose == "Seasonality"){
      (gg_season(us_gas_mth, BarrelsMean))
    }
  })
  
  output$text <- renderText({
    
    if(input$Interp == T & input$Choose == "Decomposition"){
     paste("The trend is the largest component of the of the model, with random 
variation being the second largest, and seasonal being
the third largest component. The trend seems to be positive with no 
cyclicity. Seasonality seems to have a repeating, cyclical pattern with
no variation over the time period. Classical decomposition assumes 
that the seasonal component repeats from year to year. You
can see more seasonality in the \"Seasonality\" option above.")
    }
    else if(input$Interp == T & input$Choose == "ACF"){
      paste("The slow decrease in ACF as the lags increase is due to the trend,
and the scallop shape is due to the seasonality. There does not seem
to be any white noise associated with this data and the ACF values are
outside the control limits. This means gas sales this week would be 
a good predictor for what next week's sales in gas will be.")
    }
    else if (input$Interp == T & input$Choose == "Seasonality"){
      paste("There seems to be some seasonality around the warmer summer months.
More barrels are purchased in the summer months than winter months. 
This is most likely due to the fact that more people travel and take 
vacations in the summer.")
      
    }
    
    
  })
  
  
  output$original <- renderPlot({
    autoplot(us_gas_mth, BarrelsMean) + labs(title ="Original")
  })
  
  output$transform <- renderPlot({
    if(input$select == "Inverse" & input$show_transform == T){
      us_gas_mth %>% 
      autoplot(-1/BarrelsMean) + labs(y = "Inverse Transformation")
    }
    else if(input$select == "Cube Root" & input$show_transform == T){
      us_gas_mth %>% 
        autoplot(BarrelsMean^(1/3)) + labs(y = "Cube Root Transformation")
    }
    
    else if(input$select == "Square Root" & input$show_transform == T){
      us_gas_mth %>% 
        autoplot(BarrelsMean^(1/2)) + labs(y = "Square Root Transformation")
    }
    
    else if(input$select == "Box Cox" & input$show_transform == T){
      us_gas_mth %>% 
        autoplot(box_cox(BarrelsMean, lambda)) + labs(y = "Box Cox Transformation")
    }
    
    
  })
  
  output$simple <- renderPlot({
    if("Naive" %in% input$simple){
      
      fit <- us_gas_mth %>%
        model(NAIVE(BarrelsMean))
    
    }
    if("Seasonal Naive" %in% input$simple){
      
      fit <- us_gas_mth %>%
        model(SNAIVE(BarrelsMean ~ lag("year")))
  
    }
    if("Mean" %in% input$simple){
      
      fit <- us_gas_mth %>%
        model(MEAN(BarrelsMean))
      
    }
    if("Drift" %in% input$simple){
      
      fit <- us_gas_mth %>%
        model(RW(BarrelsMean ~ drift()))
      
    }
    
    fit %>%
      forecast() %>%
      autoplot(us_gas_mth) +
      labs(title = input$simple)
    
  })
  
  output$smoothing <- renderPlot({
    if("Holts" %in% input$smoothing){
      fit_smooth <- us_gas_mth %>%
        model(
          AAN = ETS(BarrelsMean ~ error("A") + trend("A") + season("N"))
        ) 
    }
    if("Holts/Winters" %in% input$smoothing){
      fit_smooth <- us_gas_mth %>%
        model(
          additive = ETS(BarrelsMean~ error("A") + trend("A") +
                           season("A")),
          multiplicative = ETS(BarrelsMean ~ error("M") + trend("A") +
                                 season("M"))
        )

      
    }
    
    fit_smooth %>% 
      forecast() %>% 
      autoplot(us_gas_mth, level = NULL) + labs(title = input$smoothing)
  })
  
  output$dynamic_ui <- renderUI({
    
    if("Manual Parameters" %in% input$arima){
      
      div(
        
        selectInput('parameters1','Select your Model', 
                    choices = c("arima011011",
                                "arima110210" ,
                                "arima210" ,
                                "arima013","arima012011", "arima210011")
        
        # selectInput('parameters2','Second Set of Parameters for ps,,qs respectively',choices=c(0,1,2,
        #                                                                                        0,1,2,
        #                                                                                        0,1,2),
        #             multiple = TRUE)
        
      )
      )
      
    }
  })

  output$selected <- renderPlot({
    
    if("Auto" %in% input$arima){
      fit_arima <- us_gas_mth %>%
        model(ARIMA(BarrelsMean))
    }
    
    if("Manual Parameters" %in% input$arima & "arima011011" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(0, 1, 1) + PDQ(0, 1, 1)))
          
        )
    }
    
    
    if("Manual Parameters" %in% input$arima & "arima110210" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(1, 1, 0) + PDQ(2, 1, 0)))
          
        )
    }
    
    if("Manual Parameters" %in% input$arima & "arima210" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(2, 1, 0)))
          
        )
    }
    
    if("Manual Parameters" %in% input$arima & "arima013" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(0, 1, 3)))
          
        )
    }
    
    
    if("Manual Parameters" %in% input$arima & "arima012011" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(0, 1, 2) + PDQ(0, 1, 1)))
          
        )
    }
    
    if("Manual Parameters" %in% input$arima & "arima210011" %in% input$parameters1){
      
      fit_arima <- us_gas_mth %>%
        model(
          (ARIMA(BarrelsMean ~ 0 + pdq(2, 1, 0) + PDQ(0, 1, 1)))
          
        )
    }
    
    fit_arima %>% 
      forecast(h = 24) %>% 
      autoplot(us_gas_mth)
    
    
  })
  # output$auto <- renderPlot({
  #   fit_arima <- us_gas_mth %>%
  #      model(autoarima = ARIMA(us_gas_mth))
  # 
  #   fit_arima %>%
  #     select(autoarima) %>%
  #     forecast(h = 20) %>%
  #     autoplot(us_gas_mth)
  # })
  # 
  # output$manual <- renderPlot({
  # 
  #   fit_arima <- us_gas_mth %>%
  #     model(
  #       ARIMA(BarrelsMean ~ 0 + pdq(input$parameter1, input$parameter2, input$parameter3) +
  #                  PDQ(input$parameter1, input$parameter2, input$parameter3))
  # 
  #     )
  #   fit_arima %>%
  #     forecast(h = 20) %>%
  #     autoplot(us_gas_mth)
  # 
  # })
}




