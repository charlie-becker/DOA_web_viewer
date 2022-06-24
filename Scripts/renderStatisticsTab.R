##' Render STATISTICS Tab 
##' 
##' This function performs the server-side tasks for the Statistics tab

renderStatisticsTab <- function(input, output, session, dat) {
 
  # no selectInput choices to update
  
  df_stats <- reactive({
    if (input$domainInput2 == 'Snake River AVA') {
      data.table::fread('data/AVA_30YR_NoLeap917.csv')
    } else if (input$domainInput2 == 'Sunnyslope') {
      data.table::fread('data/SS_30YR_NoLeap917.csv')
    } else if (input$domainInput2 == 'Domain 01') {
      data.table::fread('data/d01_30YR_NoLeap917.csv')
    }
  })
  
  DTdf <- reactive({
    df <- df_stats() %>% 
      group_by(get(input$myGroup)) %>%
      na.omit() %>% 
      summarise(Mean_Temp = mean(TMEAN),
                Mean_Max_Temp = mean(TMAX),
                Mean_Min_Temp = mean(TMIN), 
                GDD = sum(GDD),
                Total_Precip = sum(DPRCP),
                Total_Snow = sum(DSNOW),
                Frost_Days = sum(FROSTD),
                Frost_Hours = sum(FROSTH)) %>%
      round(2)
      # change column name to "YEAR" or "MONTH" or "DAY
      names(df)[1] <- input$myGroup
      
      df
  })
  
  output$myTable <- DT::renderDataTable(
    DT::datatable(DTdf(), 
                  fillContainer = T,
                  extensions = 'Buttons',
                  options = list(pageLength = 50,
                                 dom = 'tB',
                                 buttons = list(
                                   list(extend = 'csv', filename = paste(input$domainInput2, input$myGroup, sep = '_')),
                                   list(extend = 'excel', filename = paste(input$domainInput2, input$myGroup, sep = '_'))
                                 )
                                ))
  )
}
