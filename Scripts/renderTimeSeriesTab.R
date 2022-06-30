##' Render TIME SERIES Tab 
##' 
##' This function performs the server-side tasks for the Time Series tab

renderTimeSeriesTab <- function(input, output, session, dat) {
  
  observe({
    #' render input choices
    updateSelectInput(session = session, 'scaleInput', choices = dat$scaleType)
    
    updateSelectInput(session = session, 'varInput1', choices = dat$ncVars,
                      selected = dat$ncVars[3])
  })

  #' create dataframe
  dd <- reactive({
    # if temperature is selected, get mean data
    if (input$varInput1 %in% c('TMAX', 'TMIN', 'TMEAN')) {
      as.data.frame(bind_cols(split(dat$d[[input$varInput1]], dat$d$YEAR)) %>% 
                      select(input$yearInput1))
        # dat$d is AVA_30YR_NoLeap917.csv (data from all 30 years)
        # split that into component years and select the year in question
    } # if 
    # else if != temp, get cumulative sum of data over the course of that year
    else {
      # now do the same thing for the other inputs, but this time do a cumulative sum
      base::cumsum(as.data.frame(bind_cols(split(dat$d[[input$varInput1]], dat$d$YEAR)) %>%
                                   dplyr::select(input$yearInput1)
                                )
                  )
    } # else
  })
  
  # combine dd() with dat$dates
  # dat$dates is the dates for that year
  # I don't think the specific year matters; just the Month-Day part of the date
  df <- reactive({ data.frame(dat$dates, dd()) })

  # create an xts time-series object for plotting data
  df_dy <- reactive({ xts::xts(df(), order.by = dat$dates) })
  
  # render the dygraph using the javascript functions to correct the axis labels
  # this whole df_dy() thing is a bit of a black box to me; Charlie did all of it 
  # and I only had to make some minor changes (and those changes were just plug and chug)
  output$myGraph <- renderDygraph({
    dygraphs::dygraph(df_dy()) %>%
      dygraphs::dyRangeSelector() %>%
    # removed this line because it was messing up the app and don't seem to matter at all
      # getMonth and getMonthDay are functions defined in fct_helpersS
      dygraphs::dyAxis(name = 'x', axisLabelFormatter = htmlwidgets::JS(getMonth), valueFormatter = htmlwidgets::JS(getMonthDay))
  }) # renderdygraph
  
}