library(dygraphs)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(xts)
library(htmlwidgets)

#the axis label is passed as a date, this function outputs only the month of the date
getMonth <- 'function(d){
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
return monthNames[d.getMonth()];
}'

#the x values are passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
date = new Date(d);
return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'

ncVarNames <- c("TMAX", "TMIN", "TMEAN", "GDD", "DPRCP", "DSNOW", "FROSTD", "FROSTH")

varNamesLong <-  c("Maximum Daily Temperature","Minimum Daily Temperature","Mean Daily Temperature", "Growing Degree Days",
                   "Total Precipitation","Snowfall", "Frost Days", "Frost Hours")

scaleType <- c("Individual Years", "Monthly (across all years)", "All years")

d <- read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/AVA_30YR_NoLeap917.csv")

dates <- seq(as.Date("1987-01-01"),as.Date("1987-12-31"), "day")

ui <- fluidPage(
    #useShinyjs(),
    #tags$style(appCSS),
    titlePanel("Time series statistics"),
    sidebarLayout(
        sidebarPanel(
            selectInput("domainInput", "Domain",
                        choices = c("Snake River AVA", "Sunnyslope", "Domain 02", "Domain 01"),  
                        selected = "Snake River AVA"),
            selectInput("scaleInput","What would you like to plot?", 
                        choices = scaleType, selected = scaleType[1] ),
            pickerInput("yearInput", "Years", choices = 1987:2017, selected = "1995", multiple = T,
                        options =  list("max-options" = 5)),
            selectInput("varInput", "Variable", choices = varNamesLong, selected = varNamesLong[3]),
            radioButtons("unitInput", "Unit type", 
                         choices = c("Fahrenheit/Inches", "Celcius/mm"), selected = "Fahrenheit/Inches")),
        
        mainPanel(
            dygraphOutput("myGraph", width = "900", height = "650"),
            br(),br()
        )
    )
)

server = function(input, output, session) {
    
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
    
    dd <- reactive ({ if (v() == "TMAX" | v() == "TMIN" | v() == "TMEAN") {
        
        as.data.frame(bind_cols(split(d[[v()]], d$YEAR)) %>% select(input$yearInput)
    )}
        else {cumsum(as.data.frame(bind_cols(split(d[[v()]], d$YEAR)) %>% select(input$yearInput))) }
        })
    
    df <- reactive ({ data.frame(dates, dd()) })
    
    df_dy <- reactive ({ xts(df(), order.by = dates) })
    
    output$myGraph <- renderDygraph({ dygraph(df_dy()) %>% 
            dyAxis("x", axisLabelFormatter = JS(getMonth), valueFormatter = JS(getMonthDay)) })
}


shinyApp(ui = ui, server = server)



