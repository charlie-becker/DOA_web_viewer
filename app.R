library(xts)
library(ncdf4)
library(shiny)
library(leaflet)
library(raster)
library(rjson)
library(sp)
library(shinyWidgets)
library(dplyr)
library(htmlwidgets)
library(dygraphs)

source("buttonIndicator.R")

d <- read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/AVA_30YR_NoLeap917.csv")
dates <- seq(as.Date("1987-01-01"),as.Date("1987-12-31"), "day")

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

scaleType <- c("Individual Years", "Monthly (across all years)", "All years")

setwd("/Users/charlesbecker/Desktop/Data/30YR_Daily/Data/")

monNames = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# get netCDF datamb 
ncFileNames <- list.files(pattern = ".nc")

# initial raster
init_raster <- "AVA_WY1988_yearly_stats_d02.nc"

# get AVA Shape File
jsonFile <- "/Users/charlesbecker/Desktop/Data/Project Data/Shiny/SR_AVA_simplified_pointRemove50m.json"

# set variable names to pull from file and variable names to list on UI
ncVarNames <- c("TMAX", "TMIN", "TMEAN", "GDD", "DPRCP", "DSNOW", "FROSTD", "FROSTH")
varNamesLong <-  c("Maximum Daily Temperature","Minimum Daily Temperature","Mean Daily Temperature", "Growing Degree Days",
                   "Total Precipitation","Snowfall", "Frost Days", "Frost Hours")

# convert JSON file to R list object (this is just an additional shape layer)
json <- fromJSON(file = jsonFile)

# define CRS from netCDF file (from WRF) and Extent (from GDAL)
myCRS <- CRS("+proj=lcc +lat_1=44.299999f +lat_2=44.99999f +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs")
myExtent <- c(-247739.5, 100260.5, -185023.9, 141976.1)

# Load raster and set projection and lat/lon extent
init_rast <- brick(init_raster, varname="GDD") 
projection(init_rast) <- myCRS
extent(init_rast) <- myExtent

# Project to the leaflet lat/long grid and visualize
r <- projectRasterForLeaflet(sum(init_rast[[1:10]]), method = "bilinear")

# set color palette
color_pal <- colorNumeric(c("dark red", "light blue", "dark green"), values(r),
                          na.color = "transparent")

# reverse the palette 
rev_color_pal <- colorNumeric(rev(c("dark red", "light blue", "dark green")), values(r),
                              na.color = "transparent")

# create leaflet map with projected raster and marker located at "Sunnyslope"
map = leaflet() %>% addTiles() %>%
    setView(lng = -116.5, lat = 43.8 ,zoom = 8) %>%
    addMarkers(lat = 43.5885, lng = -116.7932, label = as.character(round(r[221,79],4))) %>%
    addGeoJSON(json, weight = .5, color = "black", fill = F, opacity = 1) %>%
    addRasterImage(r, colors = rev_color_pal, opacity = .7) %>%
    addLegend(pal = color_pal, values = values(r),
              title = "GDD",labFormat = 
                  labelFormat(transform = function(x) sort(x, decreasing = TRUE))) 

################################################################################

ui <- navbarPage("SRAVA",
    tabPanel("Explorer",             
    useShinyjs(),
    tags$style(appCSS),
    titlePanel("Snake River Valley American Viticultural Area"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("domainInput", "Domain", choices = c("Snake River AVA (1km resolution)", "Domain 02 (1km resolution)", "Domain 01 (3km resolution)"), selected = "Snake River AVA (1km resolution)"),
            radioButtons("plotInput", "Plot", choices = c("Historical","Yearly Anomaly", "Monthly Anomaly"), selected = "Historical"),
            selectInput("varInput", "Variables", choices = varNamesLong, selected = varNamesLong[1]),
            selectInput("yearInput", "Year", choices = 1988:2017, selected = "1988"),
            conditionalPanel(
                condition = "input.plotInput == 'Historical'",
                sliderInput("dateInput", "Days of Water Year", min = 1, max = 365, value = c(1,10))),
            conditionalPanel(
                condition = "input.plotInput == 'Monthly Anomaly'",
                selectInput("monInput","Month", choices = monNames)),
            withBusyIndicatorUI(actionButton("button", "Create Map", class = "btn-primary"))),
        mainPanel(
            leafletOutput("myMap", width = "900", height = "650"),
            br(),br()
        ))),
        tabPanel("TimeSeries",
                 titlePanel("Time series statistics"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("domainInput1", "Domain",
                                     choices = c("Snake River AVA", "Sunnyslope", "Domain 02", "Domain 01"),  
                                     selected = "Snake River AVA"),
                         selectInput("scaleInput","What would you like to plot?", 
                                     choices = scaleType, selected = scaleType[1] ),
                         pickerInput("yearInput1", "Years", choices = 1987:2017, selected = "1995", multiple = T,
                                     options =  list("max-options" = 5)),
                         selectInput("varInput1", "Variable", choices = varNamesLong, selected = varNamesLong[3]),
                         radioButtons("unitInput", "Unit type", 
                                      choices = c("Fahrenheit/Inches", "Celcius/mm"), selected = "Fahrenheit/Inches")),
                     
                     mainPanel(
                         dygraphOutput("myGraph", width = "900", height = "650"),
                         br(),br()
                     )
                 )),
        tabPanel("Stats"),
        tabPanel("Background")
    
)

################################################################################

server = function(input, output, session) {
    
###############################################################################
#  The follwing section refers to the "Explorer" Tab Panel 
    
    
    # render leaflet map
    output$myMap <- renderLeaflet(map)
    
    # get variable name to pull from data from user selection
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
   # m <- reactive ({ monNames[match(input$monInput, monNames)] })
    # conditionals to determine which file to load 
    rast <- eventReactive(input$button, { 
        if (input$plotInput == "Historical") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick(paste0("./AVA_WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Domain 02 (1km resolution)") { 
                brick(paste0("./WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Domain 01 (3km resolution)") { 
                brick(paste0("./WY",input$yearInput, "_yearly_stats_d01.nc"), varname = v(),
                      crs = myCRS) }}
        else if (input$plotInput == "Yearly Anomaly") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick("./AVA_Yearly_Anomalies_d02.nc", varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Domain 02 (1km resolution)") { 
                brick("./Yearly_Anomalies_d02.nc", varname = v(),
                      crs = myCRS) }}
        else if (input$plotInput == "Monthly Anomaly") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick(paste0("./", input$monInput, "_AVA_Anomalies_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Domain 02 (1km resolution)") { 
                brick(paste0("./", input$monInput, "_Anomalies_d02.nc"), varname = v(),
                      crs = myCRS) }
        }}
        )
    
    # set the extent of loaded file for proper projection
    rast1 <- eventReactive(input$button, { setExtent(rast(), myExtent) })
    
    # project to the leaflet lat/long grid 
    rast2 <- eventReactive(input$button, { 
        
        if (input$plotInput == "Historical") {
        
            if ((input$varInput %in% varNamesLong[1:3]) == TRUE) {
                
                projectRasterForLeaflet(mean(rast1()[[input$dateInput[1]:input$dateInput[2]]], na.rm = T), method = "bilinear") }
            else {
                projectRasterForLeaflet(sum(rast1()[[input$dateInput[1]:input$dateInput[2]]]), method = "bilinear") }}
        
        else if (input$plotInput == "Yearly Anomaly" || input$plotInput == "Monthly Anomaly") {
            
            projectRasterForLeaflet(rast1()[[as.integer(input$yearInput)-1987]]-rast1()[[31]], method = "bilinear")
     }})
    #input$yearInput - 1987
    # set color palette
    color_pal <- eventReactive(input$button, { colorNumeric(c("dark red", "light blue", "dark green"), values(rast2()),
                                          na.color = "transparent") })
    
    # reverse color palette
    rev_color_pal <- eventReactive(input$button, { colorNumeric(rev(c("dark red", "light blue", "dark green")), values(rast2()),
                                              na.color = "transparent") })
    
        # create proxy leaflet map (from original map) 
    observeEvent(input$button, {
        withBusyIndicatorServer("button", { 
        leafletProxy("myMap", data = rast2()) %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(rast2(),colors = rev_color_pal(), opacity = .7) %>%
            addMarkers(lat = 43.5885, lng = -116.7932, label = as.character(round(rast2()[221,79],2))) %>%
            addLegend(pal = color_pal(), values = values(rast2()),
                      title = v(), labFormat = 
                          labelFormat(transform = function(x) sort(x, decreasing = TRUE)))})
    })
    


###############################################################################
## The Following section refers to the "Time series" tabPanel

vv <- reactive ({ ncVarNames[match(input$varInput1, varNamesLong)] })

dd <- reactive ({ if (vv() == "TMAX" | vv() == "TMIN" | vv() == "TMEAN") {
    
    as.data.frame(bind_cols(split(d[[vv()]], d$YEAR)) %>% select(input$yearInput1)
    )}
    else {cumsum(as.data.frame(bind_cols(split(d[[vv()]], d$YEAR)) %>% select(input$yearInput1))) }
})

df <- reactive ({ data.frame(dates, dd()) })

df_dy <- reactive ({ xts(df(), order.by = dates) })

output$myGraph <- renderDygraph({ dygraph(df_dy()) %>% 
        dyAxis("x", axisLabelFormatter = JS(getMonth), valueFormatter = JS(getMonthDay)) })

# Kill the app when closed in the browser 
session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
