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
library(DT)

# script to create animation for loading new map
source("buttonIndicator.R")

# set working directory
setwd("/Users/charlesbecker/Desktop/Data/30YR_Daily/Data/AVA_d02/")

# laod data for dygraphs (AVA)
d <- read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/AVA_30YR_NoLeap917.csv")

# create a list of non-leap year dates to be used for dygraphs (year will be stripped)
dates <- seq(as.Date("1987-01-01"),as.Date("1987-12-31"), "day")

# Javascript to get the axis label passed as a date, this function outputs only the month of the date
getMonth <- 'function(d){
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
return monthNames[d.getMonth()];
}'

# Javascript to get the x values passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
date = new Date(d);
return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'

# Scale type for dygrpahs
scaleType <- c("Individual Years", "Monthly (across all years)", "All years")

# create month names
monNames = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# get list of netCDF files (raw data)
ncFileNames <- list.files(pattern = ".nc")

# initial file for default map upon loading
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
# User interface 
#

ui <- navbarPage("",
                 
        # Landing page.  "tags$" is replicating HTML directly         
        tabPanel("Home",
                 
            # Load CSS file (in /www)     
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                tags$meta(name="viewport", content="initial-scale=1")
            ),
            setBackgroundColor("white"), 
            tags$img(src="test_vine.jpg", class = "imgcon"), # top banner image
            tags$div(class = "text-block", # load CSS .text-block (style and positioning)
                tags$h1("Snake River Valley AVA"), # Title
                tags$h1("Climate Explorer")),
            tags$div(class = "p1",
                tags$p("The Snake River Valley American Viticulture Area Climate Explore is an interactive tool to visualize 30 years of high resolution climate data in the rapidily growning Idaho grape growing region and beyond."),
                tags$p("Navigation of tool is simple - use tabs up top to select the area of interest and you'll be directed to an interactive panel to select your desired parameters and visualize the data.")),
            tags$div(class = "intro-divider"), # see CSS "intro-divider" for how to make the gradient lines
            tags$div(class = "pblock",
                tags$p(tags$b("Explorer"), " is a spatial plotting tool to visualize historical and anomalous climate over individual years (or subsets thereof) in the Snake River Valley AVA and more broadly in the Pacific Northwest."),
                tags$p(tags$b("Time Series"), " is a temporal explorer to compare mean climatological values from different years."),
                tags$p("The ", tags$b("Statistics"), "tab is a work in progress..."),
                tags$p("The ", tags$b("Background"), "tab is a work in progress...")),
            tags$div(class = "intro-divider2"),
            
            # Logos at bottom of page with links
            tags$a(href = "https://www.boisestate.edu",target = "_blank", img(src="BSU2.png", class = "logo1")), 
            tags$a(href = "https://agri.idaho.gov/main/", target = "_blank", img(src="ISDOA.png", class = "logo2")),
            tags$a(href = "https://leaf.boisestate.edu/people/", target = "_blank", img(src="LEAF2.png", class = "logo3"))
                 ),
    # Explorer tab - the spatial viewer    
    tabPanel("Explorer",             
    useShinyjs(),
    sidebarLayout(
        # create side panel for parameter adjustment
        sidebarPanel(
            strong('Spatial Data Explorer', style = "font-size:36px"), br(), br(), #div(class = "intro-divider3"), br(),
            p('The spatial explorer shows the Snake River AVA - where the majority of Idaho’s vineyards are located. The full [dataset] expands far beyond this area which can be visualized by selecting the region of your choice. The plot can be zoomed in or out in the same fashion as Google Maps.', style = "font-size:18px"),
            p('To create a unique plot, select a climate variable (for example, temperature or precipitation) and years on the left and then click the ‘create map’ button. The historical option will require a date range to be averaged or summed over (depending on the variable).', style = "font-size:18px"),
            p('Anomalies are the difference between your year of choice  and the average of that variable for all 30 years.  For example, if you select ‘Monthly Anomaly’, VARIABLE=’Minimum Daily Temperature’,‘YEAR=2011’ and ‘MONTH=April’ you will get a plot showing the difference between the average minimum daily temperatures for the month of April in 2011 and the average minimum temperatures in April for all years spanning 1988-2017. In this example, temperatures are about 2.0 - 3.5 degrees Celsius cooler in April 2011 than the 30 year average. If you were to look at your crop yields over the 30 year timespan, it would be interesting to see how the deviation from average in 2011 affected your yields that year!', style = "font-size:18px"),
            br(), div(class = "intro-divider3"), br(),
        
            # Input types with defaults selected 
            radioButtons("domainInput", "Domain", choices = c("Snake River AVA (1km resolution)", "Domain 02 (1km resolution)", "Domain 01 (3km resolution)"), selected = "Snake River AVA (1km resolution)"),
            radioButtons("plotInput", "Plot", choices = c("Historical","Yearly Anomaly", "Monthly Anomaly"), selected = "Historical"),
            selectInput("varInput", "Variables", choices = varNamesLong, selected = varNamesLong[1]),
            selectInput("yearInput", "Year", choices = 1988:2017, selected = "1988"),
            
            # conditional parameter (add date selector) if looking at 'historical' data
            conditionalPanel(
                condition = "input.plotInput == 'Historical'",
                sliderInput("dateInput", "Days of Water Year", min = 1, max = 365, value = c(1,10)),
                dateRangeInput("myDate","Select dates to view", format = "mm-dd", startview = "year")),
            
            # conditional parameter (add month) if looking at 'Monthly Anomaly' data
            conditionalPanel(
                condition = "input.plotInput == 'Monthly Anomaly'",
                selectInput("monInput","Month", choices = monNames)),
            
            # call custom function for creating "Create map"button
            withBusyIndicatorUI(actionButton("button", "Create Map", class = "btn-primary"))),
        # start main panel (defaulted to right side)
        mainPanel(
            # Render leaflet map
            leafletOutput("myMap", width = "100%", height = "88vh")
        ))),
    
        # Start Time Seeries tab
        tabPanel("TimeSeries",
                 titlePanel(""),
                 sidebarLayout(
                     # Inputs 
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
                         # "Dygraph" (interactive timeseries graph)
                         dygraphOutput("myGraph", width = "100%", height = "60vh")
                     )
                 )),
        tabPanel("Statistics",
                 titlePanel(""),
                 sidebarPanel(
                     selectInput("domainInput2", "Domain",
                        choices = c("Snake River AVA", "Sunnyslope", "Domain 02", "Domain 01"),  
                        selected = "Snake River AVA"),
                     selectInput("myGroup", "Group by",
                        choices = c("YEAR","MONTH","DAY"),
                        selected = "YEAR")),
                mainPanel(
                     DT::dataTableOutput("myTable", height = "75vh")
                 )),
        tabPanel("Background"),
        tabPanel("Contact")
    
)

################################################################################

server = function(input, output, session) {
    
###############################################################################
#  The follwing section refers to the "Explorer" Tab Panel 
    
    # render leaflet map
    output$myMap <- renderLeaflet(map)
    
    # get variable name to pull from data from user selection
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
    
    # conditionals to determine which file to load (based on year and domain)
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
    
    # project to the leaflet lat/long grid based on conditions
    rast2 <- eventReactive(input$button, { 
        
        # if historical...
        if (input$plotInput == "Historical") {
        
            #(if selected temperature (use mean instead of sum))
            if ((input$varInput %in% varNamesLong[1:3]) == TRUE) {
                
                projectRasterForLeaflet(mean(rast1()[[input$dateInput[1]:input$dateInput[2]]], na.rm = T), method = "bilinear") }
            # use sum (non-temperature metrics)
            else {
                projectRasterForLeaflet(sum(rast1()[[input$dateInput[1]:input$dateInput[2]]]), method = "bilinear") }}
        
        # if yearly anomaly
        else if (input$plotInput == "Yearly Anomaly" || input$plotInput == "Monthly Anomaly") {
            
            projectRasterForLeaflet(rast1()[[as.integer(input$yearInput)-1987]]-rast1()[[31]], method = "bilinear")
     }})

    # set color palette
    color_pal <- eventReactive(input$button, { colorNumeric(c("dark red", "light blue", "dark green"), values(rast2()),
                                          na.color = "transparent") })
    
    # reverse color palette
    rev_color_pal <- eventReactive(input$button, { colorNumeric(rev(c("dark red", "light blue", "dark green")), values(rast2()),
                                              na.color = "transparent") })
    
    # create proxy leaflet map (from original map) 
    observeEvent(input$button, {
        withBusyIndicatorServer("button", { # add loading icon...
        leafletProxy("myMap", data = rast2()) %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(rast2(),colors = rev_color_pal(), opacity = .7) %>% # add the raster data
            addMarkers(lat = 43.5885, lng = -116.7932, label = as.character(round(rast2()[221,79],2))) %>% # add sunnyslope marker
            addLegend(pal = color_pal(), values = values(rast2()), # add legend
                      title = v(), labFormat = 
                          labelFormat(transform = function(x) sort(x, decreasing = TRUE)))})
    })
    
###############################################################################
## The Following section refers to the "Time series" tabPanel

# get variable selection    
vv <- reactive ({ ncVarNames[match(input$varInput1, varNamesLong)] })

 # create dataframe
dd <- reactive ({ 
    
    # if temperature is selected, get mean data 
    if (vv() == "TMAX" | vv() == "TMIN" | vv() == "TMEAN") {
    
        as.data.frame(bind_cols(split(d[[vv()]], d$YEAR)) %>% select(input$yearInput1)
    )}
    
    # if != temp, get cummulative sum of data
    else {cumsum(as.data.frame(bind_cols(split(d[[vv()]], d$YEAR)) %>% select(input$yearInput1))) }
})

# transform dataframe 
df <- reactive ({ data.frame(dates, dd()) })

# create xts object and order by date (for time series data - required by dygraphs)
df_dy <- reactive ({ xts(df(), order.by = dates) })

# render the dygraph using the javascript functions to correct the axis labels
output$myGraph <- renderDygraph({ dygraph(df_dy()) %>% 
        dyRangeSelector() %>%
        dyAxis("x", axisLabelFormatter = JS(getMonth), valueFormatter = JS(getMonthDay)) })

###############################################################################
# The folling section refers to the "Statistics" tab

# Coming soon!

df_stats <- reactive ({ 
    
    if (input$domainInput2 == "Snake River AVA") {
        read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/AVA_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Sunnyslope") {
        read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/SS_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Domain 01") {
        read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/d01_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Domain 02") {
        read.csv("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/30YR_Stats/d02_30YR_NoLeap917.csv") }
        })

DTdf <-  reactive ({ df_stats() %>% group_by_(input$myGroup)  %>% na.omit() %>% summarise(Mean_Temp = mean(TMEAN),
            Mean_Max_Temp = mean(TMAX), Mean_Min_Temp = mean(TMIN), GDD = sum(GDD),
            Total_Precip = sum(DPRCP), Total_Snow = sum(DSNOW), Frost_Days = sum(FROSTD),
            Frost_Hours = sum(FROSTH)) %>% round(2) })

output$myTable <- DT::renderDataTable({ DT::datatable(DTdf(),fillContainer = T,
                    options = list(pageLength = 50)) })

    
############################################################################### 
# Kill the app when closed in the browser 
session$onSessionEnded(stopApp)
}

# required when using single app.R with both ui and server combined
shinyApp(ui, server)