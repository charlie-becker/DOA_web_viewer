library(ncdf4)
library(shiny)
library(leaflet)
library(raster)
library(rjson)
library(sp)

source("buttonIndicator.R")

setwd("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/")

# get netCDF data
ncFileNames <- list.files(pattern = ".nc")

# get AVA Shape File
jsonFile <- "SR_AVA_simplified_pointRemove50m.json"

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
init_rast <- brick(ncFileNames[1], varname="GDD") 
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
              title = "2M Temp",labFormat = 
                  labelFormat(transform = function(x) sort(x, decreasing = TRUE))) 

################################################################################

ui <- fluidPage(
    useShinyjs(),
    tags$style(appCSS),
    titlePanel("Snake River Valley American Viticultural Area"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("domainInput", "Domain", choices = c("Snake River AVA (1km resolution)", "Domain 02 (1km resolution)", "Domain 01 (3km resolution)"), selected = "Snake River AVA (1km resolution)"),
            selectInput("yearInput", "Year", choices = 1988:1992, selected = "1988"),
            selectInput("varInput", "Variables", choices = varNamesLong, selected = varNamesLong[1]),
            sliderInput("dateInput", "Days of Water Year", min = 1, max = 365, value = c(1,10)),
            sliderInput("dateInput1", "Days of Water Year", min = as.Date("2007-10-01"), max = as.Date("2008-09-30"), value = c(as.Date("2007-10-01"),as.Date("2007-10-31"))),
            withBusyIndicatorUI(actionButton("button", "Create Map", class = "btn-primary"))),
        mainPanel(
            leafletOutput("myMap", width = "900", height = "650"),
            br(),br()
        )
    )
)

################################################################################

server = function(input, output, session) {
    
    # render leaflet map
    output$myMap <- renderLeaflet(map)
    
    # get variable name to pull from data from user selection
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
    
    # conditionals to determine which file to load 
    rast <- eventReactive(input$button, { 
        if (input$domainInput == "Snake River AVA (1km resolution)") { 
            brick(paste0("./AVA_WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                  crs = myCRS) }
        
        else if (input$domainInput == "Domain 02 (1km resolution)") { 
            brick(paste0("./WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                  crs = myCRS) }
        else if (input$domainInput == "Domain 01 (3km resolution)") { 
            brick(paste0("./WY",input$yearInput, "_yearly_stats_d01.nc"), varname = v(),
                  crs = myCRS) }})
    
    # set the extent of loaded file for proper projection
    rast1 <- eventReactive(input$button, { setExtent(rast(), myExtent) })
    
    # project to the leaflet lat/long grid 
    rast2 <- eventReactive(input$button, { 
        if ((input$varInput %in% varNamesLong[1:3]) == TRUE) {
            
            projectRasterForLeaflet(mean(rast1()[[input$dateInput[1]:input$dateInput[2]]], na.rm = T), method = "bilinear") }
        else {
            
            projectRasterForLeaflet(sum(rast1()[[input$dateInput[1]:input$dateInput[2]]]), method = "bilinear") }})
    
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
                      title = "2M Temp",labFormat = 
                          labelFormat(transform = function(x) sort(x, decreasing = TRUE)))})
    })
    
    # Kill the app when closed in the browser 
    session$onSessionEnded(stopApp)
}

shinyApp(ui, server)

