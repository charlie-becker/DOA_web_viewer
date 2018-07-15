library(shiny)
library(leaflet)
library(raster)
library(rjson)

setwd("/Users/charlesbecker/Desktop/Data/Project Data/Shiny/")
ncFileNames <- list.files(full.names = T)

jsonFile <- "/Users/charlesbecker/Downloads/SR_AVA_simplified_pointRemove50m.json"

ncVarNames <- c("TMAX", "TMIN", "TMEAN", "GDD", "CUMPRCP", "DSNOW", "DHAIL", "FROSTD", "FROSTH")
varNamesLong <-  c("Maximum Temperature","Minimum Temperature","Mean Temperature", "Growing Degree Days",
                   "Total Precipitation","Snowfall", "Hail", "Frost Days", "Frost Hours")

# convert JSON file to R list object (this is just an additional shape layer)
json <- fromJSON(file = jsonFile)

# Grab the lat and lon from the data
lattitude <- raster(ncFileNames[1], varname="XLAT")
longitude <- raster(ncFileNames[1], varname="XLONG")

# Convert to points and match the lat and lons
pointLat <- rasterToPoints(lattitude)
pointLon <- rasterToPoints(longitude)
longLat <- cbind(pointLon[,3], pointLat[,3])

# Specify the lonlat as spatial points with projection as long/lat
longLat <- SpatialPoints(longLat, proj4string = CRS("+proj=longlat +datum=WGS84"))

# define CRS from netCDF file (from WRF)
myCRS <- CRS("+proj=lcc +lat_1=44.299999f +lat_2=44.99999f +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs")
plonlat <- spTransform(longLat, CRSobj = myCRS)

# Load raster and set projection and lat/lon extent
init_rast <- raster(ncFileNames[1], varname="TMAX")
projection(init_rast) <- myCRS
extent(init_rast) <- extent(plonlat)

# Project to the leaflet lat/long grid and visualize
r <- projectRasterForLeaflet(init_rast, method = "bilinear")

# set color palette
color_pal <- colorNumeric(c("dark red", "light blue", "dark green"), values(r),
                          na.color = "transparent")

rev_color_pal <- colorNumeric(rev(c("dark red", "light blue", "dark green")), values(r),
                              na.color = "transparent")

map = leaflet() %>% addTiles() %>%
    setView(lng = -116.5, lat = 43.8 ,zoom = 8) %>%
    addGeoJSON(json, weight = .5, color = "black", fill = F, opacity = 1) %>%
    addRasterImage(r, colors = rev_color_pal, opacity = .7) %>%
    addLegend(pal = color_pal, values = values(r),
              title = "2M Temp",labFormat = 
                  labelFormat(transform = function(x) sort(x, decreasing = TRUE))) 

################################################################################

ui <- fluidPage(
    titlePanel("Snake River Valley American Viticultural Area"),
    sidebarLayout(
        sidebarPanel(
            selectInput("yearInput", "Year", choices = 1996:1998, selected = "2007"),
            selectInput("varInput", "Variables", choices = varNamesLong, selected = varNamesLong[1]),
            sliderInput("dateInput", "Days of Water Year", min = 1, max = 365, value = c(1,10))),
        mainPanel(
            leafletOutput("myMap", width = "900", height = "650"),
            br(),br()
        )
    )
)

server = function(input, output, session) {
    
    output$myMap <- renderLeaflet(map)
    
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
    
    rast <- reactive ({ brick(paste0("./AVA_WY",input$yearInput, "_yearly_stats.nc"), varname = v(),
                              crs = "+proj=lcc +lat_1=44.299999f +lat_2=44.99999f +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs") })
    
    rast1 <- reactive ({ setExtent(rast(), extent(plonlat)) })
    
    #Project to the leaflet lat/long grid and visualize
    rast2 <- reactive ({ projectRasterForLeaflet(sum(rast1()[[input$dateInput[1]:input$dateInput[2]]]), method = "bilinear") })
    
    # set color palette
    color_pal <- reactive ({ colorNumeric(c("dark red", "light blue", "dark green"), values(rast2()),
                                          na.color = "transparent") })
    
    rev_color_pal <- reactive ({ colorNumeric(rev(c("dark red", "light blue", "dark green")), values(rast2()),
                                              na.color = "transparent") })
    
    observe({
        leafletProxy("myMap", data = rast2()) %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(rast2(),colors = rev_color_pal(), opacity = .7) %>%
            addLegend(pal = color_pal(), values = values(rast2()),
                      title = "2M Temp",labFormat = 
                          labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    })
    session$onSessionEnded(stopApp)
}

shinyApp(ui, server)

