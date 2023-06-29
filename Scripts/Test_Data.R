#### Test Objects
#### Create lists called input, output, and dat so that I can test parts of the
#### app outside of a shiny session

input <- list()

input$yearInput <- 2000
input$varInput <- 'Snowfall'
input$varInput1 <- 'Frost Days'

input$dateInput <- c(1, 20)

output <- list()
dat <- list()

dat$d <- data.table::fread('data/AVA_30YR_NoLeap917.csv')

#' load for site specific "Download Data" Tab
dat$site_specific_data <- data.table::fread('data/AVA_30YR_df_all_vars.csv')

#' initial file for default map upon loading
#' This is a netCDF file, which stores multidimensional data that can be 
#' displayed as a raster
#' starts with 1988 data
print('loading dat$init_raster')
dat$init_raster <- 'data/AVA_WY1988_yearly_stats_d02.nc'  

#' get AVA Shape File  
dat$jsonFile <- 'data/SR_AVA_simplified_pointRemove50m.json'

#' convert JSON file to R list opject (this is an additional shape layer)
dat$json <- fromJSON(file = dat$jsonFile)

#' create a list of of non-leap year dates to be used for dygraphs (year will
#' be stripped)
dat$dates <- seq(as.Date('1987-01-01'), as.Date('1987-12-31'), 'day')

#' Scale type for dygraphs
dat$scaleType <- c('Individual Years', 'Monthly (across all years)', 'All years')

#' Create month names
dat$monNames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#' get list of netCDF files (raw data)
dat$ncFileNames <- list.files(pattern = '.nc', full.names = T, recursive = T)

#' set variable names to pull from file and variable names to list on UI
dat$ncVarNames <- stats::setNames(
  object = c("TMAX", "TMIN", "TMEAN", "GDD", "DPRCP", "DSNOW", 
             "FROSTD", "FROSTH"),
  nm = c("Maximum Daily Temperature","Minimum Daily Temperature",
         "Mean Daily Temperature", "Growing Degree Days",
         "Total Precipitation","Snowfall", "Frost Days", "Frost Hours")
)

#' Define CRS from netCDF file (from WRF) and Extent (from GDAL)
# dat$myCRS <- sp::CRS("+proj=lcc +lat_1=44.299999 +lat_2=44.99999 +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs")
# dat$myCRS <- sf::st_crs(26911)$wkt # EPSG code 26911 https://epsg.io/26911-1716
dat$wkt <- readr::read_file('data/crs_wkt.txt')
dat$myCRS <- sp::CRS(dat$wkt)
dat$myExtent <- c(-247739.5, 100260.5, -185023.9, 141976.1)

#### EXPLORER TAB ----

#' Load raster and set projection and lat/lon extent
print('loading dat$init_rast')
dat$init_rast <- brick(dat$init_raster, varname = 'GDD')

projection(dat$init_rast) <- dat$myCRS

extent(dat$init_rast) <- dat$myExtent

#' Project to the leaflet lat/long grid and visualize
dat$r <- projectRasterForLeaflet(sum(dat$init_rast[[1:10]]), method = 'bilinear')

#' set color palette
dat$color_pal <- leaflet::colorNumeric(
  palette = c('dark red', 'light blue', 'dark green'),
  domain = raster::values(dat$r), na.color = 'transparent'
)

#' reverse the palette
dat$rev_color_pal <- colorNumeric(rev(c('dark red', 'light blue', 'dark green')),
                                  domain = raster::values(dat$r), na.color = 'transparent')

# create leaflet map with projected raster and marker located at 'Sunnyslope'
dat$map <- leaflet() %>% addTiles() %>%
  setView(lng = -116.5, lat = 43.8, zoom = 8) %>%
  addGeoJSON(dat$json, weight = .5, color = 'black', fill = T, opacity = .5)

dat$v <- dat$ncVarNames[match(input$varInput, dat$varNamesLong)]

#### TIME SERIES TAB ----

input$scaleInput <- 'All years'
input$varInput1 <- dat$ncVarNames[7]
input$yearInput1 <- '2016'

if(input$varInput1 %in% c('TMAX', 'TMIN', 'TMEAN')) {
  dd <- as.data.frame(
    dplyr::bind_cols( split(dat$d[[input$varInput1]], dat$d$YEAR) ) %>% 
      dplyr::select(input$yearInput1)
  ) 
} else {
  dd <- cumsum(as.data.frame(bind_cols(split(dat$d[[input$varInput1]], dat$d$YEAR)) %>%
                               select(input$yearInput1)))
}

df <- data.frame(dat$dates, dd)

df_dy <- xts(df, order.by = dat$dates)
# renderDygraph({
  dygraph(df_dy) %>%
    dyRangeSelector() %>%
    # dyAxis(name = 'x', axisLabelFormatter = htmlwidgets::JS(getMonth),
    #        valueFormatter = htmlwidgets::JS(getMonthDay))
    dyAxis(name = 'x', axisLabelFormatter = htmlwidgets::JS(getMonth)[[1]])
    
# })
# 
#### DOWNLOAD TAB ----

input$latitude <- 43.48481
input$longitude <- -116.4227
dat$site_specific_data

dat$stats_1988 <- data.table::fread('data/AVA')



