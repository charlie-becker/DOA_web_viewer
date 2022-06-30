##' Load Data Function
##' 
##' Load data and put them all in a reactiveValues object so that they can be 
##' passed into the functions as a single argument
##' 

load_data <- function(input, output, session) {
  # Create reactiveValues object 
  # similar to list but reactive
  dat <- reactiveValues()
  
  observe({
    #' load data for dyGraphs (AVA)
    dat$d <- data.table::fread('data/AVA_30YR_NoLeap917.csv')
  })
    observe({
    #' load for site specific "Download Data" Tab
    dat$site_specific_data <- data.table::fread('data/AVA_30YR_df_all_vars.csv')
  })
  observe({
    #' initial file for default map upon loading
    #' This is a netCDF file, which stores multidimensional data that can be 
    #' displayed as a raster
    #' starts with 1988 data
    dat$init_raster <- 'data/AVA_WY1988_yearly_stats_d02.nc'  
  })
  observe({
    #' get AVA Shape File  
    dat$jsonFile <- 'data/SR_AVA_simplified_pointRemove50m.json'
  })
  observe({
    #' convert JSON file to R list opject (this is an additional shape layer)
    dat$json <- fromJSON(file = dat$jsonFile)
  })
  observe({
    #' create a list of of non-leap year dates to be used for dygraphs (year will
    #' be stripped)
    dat$dates <- seq(as.Date('1987-01-01'), as.Date('1987-12-31'), 'day')
  })
  observe({
    #' Scale type for dygraphs
    dat$scaleType <- c('Individual Years', 'Monthly (across all years)', 'All years')
  })
  observe({
    #' Create month names
    dat$monNames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  })
  observe({
    #' get list of netCDF files (raw data)
    dat$ncFileNames <- list.files(pattern = '.nc', full.names = T, recursive = T)
  })
  observe({
    #' set variable names to pull from file and variable names to list on UI
    dat$ncVars <- stats::setNames(
      # actual values
      object = c("TMAX", "TMIN", "TMEAN", "GDD", "DPRCP", "DSNOW", 
        "FROSTD", "FROSTH"),
      # names
      # NOTE: these names will appear in the select input choices, but will not 
      # be maintained as part of the input$ object (ie input$variable will have a
      # value of 'TMAX', and won't retain the name 'Maximum Temperature')
      nm = c("Maximum Daily Temperature (C)","Minimum Daily Temperature (C)",
             "Mean Daily Temperature (C)", "Growing Degree Days",
             "Total Precipitation (mm)","Snowfall (mm)", "Frost Days", "Frost Hours")
    )
    
    # set variables that can be displayed by calendar year
    dat$calendarvars <- c('TMEAN', 'GDD', 'TMAX', 'TMIN')
  })
  
  observe({
    #' Define CRS from netCDF file (from WRF) and Extent (from GDAL)
    dat$wkt <- readr::read_file('data/crs_wkt.txt')
    dat$myCRS <- as.character(sp::CRS(dat$wkt))
    # This was originally the below proj4string, but proj4string has since been
    # deprecated, and wasn't working properly. I generated the above wkt by plugging
    # in the values from that proj4string into the wkt from this site: 
    # https://spatialreference.org/ref/sr-org/29/ . I copied that into a txt file
    # and then replaced the parameters with the values from the string below
    # This page gives a useful breakdown of how the components of the proj4string
    # are translated to a wkt: https://mgimond.github.io/Spatial/coordinate-systems-in-r.html#checking-for-a-coordinate-system
    # (Appendix Section G "Understanding the Proj4 coordinate syntax")
    # dat$myCRS <- sp::CRS("+proj=lcc +lat_1=44.299999 +lat_2=44.99999 +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs +type=crs")
    
    dat$myExtent <- c(-247739.5, 100260.5, -185023.9, 141976.1)
  })
  observe({
    #' Load raster and set projection and lat/lon extent
    #' this is for the maps on the explorer and download tabs
    dat$init_rast <- raster::brick(dat$init_raster, varname = 'GDD')
  })
  observe({
    # assign projection coord reference system
    projection(dat$init_rast) <- dat$myCRS
  })
  
  observe({
    # assign extent of initial raster
    extent(dat$init_rast) <- dat$myExtent
  })
  observe({
    #' Project to the leaflet lat/long grid and visualize      
    dat$r <- projectRasterForLeaflet(sum(dat$init_rast[[1:10]]), method = 'bilinear')
  })
  observe({
    #' set color palette
    dat$color_pal <- leaflet::colorNumeric(
      palette = c('dark red', 'light blue', 'dark green'),
      domain = raster::values(dat$r), na.color = 'transparent'
    )
  })
  observe({
    #' reverse the palette
    dat$rev_color_pal <- colorNumeric(rev(c('dark red', 'light blue', 'dark green')),
                                          domain = raster::values(dat$r), na.color = 'transparent')
  })
  observe({
    # create leaflet map with projected raster and marker located at 'Sunnyslope'
    dat$map <- leaflet() %>% addTiles() %>%
      # lng and lat set center point, zoom is zoom level
      leaflet::setView(lng = -116.5, lat = 43.8, zoom = 8) %>%
      # put marker at Sunnyslope
      addMarkers(lat = 43.5885, lng = -116.7932, label = as.character(round(dat$r[221,79],4))) %>%
      # add JSON file, not exactly sure what json is but we read that in at the beginning
      addGeoJSON(dat$json, weight = .5, color = 'black', fill = T, opacity = .5) %>%
      # add initial raster image (projected dat$init_raster)
      addRasterImage(dat$r, colors = dat$rev_color_pal, opacity = .7) %>%
      addLegend(pal = dat$color_pal, values = values(dat$r),
                title = 'Growing Degree Days', labFormat = 
                  labelFormat(transform = function(x) sort(x, decreasing = T)))
  }) # observe 
  observe({
    # initialize map2 in the same way; map2 is used in the download data tab
    dat$map2 <- dat$map
  })
  
  observe({
    # info about the map
    geoinfo <- ncdf4::nc_open(list.files(pattern = 'wrf_geoinfo.nc',
                                         recursive = T, full.names = T))
    # lat and long values for the grid cells, used when selecting a point on the 
    # map for which to download data (used in get_latlon(), defined in fct_helpers)
    dat$XLAT <- ncdf4::ncvar_get(geoinfo, 'XLAT')[,,1] 
    dat$XLONG <- ncdf4::ncvar_get(geoinfo, 'XLONG')[,,1]
    
    # set initial coordinates to Sunnyslope
    dat$coords <- c(79, 108)
  })
  
  observe({
    ## only run if changing how the summary table is laid out, and in that case 
    ## need to run manually (ie not in a shiny session)
    # run this, then first part of generate.summary.table() (run up to and including
    # where it binds all the individual tables into summtable)
    # airport.list <- lapply(dat$ncVars, function(x) {
    #   data <- extract.data(x, coords = c(124, 105), years = 1988:2017,
    #                        round.interval = 1,
    #                        yeartype = 'hydro')
    #   data
    # })
    # names(airport.list) <- dat$ncVars
    # ## summtable is generated from running the part of generate.summary.table()
    # write.csv(summtable, 'data/Airport_Table.csv')
    
    # this does need to be run every time
    # used for creating summary table
    dat$airport.table <- read.csv('data/Airport_Table.csv', row.names = 1) 
  })
  
  observe({
    strwidth <- 120
    # these are used when plotting variables
    dat$captiontext <- stats::setNames(
      object = c(
        # TMEAN
        # strwrap takes a string and inserts newlines so the chunk of text is a certain width
        strwrap('Mean Daily Temperature in degrees Celsius. Shaded area represents 
                the range between minimum and maximum daily temperature over the 
                same period.',
                width = strwidth) %>%
          paste(collapse = '\n'),
        # GDD
        strwrap('Growing degree days (GDD) are a measure of heat accumulation and 
                are a good predictor of plant and insect development rates. GDDs 
                are determined by  both the amount of time the temperature is above 
                a base temperature that  is required for growth (10°C/50°F for 
                the Snake River Valley AVA) and how much hotter it is than the 
                base temperature. The figure shows GDDs as they accumulate during 
                the year/growing season.',
                width = strwidth) %>%
          paste(collapse = '\n'),
        # DPRCP
        strwrap('Total precipitation in mm, includes rain and snowfall. Snowfall 
                is measured in "Snow Water Equivalent," which is the amount of 
                water that would be present if the snow were allowed to melt, and 
                is a metric commonly used by hydrologists.',
                width = strwidth) %>%
          paste(collapse = '\n'),
        # DSNOW
        strwrap('Snowfall in mm of Snow Water Equivalent, which is the amount of
                water that would be present if the snow were allowed to melt, and 
                is a metric commonly used by hydrologists.',
                width = strwidth) %>%
          paste(collapse = '\n'),
        # FROSTD
        strwrap('Days where frost is possible, shown as they accumulate throughout
                the hydrological year.',
                width = strwidth) %>%
          paste(collapse = '\n'),
        # FROSTH
        strwrap('Hours where frost is possible, shown as they accumulate throughout
                the hydrological year.',
                width = strwidth) %>%
          paste(collapse = '\n')
      ),
      nm = dat$ncVars[!(dat$ncVars %in% c('TMAX', 'TMIN'))]
    )
  })
  
  # return the whole object so that it can be called into other functions
  return(dat)
}
