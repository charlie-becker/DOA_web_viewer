##' Render EXPLORER Tab 
##' 
##' This function performs the server-side tasks for the Explorer tab

renderExplorerTab <- function(input, output, session, dat) {
  # update varInput and monInput objects 
  # done server-side because they use the dat object
  observe({
    updateSelectInput(session, 'varInput', choices = dat$ncVars)
    updateSelectInput(session, 'monInput', choices = dat$monNames)
  })
  
  # when the user updates the slider, updates date range displayed 
  observeEvent(list(input$dateInput, input$yearInput), {
    # data are based on hydro year, so need to start with October 1
    year <- lubridate::ymd(paste0(input$yearInput, '-10-01'))
    updateDateRangeInput(session = session, inputId = 'myDate',
                         start = year + input$dateInput[1],
                         end = year + input$dateInput[2])
  })
  

  # render default leaflet map 
  # This is the initial map set up in load_data
  output$myMap <- renderLeaflet(dat$map)

  # render leaflet map ('Download Data' Tab)
  output$myMap2 <- renderLeaflet({dat$map2 %>%
      # addMouseCoordinates() makes it so coordinates appear at the top of the map
      # as you mouse around
      addMouseCoordinates() #%>%
      # addMarkers(lng = -116.24053, lat = 43.56704)#, #label = 'Boise Airport',
                 # icon = icons(
                 #   iconUrl = 'https://leafletjs.com/SlavaUkraini/examples/custom-icons/leaf-green.png',
                 #   # iconUrl = 'https://pngtree.com/freepng/vector-airplane-icon_4277510.html',
                 #   iconWidth = 38, iconHeight = 95))
  })

  #' Load initial leaflet map in background for Spatial Explorer
  #' (loads even while another tab is selected)
  outputOptions(output, 'myMap', suspendWhenHidden = F)

  #' Load initial leaflet map in background for Data Download
  outputOptions(output, 'myMap2', suspendWhenHidden = F)

  # when user clicks 'Create Map'
  # Note: this also updates the download data tab map
  rast <- eventReactive(input$button, {
    if (input$plotInput == "Historical") {
      if (input$domainInput == "Snake River AVA (1km resolution)") {
        #' raster::brick grabs a netcdf file and turns it into a raster
        #' first arg is the filename to get
        #' varname is the variable to grab from the file
        #' crs is coordinate reference system (how to map data)
          raster::brick(paste0("data/AVA_WY",input$yearInput, "_yearly_stats_d02.nc"),
                        varname = input$varInput, crs = dat$myCRS)
      } else if (input$domainInput == "Greater Idaho (1km resolution)") {
          raster::brick(paste0("data/WY",input$yearInput, "_yearly_stats_d02.nc"),
                        varname = input$varInput,
                        crs = dat$myCRS)
      } else if (input$domainInput == "Greater Pacific Northwest (3km resolution)") {
          raster::brick(paste0("data/WY",input$yearInput, "_yearly_stats_d02.nc"),
                        varname = input$varInput,
                        crs = dat$myCRS)
      }
    } else if (input$plotInput == "Yearly Anomaly") {
        if (input$domainInput == "Snake River AVA (1km resolution)") {
            raster::brick("data/AVA_Yearly_Anomalies_d02.nc",
                          varname = input$varInput, crs = dat$myCRS)
        } else if (input$domainInput == "Greater Idaho (1km resolution)") {
            raster::brick("data/Yearly_Anomalies_d02.nc",
                          varname = input$varInput, crs = dat$myCRS)
        }
    } else if (input$plotInput == "Monthly Anomaly") {
        if (input$domainInput == "Snake River AVA (1km resolution)") {
            raster::brick(paste0("data/", input$monInput, "_AVA_Anomalies_d02.nc"),
                          varname = input$varInput, crs = dat$myCRS)
        } else if (input$domainInput == "Greater Idaho (1km resolution)") {
            raster::brick(paste0("data/", input$monInput, "_Anomalies_d02.nc"),
                          varname = input$varInput, crs = dat$myCRS)
        }
    }
  })

  #' This also happens when 'Create Map' is clicked
  #' set the extent of loaded file for proper projection
  #' I think this cuts it off at the x and y bounds
  rast1 <- eventReactive(input$button, {
    raster::setExtent(rast(), dat$myExtent)
  })

  #' project to the leaflet lat/long grid based on conditions
  rast2 <- eventReactive(input$button, {
    #' if historical
    if (input$plotInput == 'Historical') {
      # if selected temperature, use mean instead of sum
      if (input$varInput %in% dat$ncVars[1:3]) {
        leaflet::projectRasterForLeaflet(
          mean( rast1()[[input$dateInput[1]:input$dateInput[2]]], na.rm = T ),
          # method assigns how to compute values
          method = 'bilinear'
        )

      } # if
      # else use sum
      else {
        leaflet::projectRasterForLeaflet(
          sum(rast1()[[input$dateInput[1]:input$dateInput[2]]]),
          method = 'bilinear'
        )
      } # else
    } # if
    # else if yearly or monthy
    else if (input$plotInput %in% c('Yearly Anomaly', 'Monthly Anomaly')) {
      leaflet::projectRasterForLeaflet(
        rast1()[[as.integer(input$yearInput) - 1987]] - rast1()[[31]],
        method = 'bilinear'
      )
    }
  }) # eventReactive

  # set color palette
  # I'm not totally sure how the color palette stuff works, but basically it 
  # assigns a color range based on the values
  color_pal <- eventReactive(input$button, {
    colorNumeric(palette = c('dark red', 'light blue', 'dark green'), 
                 # domain sets the values for the palette
                 domain = raster::values(rast2()),
                 na.color = 'transparent')
  }) # eventReactive

  rev_color_pal <- eventReactive(input$button, {
    colorNumeric(palette = rev(c('dark red', 'light blue', 'dark green')), 
                 domain = raster::values(rast2()),
                 na.color = 'transparent')
  }) # eventReactive

  # create proxy leaflet map (from original map)
  observeEvent(input$button, {
    print(base::names(input$varInput))
    ## ANY CHANGES TO ONE OF THESE NEEDS TO BE MADE TO BOTH
    withBusyIndicatorServer('button', {
      # leaflet proxy creates map-like object that can be used to modify a map
      # that has already been rendered
      leafletProxy('myMap', data = rast2()) %>%
        # remove any existing elements from map
        clearImages() %>%
        clearControls() %>%
        # add raster data
        addRasterImage(rast2(), colors = rev_color_pal(), opacity = .7, project = F) %>%
        # add sunnyslope marker
        addMarkers(lat = 43.5885, lng = -116.7932,
                   label = as.character(round(rast2()[221,79], 2))) %>%
        # legend 
        addLegend(pal = color_pal(), values = terra::values(rast2()),
                  # grab the full variable name that matches the input value
                  title = names(dat$ncVars)[which(dat$ncVars == input$varInput)],
                  # sort values in decreasing order
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = T)))
    }) # withBusyIndicatorServer
    # update the download tab in the same way that the explorer tab map is updated
    # (I tried to use the same leaflet object to render both maps so that I can 
    # change bothmaps simultaneously, but I couldn't figure it out)
    leafletProxy('myMap2', data = rast2()) %>%
      clearImages() %>%
      clearControls() %>%
      # add raster data
      addRasterImage(rast2(), colors = rev_color_pal(), opacity = .7, project = F) %>%
      # add sunnyslope marker
      addMarkers(lat = 43.5885, lng = -116.7932,
                 label = as.character(round(rast2()[221,79], 2))) %>%
      addLegend(pal = color_pal(), values = terra::values(rast2()),
                title = names(dat$ncVars)[which(dat$ncVars == input$varInput)],
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = T)))
  }) # observeEvent
}




