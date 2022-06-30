##' Render DOWNLOAD DATA Tab
##' 
##' This function performs the server-side tasks for the Download Data tab

renderDownloadTab <- function(input, output, session, dat) {
  
  #' Update Input choices
  observe({
    updatePickerInput(session = session, inputId = 'downloadvars',
                      choices = dat$ncVars[!(dat$ncVars %in% c('TMAX', 'TMIN'))], 
                      selected = dat$ncVars[!(dat$ncVars %in% c('TMAX', 'TMIN'))])
  })
  
  #' Observe mouse clicks and get lat/lon
  #' the _click part is added automagically
  observeEvent(input$myMap2_click, {
    click <- input$myMap2_click
    clat <- click$lat
    clon <- click$lng
    
    leafletProxy('myMap2') %>%
      clearMarkers() %>%
      addMarkers(lng = clon, lat = clat) #%>%
    
    # updates the lat long displayed under the map
    updateNumericInput(session = session, 'latitude', value = click$lat)
    updateNumericInput(session = session, 'longitude', value = click$lng)
    
    # convert decimal degrees to raster grid cells 
    dat$coords <- get_latlon(dat, clat, clon)
  })
  
  #### Render Download Inputs ----
  output$downloadinputs <- renderUI({
    # the user selects to view the plots with the months on the x axis and a 
    # separate line for each year/set of years
    # (xmonth means month on the x axis)
    if (input$figuretype == 'xmonth') {
      tagList(
        h5('Select an interval to round by. 
            For example, if you select 
            5 years, each year will be rounded to the nearest 5 years 
            (1997 rounded to 1995, 1998 to 2000, etc) and values will 
            be averaged across those 5 years.'),
        
        # interval to round by
        selectInput(inputId = 'round.interval',
                    label = 'Interval',
                    choices = c('1 (Each year gets its own line)' = 1,
                                '2 (Years are lumped into sets of 2)' = 2,
                                '3' = 3, '4' = 4, '5' = 5, '10' = 10,
                                'all' = 30),
                    
                    selected = 5),
        
        radioButtons(inputId = 'yeartype',
                     label = 'Hydrological Year or Calendar Year for Temperature
                              and Growing Degree Days?\n(Other variables will all
                              be displayed by Hydrological Year, which starts in October).',
                     choices = stats::setNames(
                       object = c('hydro', 'calendar'),
                       nm = c('Hydro Year (starts October 1)', 'Calendar Year')
                     )),
      )
    } else if (input$figuretype == 'xyear') {
      # if the user selects to view the plots with years on the x axis and a different line for each month
      tagList(
        pickerInput(inputId = 'downloadmonths',
                    label = 'Select Month to View',
                    # month.name is a built in vector 
                    choices = month.name,
                    # default to all selected
                    selected = month.name,
                    multiple = T, 
                    # this adds the option to select/deselect all
                    options = list('actions-box' = T))
      )
    }
  })
 
  #### Download Full Report ----
  observeEvent(input$downloadDataBtn, {
    # warn the user that it will take a while (because in order to produce the 
    # report, have to load in the entire ncdf file for every year selected. The 
    # work all happens on hte server; the actual download is very fast so the user's 
    # internet speed doesn't matter)
    shiny::showModal(shiny::modalDialog(
      h3('Note: downloading the data and producing a report will take several minutes.
         If you still want to download the data, click the button below:'),
      fluidRow(column(4), 
               # download button for if they want to continue 
               column(4, downloadButton('downloadData', label = 'Download Data', 
                     class = 'btn-primary',
                     style = 'center-align')),
               column(4)),
      # I made this an action button because I couldn't figure out how to change 
      # the color of the cancel button
      footer = actionButton('removeDnldModal', 'Cancel and return to Download Page',
                            class = 'btn-danger')
    ))
  })
  
  observeEvent(input$removeDnldModal, removeModal())
  
  #### Full Report Download Handler ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('FullReport_', round(as.numeric(input$latitude), 4), '_', round(as.numeric(input$longitude), 4), '.pdf')
    },
    
    content = function(file) {
      
      if (input$yeartype == 'calendar' & 1988 %in% input$downloadyears & input$round.interval == 1) {
        shinyalert(title = 'Note: Any figures showing cumulative data (Growing Degree Days, Frost, Precipitation/Snow) for 1988 will
                           start in October because that is when data collection began for that year.',
                   type = 'warning')
      }
      shinybusy::show_modal_spinner()
      
      # this is all for the title at the top of the PDF file 
      # reassign names to variables, because the names are lost in the selectInput process
      varnames <- names(dat$ncVars)[which(dat$ncVars %in% input$downloadvars)]
      # paste them all together 
      varnames <- paste(varnames, collapse = ', ')
      # find.sequence defined in fct_helpers.R
      years <- find.sequence(as.numeric(input$downloadyears))
      # paste these all together into a string; strwrap keeps them from going off
      # the edge of the page
      yearstring <- paste('YEARS:\n', paste(strwrap(years, width = 40), collapse = '\n'))
      varstring <- paste('VARIABLES:\n', paste(strwrap(varnames, width = 40), collapse = '\n'))
      
      # print('creating pdf')
      
      # create pdf file
      pdf(file = file, height = 24, width = 16, onefile = T)
      
      # create title
      gridExtra::grid.arrange(
        grid::textGrob(label = paste(paste0('Data for ',
                                          round(as.numeric(input$latitude),4), ', ',
                                          round(as.numeric(input$longitude),4)), 
                               varstring, yearstring, 
                               sep = '\n\n'),
                 gp = grid::gpar(fontsize = 45)
        ),
        ncol = 1
      )
      shiny::withProgress(message = 'Building plots, creating PDF', value = 0, {
        
        # print('extracting tmin and tmax')
        # extract.data defined in DownloadTab_helpers
        # TMIN and TMAX are used when plotting the TMEAN variables
        tmindf <- extract.data('TMIN', coords = dat$coords, years = input$downloadyears,
                               round.interval = input$round.interval, 
                               yeartype = ifelse(input$yeartype == 'calendar', 
                                                'calendar', 'hydro'))
        
        tmaxdf <- extract.data('TMAX', coords = dat$coords, years = input$downloadyears,
                               round.interval = input$round.interval, 
                               yeartype = ifelse(input$yeartype == 'calendar', 
                                                'calendar', 'hydro'))
        
        # combine all plots into a list
        plots <- list()
        # print('extracted tmin and tmax')
        
        # run through variables to download
        for (i in 1:length(input$downloadvars)) {
          var <- input$downloadvars[i]
        # for (var in input$downloadvars) {}
          # print('in for loop')
          # print(var)
          
          # assign name again 
          varname <- names(dat$ncVars)[which(dat$ncVars == var)]
          
          df <- extract.data(var = var, coords = dat$coords, years = input$downloadyears,
                             round.interval = input$round.interval, 
                             yeartype = ifelse(var %in% dat$calendarvars & 
                                                 input$yeartype == 'calendar', 
                                               'calendar', 'hydro'))
          # print('extracted ')
          shiny::incProgress(1/length(input$downloadvars))
          
          # for each variable, create a plot and append it onto plots list
          # plot.data.xmonth and plot.data.xyear are defined in DownloadTab_helpers
          if (input$figuretype == 'xmonth') {
            # print('plotting')
            plots[[i]] <- plot.data.xmonth(var = var, df = df, ylab = varname,
                                   yeartype = ifelse((var %in% dat$calendarvars & 
                                                       input$yeartype == 'calendar'), 
                                                     'calendar', 'hydro'),
                                   tmaxdf = tmaxdf, tmindf = tmindf,
                                   caption = dat$captiontext[which(names(dat$captiontext) == var)]
                                   )
            
          } else {
            plots[[i]] <- plot.data.xyear(var = var, df = df, ylab = varname,
                                          months = input$downloadmonths)
          }
        }
        
        # marrange allows you to set the number of plots per page, but without a 
        # set number of plots 
        # have to print the plots for them to actually appear on the page
        marrangeGrob(grobs = plots, nrow = 3, ncol = 1) %>% print()
      })
      
      # this closes the connection with the pdf file 
      dev.off()
      shinybusy::remove_modal_spinner()
    }
  )
  
  #### Download Summary Report ----
  observeEvent(input$downloadSummaryBtn, {
    shiny::showModal(shiny::modalDialog(
      h3('Note: downloading the data and producing a report will take several minutes.
         If you still want to download the data, click the button below:'),
      fluidRow(column(4), 
               column(4, downloadButton('downloadSummary', label = 'Download Data', 
                                        class = 'btn-primary',
                                        style = 'center-align')),
               column(4)),
      footer = actionButton('removeSummModal', 'Cancel and return to Download Page',
                            class = 'btn-danger')
    ))
  })
  
  observeEvent(input$removeSummModal, removeModal())
  
  #### Summary Report Download Handler ----
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste0('SummaryReport_', round(as.numeric(input$latitude), 4), '_', round(as.numeric(input$longitude), 4), '.pdf')
    },
    
    content = function(file) {
      shinybusy::show_modal_spinner()
      
      # same as above; find.sequence defined in fct_helpers.R
      years <- find.sequence(as.numeric(input$downloadyears))
      yearstring <- paste('YEARS:\n', paste(strwrap(years, width = 60)))
    
      pdf(file = file, height = 16, width = 24, onefile = T)
      
      shiny::withProgress(message = 'Loading data, creating PDF', value = 0, {
      
        # extract data for all variables (need all variables to build summary tables)
        allvars <- lapply(dat$ncVars, function(x) {
          data <- extract.data(x, coords = dat$coords, years = input$downloadyears)
          shiny::incProgress(1/length(dat$ncVars))
          data
        })
        # not really sure why, but when allvars is returned from lapply the names
        # of the list elements are "Max Temperature" etc, not "TMAX" etc
        names(allvars) <- dat$ncVars
        
        # generate.summary.table defined in DownloadTab_helpers.R
        summary <- generate.summary.table(allvars, dat)
        # print('created allvars')
        gridExtra::grid.arrange(top = grid::textGrob(paste0('\nSummary Table for ', 
                                                                 round(input$latitude, 4),
                                    ', ', round(input$longitude, 4), '\n',
                                    yearstring), 
                                    gp = grid::gpar(fontsize = 36)),
                     grid::textGrob('Summarized Values, Compared to Boise Airport Weather Station\n',
                              gp = grid::gpar(fontsize = 24, fontface = 'bold')),
                     # this table is the summary values, and the difference between
                     # the values for the selected location and the airport weather station
                     gridExtra::tableGrob(summary$combined,
                               theme = gridExtra::ttheme_default(18)) ,
                     grid::textGrob(paste('\n\n\n\n', 
                                    strwrap('Values in parentheses 
                                            show the difference between the selected 
                                            location and the Boise Airport Weather 
                                            Station (43.56704°, -116.24053°). The differences 
                                            were calculated by subtracting the values 
                                            for the Boise Airport from the values for 
                                            the location you selected. For example, (-2) 
                                            in a temperature variable row means that your 
                                            location is 2 degrees colder than the Boise Airport.
                                            TMAX = Maximum Temp, TMEAN = Mean Temp,
                                            TMIN = Minimum Temp, DPRCP = Precipitation,
                                            PRCP Days = Days with Precip,
                                            DSNOW = Snowfall (in Snow Water Equivalent),
                                            FROSTH = Frost Hours, FROSTD = Days with Frost,
                                            GDD = Growing Degree Days',
                                      width = 190) %>% paste(collapse = '\n')
                                    ), # paste
                              gp = grid::gpar(fontsize = 18,
                                        fontface = 'italic',
                                        col = '#565656')),
                     grid::textGrob('Variance of Values\n',
                              gp = grid::gpar(fontsize = 24, fontface = 'bold')),
                     # table showing standard deviations
                     gridExtra::tableGrob(summary$sdtable,
                               theme = gridExtra::ttheme_default(18)),
                     grid::textGrob(paste('\n\n\n',
                                    strwrap('The values shown are the mean for that 
                                      variable +/- the standard deviation, which 
                                      is a common metric for measuring the variability 
                                      in a set of data.
                                      TMAX = Maximum Temp, TMEAN = Mean Temp,
                                      TMIN = Minimum Temp, DPRCP = Precipitation,
                                      PRCP Days = Days with Precip,
                                      DSNOW = Snowfall (in Snow Water Equivalent),
                                      FROSTH = Frost Hours, FROSTD = Days with Frost,
                                      GDD = Growing Degree Days',
                                      width = 190) %>% paste(collapse = '\n')
                                    ), # paste
                              gp = grid::gpar(fontsize = 18,
                                        fontface = 'italic',
                                        # grey
                                        col = '#565656')),
                     ncol = 1
        )
      })
      # print('printed to pdf')
      dev.off()
      
      
      shinybusy::remove_modal_spinner()
      beepr::beep()
    }
  )
  
  #### Download Full and Summary Report ----
  ## basically the two reports above but combined into a single report
  observeEvent(input$downloadSummaryDataBtn, {
    shiny::showModal(shiny::modalDialog(
      h3('Note: downloading the data and producing a report will take several minutes.
         If you still want to download the data, click the button below:'),
      fluidRow(column(4), 
               column(4, downloadButton('downloadSummaryData', label = 'Download Data', 
                                        class = 'btn-primary',
                                        style = 'center-align')),
               column(4)),
      footer = actionButton('removeSummDataModal', 'Cancel and return to Download Page',
                            class = 'btn-danger')
    ))
  })
  
  observeEvent(input$removeSummDataModal, removeModal())
  
  #### Full/Summary Report Download Handler
  output$downloadSummaryData <- downloadHandler(
    filename = function() {
      if (!valid(input$latitude) | !valid(input$longitude)) {
        shinyalert::shinyalert('You must click a point on the map')
        break()
      }
      paste0('FullandSummaryReports_', round(as.numeric(input$latitude), 4), '_', round(as.numeric(input$longitude), 4), '.pdf')
    },
    
    content = function(file) {
      
      shinybusy::show_modal_spinner()
      
      varnames <- names(dat$ncVars)[which(dat$ncVars %in% input$downloadvars)]
      varnames <- paste(varnames, collapse = ', ')
      years <- find.sequence(as.numeric(input$downloadyears))
      yearstring <- paste('YEARS:\n', paste(strwrap(years, width = 40), collapse = '\n'))
      varstring <- paste('VARIABLES:\n', paste(strwrap(varnames, width = 40), collapse = '\n'))
      
      pdf(file = file, height = 24, width = 16, onefile = T)
      # print('created pdf')
      gridExtra::grid.arrange(
        grid::textGrob(label = paste(paste0('Data for ',
                                      round(as.numeric(input$latitude),4), ', ',
                                      round(as.numeric(input$longitude),4)), 
                               varstring, yearstring, 
                               sep = '\n\n'),
                 gp = grid::gpar(fontsize = 45)
        ),
        ncol = 1
      )
      shiny::withProgress(message = 'Building plots, creating PDF', value = 0, {
        
        # in this case, extract all the variables, even if they're not all plotted 
        # individually, because they'll be needed for the summary table
        allvars <- lapply(dat$ncVars, function(x) {
          data <- extract.data(x, coords = dat$coords, years = input$downloadyears,
                               round.interval = input$round.interval,
                               yeartype = ifelse(x %in% dat$calendarvars &
                                                 input$yeartype == 'calendar',
                                                 'calendar', 'hydro'))
          shiny::incProgress(1/length(dat$ncVars))
          data
        })
        
        names(allvars) <- dat$ncVars
        # print(names(allvars))
        # 
        # print(allvars[['TMAX']])
        # print(allvars[['Frost Hours']])
        
        ## This next chunk is all mostly the same as in the full report section,
        ## except that it's subsetting allvars rather than extracting the data each time
        plots <- list()
        
        for (i in 1:length(input$downloadvars)) {
          var <- input$downloadvars[i]
          
          varname <- names(dat$ncVars)[which(dat$ncVars == var)]
          # print('extracted ')
          shiny::incProgress(1/length(input$downloadvars))
          
          ## subset allvars to remove TMAX and TMIN
          df <- allvars[3:8][[i]] %>%
            dplyr::filter(year %in% input$downloadyears)
          if (input$figuretype == 'xmonth') {
              print('plotting data')
            plots[[i]] <- plot.data.xmonth(var = var, df = df, ylab = varname,
                                           yeartype = ifelse((var %in% dat$calendarvars & 
                                                                input$yeartype == 'calendar'), 
                                                             'calendar', 'hydro'),
                                           tmaxdf = allvars[[1]], tmindf = allvars[[2]],
                                           caption = dat$captiontext[which(names(dat$captiontext) == var)]
            )
            print('finished plotting')
          } else {
              print('in else')
            plots[[i]] <- plot.data.xyear(var = var, df = df, ylab = varname,
                                          months = input$downloadmonths)
          }
        }
        print('printing plots')
        marrangeGrob(grobs = plots, nrow = 3, ncol = 1) %>% print()
        print('printing summary table')
        
        # generate.summary.table defined in DownloadTab_helpers.R
        summary <- generate.summary.table(allvars, dat)
        
        grid.arrange(top = textGrob(paste0('\n\nSummary Table for ', round(input$latitude, 4),
                                           ', ', round(input$longitude, 4), '\n',
                                           yearstring), 
                                    gp = gpar(fontsize = 36)),
                     textGrob('Summarized Values, Compared to Boise Airport Weather Station\n',
                              gp = gpar(fontsize = 24, fontface = 'bold')),
                     tableGrob(summary$combined,
                               theme = ttheme_default(12)) ,
                     textGrob(paste('', 
                                    strwrap('Values in parentheses 
                                            show the difference between the selected 
                                            location and the Boise Airport Weather 
                                            Station (43.56704°, -116.24053°). The differences 
                                            were calculated by subtracting the values 
                                            for the Boise Airport from the values for 
                                            the location you selected. For example, (-2) 
                                            in a temperature variable row means that your 
                                            location is 2 degrees colder than the Boise 
                                            Airport.
                                            TMAX = Maximum Temp, TMEAN = Mean Temp,
                                            TMIN = Minimum Temp, DPRCP = Precipitation,
                                            PRCP Days = Days with Precip,
                                            DSNOW = Snowfall (in Snow Water Equivalent),
                                            FROSTH = Frost Hours, FROSTD = Days with Frost,
                                            GDD = Growing Degree Days',
                                            width = 120) %>% paste(collapse = '\n')
                     ), # paste
                     gp = gpar(fontsize = 18,
                               fontface = 'italic',
                               col = '#565656')),
                     textGrob('Variance of Values\n',
                              gp = gpar(fontsize = 24, fontface = 'bold')),
                     tableGrob(summary$sdtable,
                               theme = ttheme_default(12)),
                     textGrob(strwrap('The values shown are the mean for that 
                                      variable +/- the standard deviation, which 
                                      is a common metric for measuring the variability 
                                      in a set of data.
                                      TMAX = Maximum Temp, TMEAN = Mean Temp,
                                      TMIN = Minimum Temp, DPRCP = Precipitation,
                                      PRCP Days = Days with Precip,
                                      DSNOW = Snowfall (in Snow Water Equivalent),
                                      FROSTH = Frost Hours, FROSTD = Days with Frost,
                                      GDD = Growing Degree Days',
                                      width = 120) %>% paste(collapse = '\n'),
                     gp = gpar(fontsize = 18,
                               fontface = 'italic',
                               col = '#565656')),
                     ncol = 1)

      })
      
      dev.off()
      
      pdftools::pdf_info(file)
      shinybusy::remove_modal_spinner()
    }
  )
}

