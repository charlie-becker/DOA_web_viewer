#### Helper Functions for Download Data Tab

# source('Scripts/Load_Packages.R')

## Open ncdf files and read data from them
## I thought about having this all happen when the user loads the app, but it 
## would've taken forever and I figured might as well not make the mgo through
## that if they're not actually downloading any data
## The defaults are never used in the actual app, but it was easier for me when
## I was testing to have defaults added in 
extract.data <- function(var = 'TMAX', coords = c(1,1), years = 1988:1990,
                         round.interval = 1, yeartype = 'hydro') {
  # print(round.interval)
  # print(input$yeartype)
  files <- list.files('data/', pattern = '^WY.*_yearly_stats_d02.nc', full.names = T)
  # if(yeartype == 'calendar') {
  #   # print('calendar year')
  #   years.save <- years
  #   for (i in years) {
  #     # print(i)
  #     i <- as.numeric(i)
  #     
  #     if (!((i - 1) %in% years) & i != 1988) years <- c(years, i - 1)
  #     if (!((i + 1) %in% years) & i != 2017) years <- c(years, i + 1)
  #   }
  # }
  # print('added new years')
  
  # subset only the files for the years the user selected
  # adding the | in allows you to look for a bunch of patterns
  # ie, pastes 1988:1990 into "1988|1989|1990" and searches for any of those values
  # in the list of files
  files <- files[grep(paste(years, collapse = '|'), files)]
  df <- lapply(files, function(x) {
    # print(paste('extracting', x))
    # open file
    nctemp <- ncdf4::nc_open(x)
    # extract variable, and subset it for the relevant coordinates
    tmp <- ncdf4::ncvar_get(nctemp, var)[coords[1],coords[2],] %>%
      as_tibble() %>%
      # extract the year from the filename
      # x is the filename; ie "WY.1988_yearly_stats_d02.nc"
      dplyr::mutate(hydroyear = stringr::str_extract(x, '\\d{4}') %>% as.numeric(),
                    # hydro year
                    hydroday = dplyr::row_number(),
                    # for some reason need to jump back a day becaues as.Date moves 
                    # you forward one day
                    date = as.Date(hydroday - 1, origin = paste0(hydroyear, '-10-01')),
                    # This is different than hydroday, because for hydroday, Oct 1
                    # is day 1, whereas for julianday Jan 1 is day 1
                    # Once again, need to add a day for some reason
                    julianday = as.POSIXlt(date)$yday + 1,
                    # extract the month from the date 
                    month = months(date),
                    # extract calendar year
                    calyear = data.table::year(date)
                   )
    # print('assigning hydro or calendar year')
    # year and day are used later, so by assigning them here based on yeartype, 
    # don't need to discriminate later on
    if (yeartype == 'hydro') {
      tmp$year <- tmp$hydroyear
      tmp$day <- tmp$hydroday 
    } else {
      tmp$year <- tmp$calyear
      tmp$day <- tmp$julianday
    }
    # print('assigned year')
    # Round based on the user-selected rounding interval
    tmp$interval <- plyr::round_any(as.numeric(tmp$year), as.numeric(round.interval), f = floor)
    # print(head(tmp$interval))
    # print('assigned interval')
    
    # account for leap years; also the last day of the year sometimes does weird 
    # stuff and since this was just a rough depiction, it didn't have to be quite 
    # as exact
    # 
    tmp %<>% filter(day %in% 1:364)
    # print('filtered days')
    # print('filtered years')
    tmp
  }) %>%
    # bind all the list elements into one data frame that contains the data for
    # every year for the given variable 
    base::do.call('bind_rows', .) %>%
    # group them by the rounding interval (at this point, the value is just a year;
    # if the rounding interval is 5 years, then the intervals will be 1990, 1995, 2000, etc)
    group_by(interval) %>% 
    # Now create the label that will actually be displayed; ie for the interval 
    # 1990, the label would be 1988-1992
    # If the interval is one, then the label will just be the year
    mutate(label = ifelse(round.interval == 1, year, 
                          ifelse(min(year) != max(year), 
                                 paste(base::min(year, na.rm = T),
                                       base::max(year, na.rm = T), sep = '-'),
                                 year)
                         ) %>% as.character()
          ) %>%
    ungroup()
  # print(head(df$label))
  return(df)
} 



#####################################


# Creates plots if the user wants figures with the months on the bottom and a line
# for every year
plot.data.xmonth <- function(var = 'TMAX', df, ylab = var, yeartype = 'hydro',
                             tmindf, tmaxdf, captiontext) {
  # print('in plot.data')
  # print(var)
  # print(ylab)
  
  # Avg of 30.41667 days in each month; this creates intervals at which to place 
  # tick marks 
  midmonth <- seq(1,365,30.41667) %>% round()
  # order if the plot is based on the hydro year
  hydromonths <- c('October', 'November', 'December','January', 'February', 'March', 
                   'April', 'May', 'June', 'July', 'August', 'September')

  # month.name is a built in vector 
  if (yeartype == 'hydro') months <- hydromonths else months <- month.name
  
  # Temperature variables (these are displayed as the value for each day, and if 
  # the rounding interval is not 1, then the mean is taken for a given day across
  # all years)
  tempvars <- c('TMIN', 'TMAX', 'TMEAN')
  # Non-temperature variables (these are displayed as cumulative throughout the 
  # year, and if the rounding interval is not one, then the mean is taken first 
  # for each day across the years in a given interval, and then those mean values
  # are summed throughout the year)
  cumvars <- c('GDD', 'DPRCP', 'DSNOW', 'FROSTD', 'FROSTH')
  
  ## Assign second label for line type
  
  if (var %in% tempvars) {
    df2 <- df %>%
      # some of the values are absolute zero, so I filtered these out
      filter(value > -100)  %>%
      # group by the bins created by the rounding, and then group by day within 
      # those bin
      group_by(label, day) %>%
      # take the mean for that day
      summarize(summval = mean(value)) 
    
    # TMIN and TMAX are displayed like a confidence interval around TMEAN, so
    # group and average these in the same way
    tmindf2 <- tmindf %>%
      filter(value > -100) %>%
      group_by(label, day) %>%
      summarize(summval = mean(value))
    
    tmaxdf2 <- tmaxdf %>%
      filter(value > -100) %>%
      group_by(label, day) %>%
      summarize(summval = mean(value))
    
  } else if (var %in% cumvars) {
    df2 <- df %>%
      group_by(label, day) %>%
      # take the mean first
      summarize(summval = mean(value)) %>%
      # then sum them throughout the year
      mutate(summval = cumsum(summval))
  }
  
  ## generate second label for varying line type
  ## This is leftover from when I was going to try to vary lines both by color and
  ## type in the event that the user wants to see all 30 years displayed individually
  ## but I gave up and instead made the default rounding interval 5 years. It could 
  ## still be useful if someone wants to add it in in the future, so I'm leaving 
  ## this in but commenting it out.
  
  # labels <- unique(df2$label)
  # labeldf <- data.frame(label = labels,
  #                       label2 = rep(1:5,6)[1:length(labels)] %>% as.factor(),
  #                       label3 = rep(1:5,6)[1:length(labels)] %>% as.factor())

  # df2 <- full_join(df2, labeldf, by = 'label')
  # # tmaxdf2 <- full_join(tmaxdf2, labeldf, by = 'label')
  # # tmindf2 <- full_join(tmindf2, labeldf, by = 'label')
  
  # Min and max values to be used when building the "confidence interval"
  if (var %in% tempvars) {
    df2$minval <- tmindf2$summval
    df2$maxval <- tmaxdf2$summval
  }
  
  # print('passed ifs in plot.data')
  plot <- ggplot(df2, aes(x = day, y = summval, color = label)) +
    # Add tick marks and labels
    scale_x_continuous(breaks = midmonth,
                       labels = months) +
    # format axis labels
    theme(axis.text.x = element_text(angle = 30, vjust = , hjust = 1)) +
    # ylab is "Mean Temperature" etc
    ylab(ylab) +
    xlab('Month') +
    # title is same as ylabel
    labs(title = ylab,
         # add caption, this is pulled from dat$captiontext in Load_Data.R
         caption = captiontext) + 
    # Format titles and legend
    theme(plot.title = element_text(size = 36, face = 'bold', hjust = .5),
          axis.title = element_text(size = 24, face = 'bold'),
          axis.text = element_text(size = 20, face = 'bold'),
          legend.title = element_blank(),
          legend.text = element_text(size = 20, face = 'bold'),
          plot.caption = element_text(size = 16, hjust = 0.5, face = 'italic'),
          # legend.key.size = unit(10, 'line'),
          plot.margin = margin(25,25,25,25)) + 
    # this changes the size of the color blocks displayed in the legend
    guides(color = guide_legend(override.aes = list(size = 4)))
  # Add lines
  # if TMAX, add the min and max as confidence interval
  if (var %in% tempvars) {
    # geom_ribbon adds that interval to the plot
    print('var in tempvars')
    plot + geom_ribbon(data = df2 %>%
                         group_by(label) %>%
                         # This part is required so it does the min/max for each
                         # label bin, otherwise it calculates a single value for 
                         # each day across all years
                         mutate(ymin_smooth = stats::predict(loess(minval ~ day)),
                                ymax_smooth = stats::predict(loess(maxval ~ day))),
                       aes(ymin = ymin_smooth, ymax = ymax_smooth, fill = label),
                       alpha = 0.1) +
    # Add the lines; geom_smooth makes it less busy
    geom_smooth(size = .75, se = FALSE)
  } else {
     print('var not in tempvars')
    # if not TMAX, add the line without any smoothing because the lines are a lot 
    # less busy
    plot + geom_line(size = .75)#, aes(linetype = df2$label2, color = df2$label))
  }
}


#####################################

## Create plots with year on the x axis, and a line for each month
## As with plot.data.xmonth, the defaults are never used in the app, but made it
## easier for testing. 
plot.data.xyear <- function(var = 'TMAX', df = df, ylab = var, 
                            months = c('January', 'February')) {
  # print(ylab)
  df %<>% 
    filter(month %in% months)
    # mutate(year = as.character(year))
  
  tempvars <- c('TMIN', 'TMAX', 'TMEAN')
  cumvars <- c('GDD', 'DPRCP', 'DSNOW', 'FROSTD', 'FROSTH')
  
  # get rid of really low values, for some reason there are some that are -273 C
  if (var %in% tempvars) {
    df2 <- df %>%
      filter(value > -100)  %>%
      group_by(month, year) %>%
      # take the mean across a given month
      summarize(summval = mean(value)) 
    
  } else if (var %in% cumvars) {
    df2 <- df %>%
      group_by(month, year) %>%
      # cumulative values, then take the mean of those
      mutate(cumval = cumsum(value)) %>%
      summarize(summval = mean(cumval))
  }
  
  df2$month <- factor(df2$month, levels = month.name)
  
  # df2 <- df %>%
  #   filter(value > -100) %>%
  #   group_by(month, year) %>%
  #   summarise(summval = mean(value))
  # print(df$year %% 2)
  
  # print(ifelse(length(unique(df$year)) > 15,
  #              unique(df$year)[which(unique(df$year) %% 2 == 0)],
  #              df$year))
  #              
  if (length(unique(df$year)) > 15) {
    yearvec <- unique(df$year)[which(unique(df$year) %% 2 == 0)]
  } else yearvec <- unique(df$year)
  
  print(yearvec)
  
  ggplot(df2, aes(x = year, y = summval, color = month)) +
    geom_line(size = .75) + 
    scale_x_continuous(breaks = yearvec,
                       labels = yearvec) +
    # scale_x_continuous(breaks = df$year)
    ylab(ylab) + 
    xlab('Year') +
    # title is same as ylabel
    labs(title = ylab,
    # add caption, this is pulled from dat$captiontext in Load_Data.R
    caption = captiontext) + 
    theme(plot.title = element_text(size = 36, face = 'bold', hjust = .5),
          axis.title = element_text(size = 24, face = 'bold'),
          axis.text = element_text(size = 20, face = 'bold'),
          legend.title = element_blank(),
          legend.text = element_text(size = 20, face = 'bold'),
          plot.margin = margin(25,25,25,25)) +
    # change size of color blocks in legend
    guides(color = guide_legend(override.aes = list(size = 4)))
}


#####################################

# This table mimics the table found if you go to Boise or any other city's 
# wikipedia page and go to the climate/weather section
# It shows average values for each month, averaged over all 30 years of data (or 
# a subset of those years if the user selects different years)
# 
# allvars is a list of dataframes for all the variables
generate.summary.table <- function(allvars, dat) {

  # For each variable, get the summary value (usually the mean), and the standard
  # deviation (for min and max temp, can't do standard deviation because it's 
  # taking the lowest temperature of any year, rather than an average)
  
  # print('in generate.summary.table')
  maxtemp <- allvars$TMAX %>%
    # remove weird values
    filter(value > -100) %>%
    group_by(month) %>%
    summarise(`Max Temp` = max(value), # Maximum temp from all 30 years
              `Avg High` = mean(value),
              `SD Max Temp` = 'NA',
              `SD Avg High` = paste0((`Avg High` - sd(value)), ' - ',
                                    (`Avg High` + sd(value))))  # Average across all 30 years
    
  
  meantemp <- allvars$TMEAN %>%
    filter(value > -100) %>%
    group_by(month) %>%                
    summarise(`Daily Mean` = mean(value),
              `SD Daily Mean` = paste0((`Daily Mean` - sd(value)), ' - ',
                                       (`Daily Mean` + sd(value))))  # Mean across all 30 years
    
  
  
  mintemp <- allvars$TMIN %>%
    filter(value > -100) %>%
    group_by(month) %>%
    summarise(`Minimum Temp` = min(value), # Min temp from all 30 years
              `Avg Low` = mean(value),
              `SD Min Temp` = 'NA',
              `SD Avg Low` = paste0((`Avg Low` - sd(value)), ' - ',
                                    (`Avg Low` + sd(value))))  # Mean across all 30 years
    
  
  
  prcp <- allvars$DPRCP %>%
    group_by(month, year) %>% 
    summarise(prcpdays = sum(value > .01),
              value = mean(value)) %>%
    summarise(`Mean Daily Precip (mm)` = mean(value), # Mean across all 30 years
              `Precip Days` = mean(prcpdays), # mean number of days each month with precip
              `SD Mean Daily Precip` = paste0((mean(value) - sd(value)), ' - ',
                                              (mean(value) + sd(value))),
              `SD Precip Days` = paste0((mean(prcpdays) - sd(prcpdays)), ' - ',
                                        (mean(prcpdays) + sd(prcpdays))))  # mean number of days each month with precip
              
  
  
  
  snow <- allvars$DSNOW %>%
    group_by(month, year) %>%
    summarise(snowdays = sum(value > .1),
              value = mean(value)) %>%
    summarise(`Mean Daily Snow (cm)` = mean(value),
              `Snow Days` = mean(snowdays), # mean number of days each month with precip
              `SD Mean Daily Snow` = paste0((mean(value) - sd(value)), ' - ',
                                            (mean(value) + sd(value))),
              `SD Snow Days` = paste0((mean(snowdays) - sd(snowdays)), ' - ',
                                      (mean(snowdays) + sd(snowdays)))) 
    
  
  
  frosth <- allvars$FROSTH %>%
    group_by(month, year) %>%
    summarise(value = mean(value),
              totalfrosth = sum(value)) %>%
    summarise(`Frost Hours Per Day` = mean(value),
              `Total Frost Hours` = sum(totalfrosth), 
              `SD Frost Hours Per Day` = paste0(mean(value) - sd(value), ' - ',
                                                mean(value) + sd(value)),
              `SD Total Frost Hours` = paste0(sum(totalfrosth) - sd(totalfrosth), ' - ',
                                                sum(totalfrosth) + sd(totalfrosth))
              ) 
    
  
  
  frostd <- allvars$FROSTD %>%
    group_by(month, year) %>%
    summarise(frostdays = sum(value)) %>%
    summarise(`Frost Days` = mean(frostdays),
              `SD Frost Days` = paste0(mean(frostdays) - sd(frostdays), ' - ',
                                       mean(frostdays) + sd(frostdays)))#  %>%
    
  
  
  gdd <- allvars$GDD %>%
    group_by(month, year) %>%
    summarise(gdd = sum(value)) %>%
    summarise(`Growing Degree Days` = mean(gdd),
              `SD Growing Degree Days` = paste0(mean(gdd) - sd(gdd), ' - ',
                                                mean(gdd) + sd(gdd))) 
    
  
  # print('created individual dfs')
  
  # combine these all into a list
  df_list <- list(maxtemp, meantemp, mintemp, prcp, snow, frosth, frostd, gdd)
  
  # reduce is sorta like lapply or do.call(), bind all of the individual tables 
  # into one table
  summtable <- df_list %>% purrr::reduce(full_join, by = 'month') %>%
  # summtable <- cbind(maxtemp, meantemp, mintemp, prcp, snow, frosth, frostd, gdd) %>%
    as.data.frame() %>%
    # take out hte standard deviation columns
    dplyr::select(!starts_with('SD')) %>%
    # add this in so that the months are in order; the above operations puts the
    # months into alphabetical order
    dplyr::full_join(data.frame(month = month.name, key = 1:12)) %>%
    # sort them by months
    dplyr::arrange(key) %>%
    tibble::column_to_rownames('month') %>%
    dplyr::select(-c('key')) %>%
    # round to 1 decimal
    round(1) %>%
    # convert to matrix (needs to be matrix so that the table of values for the 
    # Boise airport weather station can be subtracted)
    as.matrix()

  # Make table of standard deviations
  sdtable <- cbind(maxtemp, meantemp, mintemp, prcp, snow, frosth, frostd, gdd) %>%
    as.data.frame() %>%
    dplyr::select(starts_with('SD')) %>% 
    as.matrix()
  
  # print('created summtable')
  
  # Table comparing selected location to airport
  comptable <- summtable - as.matrix(dat$airport.table) %>% 
    as.matrix() %>%
    # round to 1 decimal
    round(1)
  # print('created comptable')
  
  # combine these into one table with selection location value, then difference 
  # btwn that and airport in parentheses
  # need to do a lot of these converting between formats because it gets all jumbled
  # otherwise
  combined <- paste0(summtable, ' (', round(comptable, 2), ')') %>%
    as.vector() %>%
    matrix(ncol = 12) %>%
    data.frame() %>% 
    # transpose
    base::t()
  # print('created combined table')
  rownames(combined) <- month.name
  
  colnames(combined) <- c('TMAX', 'Avg High', 'TMEAN', 'TMIN', 'Avg Low', 'DPRCP', 'PRCP Days',
                          'DSNOW', 'Snow Days', 'FROSTH/Day', 'FROSTH', 'FROSTD', 'GDD')
  
  rownames(sdtable) <- month.name
  
  colnames(sdtable) <- c('TMAX', 'Avg High', 'TMEAN', 'TMIN', 'Avg Low', 'DPRCP', 'PRCP Days',
                          'DSNOW', 'Snow Days', 'FROSTH/Day', 'FROSTH', 'FROSTD', 'GDD')
  
  # since the numbers are all in text format at this point, this rounds them to 2 decimal points
  sdtable <- gsub('(\\d+\\.\\d{2})\\d+', '\\1', sdtable)
  
  return(list(combined = combined, sdtable = sdtable))
}








