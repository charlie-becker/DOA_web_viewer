library(xts)
library(data.table)
library(ncdf4)
library(grid)
library(gridExtra)
library(shiny)
library(leaflet)
library(leafem)
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

# specify working directory
working_dir <- '/Users/kendrakaiser/github/DOA_web_viewer/'
# set working directory
setwd(working_dir)

##### R FUNCTIONS FOR DATA DOWNLOAD ############################################

distance <- function(lat_dist, lon_dist) { sqrt(lat_dist*2 + lon_dist*2)}

get_latlon <- function(dff, lat, lon) {
    
    lats <- dff$XLAT
    lons <- dff$XLONG
    
    a <- abs(lats - lat)
    b <- abs(lons - lon)
    c = distance(a,b)
    index = which.min(c)
    
    return(c(lats[index],lons[index]))
}

generate_table <- function(df, lat, lon) {
    
    coords <- get_latlon(df,lat, lon)
    dfs <- filter(df, XLAT == coords[1], XLONG == coords[2])
    columns <- colnames(dfs[1:18])
    dfs <- dfs[columns]
    trends <- c()
    pvals <- c()
    
    for (i in columns[2:15]) {
        
        mod <- lm(dfs[[i]]~dfs$year)
        trend <- mod[[1]][2] 
        pval <- summary(mod)$coefficients[2,4]
        index <- match(i, columns[2:15])
        
        trends[index] <- trend
        pvals[index] <- pval
    }
    
    summary_df <- as.data.frame(t(do.call(cbind, lapply(dfs[2:15], summary))))
    summary_df <- cbind(summary_df, trends, pvals)
    summary_df <- rename(summary_df, "Trends [units/year]" = "trends", 'P-Value' = 'pvals')
    
    l = list(round(dfs[columns[1:18]],3), round(summary_df,3))
    
    return(l)
}

create_plots <- function(df_sub) {
    
    barplot(height=df_sub$GDD, names.arg = df_sub$year, ylim = c(1500,2200), xpd = FALSE, col = 'steelblue', border = 'black',
            main = 'Growing degree days [C]', ylab = 'GDD', xlab='Year')
    axis(side = 1, at = c(.75,5.5, 10.25, 15, 19.75, 24.5, 29.5, 34.25), labels = FALSE)
    
}

##### JAVASCRIPT FUNCTIONS #########

# get the axis label passed as a date, this function outputs only the month of the date
getMonth <- 'function(d){
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
return monthNames[d.getMonth()];
}'

# Javascript to get the x values passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
date = new Date(d);
return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'
####################################

# load data for dygraphs (AVA)
d <- data.table::fread("data/AVA_30YR_NoLeap917.csv")

# load data for site specific "Download Data" tab
site_specific_data <- data.table::fread('data/AVA_30YR_df_all_vars.csv')

# initial file for default map upon loading
init_raster <- "data/AVA_WY1988_yearly_stats_d02.nc"

# get AVA Shape File
jsonFile <- "data/SR_AVA_simplified_pointRemove50m.json"
# convert JSON file to R list object (this is just an additional shape layer)
json <- fromJSON(file = jsonFile)


# create a list of non-leap year dates to be used for dygraphs (year will be stripped)
dates <- seq(as.Date("1987-01-01"),as.Date("1987-12-31"), "day")

# Scale type for dygrpahs
scaleType <- c("Individual Years", "Monthly (across all years)", "All years")

# create month names
monNames = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# get list of netCDF files (raw data)
ncFileNames <- list.files(pattern = ".nc")

# set variable names to pull from file and variable names to list on UI
ncVarNames <- c("TMAX", "TMIN", "TMEAN", "GDD", "DPRCP", "DSNOW", "FROSTD", "FROSTH")
varNamesLong <-  c("Maximum Daily Temperature","Minimum Daily Temperature","Mean Daily Temperature", "Growing Degree Days",
                   "Total Precipitation","Snowfall", "Frost Days", "Frost Hours")

# define CRS from netCDF file (from WRF) and Extent (from GDAL)
myCRS <- CRS("+proj=lcc +lat_1=44.299999 +lat_2=44.99999 +lat_0=44.300003 +lon_0=-114.7 +x_0=0 +y_0=0 +units=m +datum=WGS84 +no_defs")
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

map2 = leaflet() %>% addTiles() %>%
    setView(lng = -116.5, lat = 43.8 ,zoom = 8) %>%
    addGeoJSON(json, weight = .5, color = "black", fill = T, opacity = .5)

################################################################################
# User interface 

ui <- navbarPage("", 
        # Landing page.  "tags$" is replicating HTML directly         
        tabPanel("Home",
                 
            # Load CSS file (in /www)     
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                tags$meta(name="viewport", content="initial-scale=1")
            ),
            setBackgroundColor("lightgrey"), 
            tags$img(class="bg", src="ales-me-z0bACVUDTJM-unsplash.jpg"),
            tags$div(class = "text-block", # load CSS .text-block (style and positioning)
                     tags$h1("Snake River Valley AVA"), # Title
                     tags$h1("Climate Explorer")),
            tags$div(class="landing-block",
                     p(class="lp_text","The Snake River Valley American Viticultural Area (AVA) Climate Explorer is an interactive tool to visualize 30 years of
                                high resolution climate data in the rapidly growing Idaho grape growing region and beyond."),
                     p(class='lp_text', "In the age of big data and high computational power, new and novel climate datasets are available that can be leveraged to
                                inform wine enthusiasts and farmers alike about historical climate conditions."),
                     p(class="lp_text","Use the toolbar at the top of the page to select the data or information category of interest and you’ll be directed to
                                a dynamic graph for visualization or a table for downloading. Under the background tab you’ll find additional information 
                                about the project and dataset."),
                     br(), div(class = "intro-divider4"), br(),
                     p(class="fp_note","PLEASE NOTE: This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, 
                                refresh the page. This will reset all previously-selected input options."))),
    # Explorer tab - the spatial viewer    
    tabPanel("Explorer",             
    useShinyjs(),
    #titlePanel(""),
    sidebarLayout(
        
        # create side panel for parameter adjustment
        sidebarPanel(width = 5, style = "height:90vh; font-size:1.2vh",
            strong('Spatial Data Explorer', style = "font-size:2.5vh"), br(), br(), 
            p('The spatial explorer shows the Snake River AVA - where the majority of Idaho’s vineyards are located. The full [dataset] expands far beyond this area which can be visualized
              by selecting the region of your choice. The plot can be zoomed in or out in the same fashion as Google Maps.', style = "font-size:1.5vh"),
            p('To create a unique plot, select a climate variable (for example, temperature or precipitation) and years on the left and then click the ‘create map’ button. The historical option
              will require a date range to be averaged or summed over (depending on the variable).', style = "font-size:1.5vh"),
            p('Anomalies are the difference between your year of choice  and the average of that variable for all 30 years.  For example, if you select ‘Monthly Anomaly’, VARIABLE=’Minimum
              Daily Temperature’,‘YEAR=2011’ and ‘MONTH=April’ you will get a plot showing the difference between the average minimum daily temperatures for the month of April in 2011 and the
              average minimum temperatures in April for all years spanning 1988-2017. In this example, temperatures are about 2.0 - 3.5 degrees Celsius cooler in April 2011 than the 30 year
              average. If you were to look at your crop yields over the 30 year timespan, it would be interesting to see how the deviation from average in 2011 affected your yields that year!',
              style = "font-size:1.5vh"),
            br(), div(class = "intro-divider3"), br(),
        
            # Input types with defaults selected 
            radioButtons("domainInput", "Region", choices = c("Snake River AVA (1km resolution)", "Greater Idaho (1km resolution)"), #"Greater Pacific Northwest (3km resolution)"), 
                                                              selected = "Snake River AVA (1km resolution)"),
            radioButtons("plotInput", "Plot Type", choices = c("Historical","Yearly Anomaly", "Monthly Anomaly"), selected = "Historical"),
            selectInput("varInput", "Variable", choices = varNamesLong, selected = varNamesLong[1]),
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
        mainPanel(width = 7,
            # Render leaflet map
            leafletOutput("myMap", height = "90vh")
        ))),
    
        # Start Time Seeries tab
        tabPanel("TimeSeries",
                 #titlePanel(""),
                 sidebarLayout(
                     # Inputs 
                     
                     sidebarPanel(width = 4, style = "height:80vh; font-size:1.2vh",
                         strong('Timeseries Data Explorer', style = "font-size:2.5vh"), br(), br(), 
                         p('To use the time series graph, select the region, variable and up to five different years to be compared simultaneously - the graph will update automatically as inputs 
                           are changed. Mousing over the graph will display the values at that location for each value in the upper right corner along with the date.  You can zoom into these graphs
                           by clicking and dragging across an area  on the map or by moving  the slider below the graph.', style = "font-size:1.5vh"),
                         p("Each data line represents the average value over the entire region that has been selected. These time series graphs are particularly useful for assessing climate
                           differences between different years. For example, if you select Growing Degree Days and the years 2006 and 2007 you can compare the mean cumulative GDD of each year
                           through time. ", style = "font-size:1.5vh"),
                         br(), div(class = "intro-divider3"), br(),
            
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
                         dygraphOutput("myGraph", width = "100%", height = "70vh")
                     )
                 )),
        tabPanel("Statistics",
                 #titlePanel(""),
                 sidebarPanel(width = 4, style = "height:80vh; font-size:1.2vh",
                     strong('Statistics', style = "font-size:2.5vh"), br(), br(),
                     p('The data table will update as  different regions or time periods are selected.  Additionally, you can click on any header within
                       the table itself to organize the table 
                       by ascending or descending values of that variable. To export the data …', style = "font-size:1.5vh"),
                     p('Assessing these statistics allows you to see the total variability of each metric over 30 years and easily compare yearly
                       summaries', style = "font-size:1.5vh"),
                     p('In future versions of the climate data explorer we will add the capability for you to get these same statistics for your own
                       area of interest!', style = "font-size:1.5vh"),
                     br(), div(class = "intro-divider3"), br(),
                     
                     selectInput("domainInput2", "Domain",
                        choices = c("Snake River AVA", "Sunnyslope", "Domain 02", "Domain 01"),  
                        selected = "Snake River AVA"),
                     selectInput("myGroup", "Group by",
                        choices = c("YEAR","MONTH","DAY"),
                        selected = "YEAR")),
                mainPanel(
                     DT::dataTableOutput("myTable", height = "90vh")
                 )),
        tabPanel("Download Data",
                 
                 sidebarPanel(width = 5,
                    p('Testing......'),
                    uiOutput("clat"),
                    uiOutput("clon"),
                    # Render leaflet map
                    leafletOutput("myMap2", height = "50vh"),
                    br(),
                    # call custom function for creating "View/Download" buttons
                    withBusyIndicatorUI(actionButton("button2", "View Annual Report", class = "btn-primary")),
                    br(),
                    withBusyIndicatorUI(actionButton("button3", "View Summary Report", class = "btn-primary")),
                    br(),
                    #withBusyIndicatorUI(actionButton("button4", "Download Full Report", class = "btn-primary"))),
                    #br(),
                    downloadButton('downloadData', 'Download Full Report')),
                 mainPanel(width = 7,
                            DT::dataTableOutput("siteTable", height = "90vh"))

                 ),
        tabPanel("Background",
                 # Logos at bottom of page with links
                 tags$a(href = "https://www.boisestate.edu",target = "_blank", img(src="BSU2.png", class = "logo1")), 
                 tags$a(href = "https://agri.idaho.gov/main/", target = "_blank", img(src="ISDOA.png", class = "logo2")),
                 tags$a(href = "https://leaf.boisestate.edu/people/", target = "_blank", img(src="LEAF2.png", class = "logo3")),
                 sidebarPanel(width = 5, style = "height:80vh; font-size:1.2vh",
                    strong('Background', style = "font-size:36px"), br(), br(),
                    p("This project was done with through the support of a specialty crop block grant provided by the ", 
                      tags$a(href="https://agri.idaho.gov/main/", target = "_blank", "Idaho State Department of Agriculture."), 
                      "Viticulture and other specialty crops, such as hops, are rapidly emerging as significant crops in Idaho. The
                      emergence of big data and computational power allows us to determine what the historical climate was in this region at a higher
                      resolution (finer time and spatial scale) than most other products. We hope that making this data publicly available will become
                      a valuable resource for individuals that currently, or are planning to grow crops in Idaho.", style = "font-size:1.5vh"),
                    p("The climate data used for this project is a modeled dataset using the ",
                      tags$a(href="https://en.wikipedia.org/wiki/Weather_Research_and_Forecasting_Model", target="_blank", 
                      "Weather and Research Forecasting (WRF) model. "),  "It was generated at the ", 
                      tags$a(href="https://www.boisestate.edu/leaf/research/", target = "_blank", "LEAF lab "), 
                      "at Boise State University.  It contains two regions of interest: a larger region covering much of
                      the Pacific Northwest at 3km spatial resolution, and an inner region  at 1km spatial resolution.  Both of these have hourly
                      data from water year (Oct 1 - September 30) 1988 through 2017.", style = "font-size:1.5vh"),
                    p("The total size of full dataset in incredibly large - over 250 terabytes! The data we show through this app is but a very small
                      piece of the pie.  Of particular relevance is that all data shown here was aggregated from hourly to daily temporal resolution.
                      Additionally, data in the time series or statistic sections were all spatially averaged over the region of interest. The animation 
                      on the right showcases some of this data during the deadly frost (for many vineyards) in January 2017. You can clearly see the cold
                      air drain into Treasure Valley at night and stay there for a couple of days as one of our famous inversions.  You can see some grey
                      and white regions pop up at about the 0:20 mark where temperatures were -15 to -28 degrees F below zero!  ",
                      style = "font-size:1.5vh"),
                    p("If you have any questions regarding our ongoing Idaho projects or general questions please feel free to contact us! We look forward
                      to hearing from you.", style = "font-size:1.5vh"),
                    strong("Charlie Becker - charles11becker@gmail.com", br(), "LEAF lab - lejoflores@boisestate.edu", style = "font-size:1.5vh")
                              ),
                 mainPanel(width = 7, style = "height:95vh",
                    tags$video(src="Freeze_6fps.mp4", controls="controls", type = "video/mp4", width= "95%")
                    #div(img(src="Jvictor2.jpg", width="100%"), style="text-align:center;"),
                    #br(), p("A view from Sunnyslope - January 2019", style = "font-size:18px; text-align:center"), br()
                    
                 ))
)

################################################################################

server = function(input, output, session) {
    
###############################################################################
#  The follwing section refers to the "Explorer" Tab Panel 

    # render leaflet map
    output$myMap <- renderLeaflet(map)
    
    # render leaflet map "Download Data" tab
    output$myMap2 <- renderLeaflet({map2 %>% 
            addMouseCoordinates() })
    
    # Load initial leaflet map in background in Spatial Explorer
    outputOptions(output, "myMap", suspendWhenHidden = FALSE)
    
    # Load initial leaflet map in background in Download Data
    outputOptions(output, "myMap2", suspendWhenHidden = FALSE)
    
    # get variable name to pull from data from user selection
    v <- reactive ({ ncVarNames[match(input$varInput, varNamesLong)] })
    
    # conditionals to determine which file to load (based on year and domain)
    rast <- eventReactive(input$button, { 

        if (input$plotInput == "Historical") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick(paste0("data/AVA_WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Greater Idaho (1km resolution)") { 
                brick(paste0("data/WY",input$yearInput, "_yearly_stats_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Greater Pacific Northwest (3km resolution)") { 
                brick(paste0("data/WY",input$yearInput, "_yearly_stats_d01.nc"), varname = v(),
                      crs = myCRS) }}
        else if (input$plotInput == "Yearly Anomaly") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick("data/AVA_Yearly_Anomalies_d02.nc", varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Greater Idaho (1km resolution)") { 
                brick("data/Yearly_Anomalies_d02.nc", varname = v(),
                      crs = myCRS) }}
        else if (input$plotInput == "Monthly Anomaly") {
            if (input$domainInput == "Snake River AVA (1km resolution)") { 
                brick(paste0("data/", input$monInput, "_AVA_Anomalies_d02.nc"), varname = v(),
                      crs = myCRS) }
            else if (input$domainInput == "Greater Idaho (1km resolution)") { 
                brick(paste0("data/", input$monInput, "_Anomalies_d02.nc"), varname = v(),
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

df_stats <- reactive ({ 
    
    if (input$domainInput2 == "Snake River AVA") {
        data.table::fread("data/AVA_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Sunnyslope") {
        data.table::fread("data/SS_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Domain 01") {
        data.table::fread("data/d01_30YR_NoLeap917.csv") }
    else if (input$domainInput2 == "Domain 02") {
        data.table::fread("data/d02_30YR_NoLeap917.csv") }
        })

DTdf <-  reactive ({ df_stats() %>% group_by_(input$myGroup)  %>% na.omit() %>% summarise(Mean_Temp = mean(TMEAN),
            Mean_Max_Temp = mean(TMAX), Mean_Min_Temp = mean(TMIN), GDD = sum(GDD),
            Total_Precip = sum(DPRCP), Total_Snow = sum(DSNOW), Frost_Days = sum(FROSTD),
            Frost_Hours = sum(FROSTH)) %>% round(2) })

output$myTable <- DT::renderDataTable({ DT::datatable(DTdf(),fillContainer = T,
                    options = list(pageLength = 50)) })

###############################################################################
# The folling section refers to the "Download Data" tab

## Observe mouse clicks and get lat/lon
observeEvent(input$myMap2_click, {
    click <- input$myMap2_click
    clat <- click$lat
    clon <- click$lng 
    
    leafletProxy('myMap2') %>%
        clearMarkers() %>%
        clearShapes() %>%
        addCircles(lng=clon, lat=clat, group='circles',
                   weight=1, radius=500, color='black', fillColor='green',
                   fillOpacity=0.2, opacity=.5)
    
    output$clat <- renderUI(numericInput("lattitude","Lattitude",clat))
    output$clon <- renderUI(numericInput("longitude","Longitude",clon))
    
})

#site_data <- eventReactive(input$button2, {generate_table(site_specific_data, input$lattitude, input$longitude)})
observeEvent(input$button2, {
    
    output$siteTable <- DT::renderDataTable({ DT::datatable(as.data.frame(generate_table(site_specific_data, input$lattitude, input$longitude)[1]),
                                fillContainer = T,options = list(pageLength = 50)) })  
})

observeEvent(input$button3, {
    
    output$siteTable <- DT::renderDataTable({ DT::datatable(as.data.frame(generate_table(site_specific_data, input$lattitude, input$longitude)[2]),
                                                            fillContainer = T,options = list(pageLength = 50)) })  
})

output$downloadData <- downloadHandler(
    filename = function() {
        paste("data-", round(input$lattitude,2), "_", round(input$longitude,2),".pdf", sep="")},
    content = function(file) {
        pdf(file=file, height = 24, width = 16)
        grid.arrange(tableGrob(as.data.frame(generate_table(site_specific_data, input$lattitude, input$longitude)[1])),
                     tableGrob(as.data.frame(generate_table(site_specific_data, input$lattitude, input$longitude)[2])),
                     newpage = TRUE)
        dev.off()
  })

############################################################################### 

# Kill the app when closed in the browser 
session$onSessionEnded(stopApp)
}

# required when using single app.R with both ui and server combined
shinyApp(ui, server)