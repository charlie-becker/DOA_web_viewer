source('Scripts/Load_Packages.R')
source('Scripts/buttonIndicator.R')
source('Scripts/Load_Data.R')
source('Scripts/fct_helpers.R')
source('Scripts/renderDownloadTab.R')
source('Scripts/renderExplorerTab.R')
source('Scripts/renderStatisticsTab.R')
source('Scripts/renderTimeSeriesTab.R')
source('Scripts/DownloadTab_helpers.R')

## 1.0 UI ----
ui <- navbarPage('',
 ## 1.1 MAIN PAGE ----
 tabPanel('Home',
          tags$head(
            tags$link(rel = 'stylesheet', type = 'text/css', 
                      href = 'app.css'),
            tags$meta(name = 'viewport', content = 'initial-scale=1')
          ), # tags$head
          shinyWidgets::setBackgroundColor('lightgrey'),
          tags$img(class = 'bg', src = 'vineyard_background.jpg'),
          #' load CSS .text-block (style and positioning)
          #' creates greyed out block for Main page title
          tags$div(class = 'text-block',
                   tags$h1('Snake River Valley AVA'), # Title
                   tags$h1('Climate Explorer')), 
          tags$div(class = 'landing-block',
                   p(class="lp_text","The Snake River Valley American Viticultural Area (AVA) Climate Explorer is an interactive tool to visualize 30 years of
                              high resolution climate data in the rapidly growing Idaho grape growing region and beyond."),
                   p(class='lp_text', "In the age of big data and high computational power, new and novel climate datasets are available that can be leveraged to
                              inform wine enthusiasts and farmers alike about historical climate conditions."),
                   p(class="lp_text","Use the toolbar at the top of the page to select the data or information category of interest and you’ll be directed to
                              a dynamic graph for visualization or a table for downloading. Under the background tab you’ll find additional information 
                              about the project and dataset."),
                   br(), div(class = "intro-divider4"), br(),
                   p(class="fp_note","PLEASE NOTE: This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, 
                              refresh the page. This will reset all previously-selected input options."))
          ), # tabPanel
 ## 1.2 EXPLORER TAB ----
 tabPanel('Explorer',
   useShinyjs(),
    sidebarLayout(
      sidebarPanel(width = 5,
                   style = 'height:90vh; font-size:1.2vh',
                   strong('Spatial Data Explorer', 
                          style = 'font-size:2.5vh'),
                   br(), br(),
                   p('The spatial explorer shows the Snake River AVA - where the majority of Idaho’s vineyards are located. The full [dataset] expands far beyond this area which can be visualized
                     by selecting the region of your choice. The plot can be zoomed in or out in the same fashion as Google Maps.', style = "font-size:1.5vh"),
                   p('To create a unique plot, select a climate variable (for example, temperature or precipitation) and years on the left and then click the ‘create map’ button. The historical option
                     will require a date range to be averaged or summed over (depending on the variable).', style = "font-size:1.5vh"),
                   p('Anomalies are the difference between your year of choice  and the average of that variable for all 30 years.  For example, if you select ‘Monthly Anomaly’, VARIABLE=’Minimum
                     Daily Temperature’,‘YEAR=2011’ and ‘MONTH=April’ you will get a plot showing the difference between the average minimum daily temperatures for the month of April in 2011 and the
                     average minimum temperatures in April for all years spanning 1988-2017. In this example, temperatures are about 2.0 - 3.5 degrees Celsius cooler in April 2011 than the 30 year
                     average. If you were to look at your crop yields over the 30 year timespan, it would be interesting to see how the deviation from average in 2011 affected your yields that year!',
                     style = "font-size:1.5vh"),
                   br(), div(class = 'intro-divider3'), br(),
                  
                   #' Input types with defaults selected
                   radioButtons('domainInput', 'Region', 
                                choices = c('Snake River AVA (1km resolution)',
                                            'Greater Idaho (1km resolution)'
                                           ),
                                selected = c('Snake River AVA (1km resolution)')
                               ), # radioButtons
                   radioButtons('plotInput', 'Plot Type',
                                choices = c('Historical', 'Yearly Anomaly',
                                            'Monthly Anomaly'),
                                selected = 'Historical'
                               ), # radioButtons
                   #' Render choices on server side
                   selectInput('varInput', 'Variable', choices = NULL), #varNamesLong),
                   selectInput('yearInput', 'Year', choices = 1988:2017),
                   
                   #' conditional parameter (add date selector) if looking at 
                   #' historical data
                   conditionalPanel(
                     condition = 'input.plotInput == "Historical"',
                     sliderInput('dateInput', 'Days of Water Year',
                                 min = 1, max = 365, value = c(1,10)),
                     disabled(dateRangeInput('myDate', 'Select dates to view',
                                    start = lubridate::ymd('1988-10-01'),
                                    end = lubridate::ymd('1988-10-10'),
                                    format = 'yyyy-mm-dd', startview = 'year'))
                   ), # conditionalPanel
                   conditionalPanel(
                     condition = 'input.plotInput == "Monthly Anomaly"',
                     #' render choices on server side
                     selectInput('monInput', 'Month', choices = NULL)#monNames)
                   ), # conditionalPanel
                   
                   #' Call custom function for creating "Create Map" button
                   # actionButton('button', 'Create Map', class = 'btn-primary'),
                   withBusyIndicatorUI(actionButton('button', 'Create Map',
                                                    class = 'btn-primary')),
                   br(), br()
                 ), # sidebarPanel
      mainPanel(width = 7,
                leafletOutput('myMap', height = '90vh'))
    ) # sidebarLayout
 ), # tabPanel
 
 ## 1.3 TIME SERIES TAB ----
 tabPanel('Time Series',
    sidebarLayout(
      sidebarPanel(width = 4,
                   style = 'height:80vh; font-size:1.2vh',
                   strong('Timeseries Data Explorer', 
                          style = 'font-size:2.5vh'),
                   br(), br(),
                   p('To use the time series graph, select the region, variable and up to five different years to be compared simultaneously - the graph will update automatically as inputs 
                     are changed. Mousing over the graph will display the values at that location for each value in the upper right corner along with the date.  You can zoom into these graphs
                     by clicking and dragging across an area  on the map or by moving  the slider below the graph.', style = "font-size:1.5vh"),
                   p("Each data line represents the average value over the entire region that has been selected. These time series graphs are particularly useful for assessing climate
                     differences between different years. For example, if you select Growing Degree Days and the years 2006 and 2007 you can compare the mean cumulative GDD of each year
                     through time. ", style = "font-size:1.5vh"),
                   br(), div(class = "intro-divider3"), br(),
      
                   selectInput('domainInput1', 'Domain',
                               choices = c('Snake River AVA', 'SunnySlope', 
                                           'Domain 02', 'Domain 01'),
                               selected = 'Snake River AVA'),
                   # Render choices server side
                   selectInput('scaleInput', 'What would you like to plot?',
                               choices = NULL),
                               # choices = scaleType, selected = scaleType[1]),
                   pickerInput('yearInput1', 'Years', choices = 1987:2017,
                               selected = '1995', multiple = T,
                               options = list('max-options' = 5)),
                   # Render choices server side
                   selectInput('varInput1', 'Variable',
                               choices = NULL)
                               # choices = varNamesLong, 
                               # selected = varNamesLong[3]), 
                   ), # sidebarPanel
      mainPanel(
        # Dygraph (interactive time series graph)
        dygraphOutput('myGraph', width = '100%', height = '70vh')
      ) # mainPanel
    ) # sidebarLayout
 ), # tabPanel
 ## 1.4 STATISTICS ----
 tabPanel('Statistics',
    sidebarLayout(
      sidebarPanel(width = 4, style = 'height:80vh; font-size:1.2vh',
                   strong('Statistics', style = 'font-size:2.5vh'), 
                   br(), br(),
                   p('The data table will update as  different regions or time periods are selected.  Additionally, you can click on any header within
                      the table itself to organize the table 
                      by ascending or descending values of that variable. To export the data …', style = "font-size:1.5vh"),
                   p('Assessing these statistics allows you to see the total variability of each metric over 30 years and easily compare yearly
                      summaries', style = "font-size:1.5vh"),
                   p('In future versions of the climate data explorer we will add the capability for you to get these same statistics for your own
                      area of interest!', style = "font-size:1.5vh"),
                   br(), div(class = "intro-divider3"), br(),
                   
                   selectInput('domainInput2', 'Domain',
                               choices = c('Snake River AVA', 'Sunnyslope',
                                           'Domain 02', 'Domain 01'),
                               selected = 'Snake River AVA'),
                   selectInput('myGroup', 'Group by',
                               choices = c('YEAR', 'MONTH', 'DAY'),
                               selected = 'YEAR')
                  ), # sidebarPanel
      mainPanel(
        DT::dataTableOutput('myTable', height = '90vh')
      )
    ) # sidebarLayout
 ), # tabPanel
 ## 1.5 DOWNLOAD DATA ----
 tabPanel('Download Data',
    sidebarLayout(
      sidebarPanel(width = 5,
                   strong('Download Data', style = 'font-size:2.5vh'),
                   br(), br(),
                   # p('Testing....'),
                   # Render leaflet map
                   leafletOutput(outputId = 'myMap2', height = '50vh'),
                   
                   # uiOutput('clat'),
                   # uiOutput('clon'),
                   numericInput('latitude', 'Latitude', value = 43.5885),
                   numericInput('longitude', 'Longitude', value = -116.7932),
                   
                   selectInput(inputId = 'figuretype',
                               label = 'How do you want to view the data?',
                               choices = c(
                                 'Months on the X axis, with each year having its
                                 own line' = 'xmonth',
                                 'Year on the X axis, with data from a single month
                                 plotted showing trends over time' = 'xyear'
                               )),

                   pickerInput(inputId = 'downloadvars', 
                               label = 'Select Variables to View',
                               choices = NULL,
                               # choices = dat$ncVars, selected = dat$ncVars,
                               multiple = T,
                               options = list('actions-box' = T)),
                   
                   pickerInput(inputId = 'downloadyears',
                               label = 'Select Years to View',
                               choices = 1988:2017, selected = 1988:2017,
                               multiple = T,
                               options = list('actions-box' = T)),
                  
                   
                   uiOutput('downloadinputs'),
                   
                   br(),
                   # call custom function for creating "View/Download" buttons
                   fluidRow(
                     column(12,
                            actionButton('downloadSummaryBtn', 'Download Summary Table',
                                         class = 'btn-primary'),
                            br(), br(),
                            actionButton('downloadDataBtn', 'Download Full Report',
                                         class = 'btn-primary'),
                            br(), br(),
                            actionButton('downloadSummaryDataBtn', 'Download Full Report with Summary Table',
                                         class = 'btn-primary'))
                   )
                  ), # sidebarPanel
      mainPanel(width = 7, 
                HTML('<b><font face = "Helvetica" size = 6>What Am I Downloading?</font></b>'),
                hr(),
                tabsetPanel(
                  type = 'pill',
                  tabPanel(h3(strong('Summary Table')),
                           h4(strong('This produces 2 tables summarizing the climate record 
                              for your selected location and years.')),
                           tags$iframe(style = 'height:800px; width:100%; scrolling=yes',
                                       src = 'SummaryReportSample.pdf')
                           ),
                  tabPanel(h3(strong('Full Report')),
                           h4(strong('This produces a set of plots showing your selected variables
                              for your selected location and years. The sample output 
                              (click the button below to view) is for all variables and all 
                              years, with a rounding interval of 5 years (the default).')),
                           tags$iframe(style = 'height:800px; width:100%; scrolling=yes',
                                       src = 'FullReportSample.pdf')
                           ),
                  tabPanel(h3(strong('Full Report with Summary Table')),
                           h4(strong('This combines the other two, with a summary table 
                             included at the end of the full report.')),
                           tags$iframe(style = 'height:800px; width:100%; scrolling=yes',
                                       src = 'FullSummarySample.pdf')
                          ) 
                )
               
               )
    ) # sidebarLayout
 ), # tabPanel
 
 ## 1.6 BACKGROUND ----
 tabPanel('Background',
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
                       strong("Charlie Becker - charles11becker@gmail.com", br(), 
                              "LEAF lab - lejoflores@boisestate.edu", br(),
                              style = "font-size:1.5vh")
                      ),
          mainPanel(width = 7, style = "height:95vh",
                    tags$video(src="Freeze_6fps.mp4", controls="controls", type = "video/mp4", width= "95%")
                   )
         ) # tabPanel
) # navbarPage

## 2.0 SERVER ----
server <- function(input, output, session) {
  dat <- load_data(input, output, session)
  renderExplorerTab(input, output, session, dat)
  renderTimeSeriesTab(input, output, session, dat)
  renderStatisticsTab(input, output, session, dat)
  renderDownloadTab(input, output, session, dat)
}

shinyApp(ui, server)
