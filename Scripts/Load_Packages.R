##____________________________________________________________________________##
#' @title Instantly install and load R packages
#' @description Simultaneously install then load a vector of R packages with one function
#'
#' @param rPack A vector of R packages to required for a session. Can be a mix of currently installed and new packages (CRAN only)
#' @param update A boolean statement, whether or not to update currently installed packages (reinstalls all packages in list). This takes longer.
#' 
#' @note
#' This function only works for R packages available on CRAN. Installing packages from GitHub is not currently supported.
#'
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @examples
#' ## Not Run:
#' ### this runs faster (only 'spatialEco' needs to be installed)
#'  instaload(rPack = c("terra","spatialEco","data.table","sf"), update = F)
#' 
#' ### this runs slower because all four packages will be reinstalled before loading
#'  instaload(rPack = c("terra","spatialEco","data.table","sf"), update = T)
#'
#' ## End Not Run
#'
#' @export instaload

instaload<-function(rPack, update=F){
  if(update==T && is.null(names(sessionInfo()$otherPkgs))==F){
    lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=T,unload=T,force=T)
  }
  lapply(rPack, function(pkg){
    if(update==T){install.packages(pkg)
    }else if(!nzchar(system.file(package=pkg))){
      install.packages(pkg)
    }
    require(pkg, character.only = T)
  })
}

dependencies <- c('shiny', 'xts', 'data.table', 'leaflet', 'ncdf4', 'grid', 'rgdal',
                  'gridExtra', 'leafem', 'raster', 'rjson', 'sp', 'sf', 'shinyWidgets', 
                  'dplyr', 'htmlwidgets', 'dygraphs', 'DT', 'shinyjs', 'epitools',
                  'ggplot2', 'ggrepel', 'lubridate', 'shinybusy', 'shinyalert',
                  'magrittr', 'tidyr', 'purrr', 'gtable', 'staplr')

instaload(dependencies)



