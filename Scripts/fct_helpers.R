##' fct_helpers.R
##' 
##' A set of functions used within the DOA Web Climate Viewer
##' These ones are small functions that are truly helpers, whereas any that do 
##' a lot of heavy lifting (the render functions or the download tab helpers which
##' all do a lot of work) are defined separately
##' 

# checks whether an input object is valid, ie not NULL, NA, empty vector, or string
# with a value of 'NA' or 'NULL' (this happen somewhat interchangeably, and it's 
# hard to tell when each one happens)
valid <- function(x) {
  if ( is.null(x) ) return(FALSE)
  else if ( is.vector(x) & length(x) == 0 ) return(FALSE)
  else if ( is.na(x) ) return(FALSE)
  else if ( x== '' | x == 'NA' | x == 'NULL' ) return(FALSE)
  # if (isTruthy(x) & x != 'NA' & x != 'NULL' & !is.na(x) &  return(TRUE)
  else return(TRUE)
}

# opposite of %in% 
`%!in%` <- Negate(`%in%`)


#' euclidean distance between 2 points
distance <- function(lat_dist, lon_dist) {
  sqrt(lat_dist*2 + lon_dist*2)
}

#' Find sequences within numeric vector
#' ie in 1988, 1989, 1990, 1991, 1995, 1997, 1998,1999,2005; returns "1988-1991, 1995, 1997-1999, 2005"
#' used for title of download pdf to make it easier for hte user to tell what years 
#' they're looking at
find.sequence <- function(dat) {
  seq <- split(dat, cumsum(c(1, diff(dat) != 1)))
  years <- c()
  
  for(i in seq) {
    years <- c(years, 
               ifelse(length(i) > 1,
                      paste(min(i), '-', max(i)),
                      i
               )
    )
  }
  
  return(paste(years, collapse = ', '))
}

#' return lat/long in data frame that's closest to the clicked lat/long
get_latlon <- function(dat, lat, lon) {

  lats <- dat$XLAT
  lons <- dat$XLONG
  
  a <- abs(lats - lat)
  b <- abs(lons - lon)
  c <- distance(a, b)
  index <- which(c == min(c), arr.ind = T)
  
  return(c(index[1], index[2]))
}
# 
# generate_table <- function(df, lat, lon) {
#   coords <- get_latlon(df, lat, lon)
#   dfs <- dplyr::filter(df, XLAT == coords[1], XLONG == coords[2])
#   columns <- colnames(dfs)[1:18]
#   dfs <- dfs[,1:18]
#   trends <- c()
#   pvals <- c()
#   
#   for (i in columns[2:15]) {
#     
#     mod <- stats::lm(dfs[[i]]~dfs$year)
#     trend <- mod[[1]][2]
#     pval <- summary(mod)$coefficients[2,4]
#     index <- match(i, columns[2:15])
#     
#     trends[index] <- trend
#     pvals[index] <- pval
#   }
#   
#   summary_df <- as.data.frame(t(do.call(cbind, lapply(dfs[,2:15], summary))))
#   summary_df <- cbind(summary_df, trends, pvals)
#   summary_df <- rename(summary_df, 'Trends [units/year]' = 'trends', 'P-Value' = 'pvals')
#   
#   l <- list(round(summary_df[columns[2:15],3]))
#   
#   return(l)
# }
# 
# create_plots <- function(df_sub) {
#   barplot(height = df_sub$GDD, names.arg = df_sub$year, ylim = c(1500,2200),
#           col = 'steelblue', border = 'black', 
#           main = 'Growing degree days [C]', ylab = 'GDD', xlab = 'Year')
#   axis(side = 1, at = c(.75,5.5,10.25, 15,19.75,24.5,29.5,29.5,34.25),
#        labels = F)
# }

#### Javascript Functions
## These can't have any extra white space in them or they won't work
## these are used in renderTimeSeriesTab
getMonth <- 'function(d){
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
return monthNames[d.getMonth()];
}'

# Javascript to get the x values passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
date = new Date(d);
return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'












































