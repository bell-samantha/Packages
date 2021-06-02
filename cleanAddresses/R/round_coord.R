#' A cleanAddresses Function
#' 
#' This function takes in 2 vectors of coordinate values - one for lattitude and one for longitude. The result is a 4-column tibble the same length and order as the input values. The new coordinates can be accessed in column 3 and 4. 
#' @param lattitude The vector of Lattitude values (usually a column from a dataset)
#' @param longitude The vector of Longitude values (usually a column from a dataset)
#' @param distance The number of degree decimal places to round the coordinate. 1 degree, or zero decimal places, rounds to an accuracy of approximately 111km. Each additional decimal place is 10 times more accurate in distance.
#' @keywords coordinates, address
#' @export
#' @examples # To round coordinates to an accuracy of approximately 1.11km. Returns 4 columns (two original, and two rounded)
#' round_coord(lattitude = myData$lat, longitude = myData$long, distance = 2)
#' # To get the rounded lattitude only
#' round_coord(lattitude = myData$lat, longitude = myData$long, distance = 2)[,4]
#'  


round_coord <- function(lattitude, longitude, distance){
  coordinates <- dplyr::as_tibble(cbind(as.numeric(lattitude), as.numeric(longitude)))
  coordinates$rounded_lattitude <- round(lattitude, distance)
  coordinates$rounded_longitude <- round(longitude, distance)
  return(coordinates)
} 


# Samantha Lynn Bell - June 1, 2021