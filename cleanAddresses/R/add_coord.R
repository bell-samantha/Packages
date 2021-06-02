#' A cleanAddresses Function
#' 
#' This function creates a simple address tibble that can be passed through censusxy() to get x and y coordinates for each record. Best used after cleaning the street field with cleanAddresses::simplify_street. Can be joined directly into a dataset.
#' @param identifier The column name containing unique record ids
#' @param street The column name containing street name and number
#' @param city The column name containing city name
#' @param state The column name containing state name
#' @param zip The column name containing zip codes
#' @keywords coordinates, address
#' @export
#' @examples myData$newField <- simplify_street(myData$rawStreetField)
#' myCoordinates <- add_coord(street = myData$newField, city = myData$cityField, state = myData$stateField, zip = myData$zipField, identifier = myData$Id)

add_coord <- function(street, city, state, zip, identifier){
  addressTibble <- dplyr::as_tibble(cbind(street, city, state, zip, identifier))
  # Remove addresses with NA values (NAs make geocoder very slow and less accurate)
  addressTibble <- addressTibble[!is.na(addressTibble[1]),]
  addressTibble <- addressTibble[!is.na(addressTibble[2]),]
  addressTibble <- addressTibble[!is.na(addressTibble[4]),]
  censusxy::cxy_geocode(addressTibble, street= "street", city = "city", state = "state", zip = "zip", output = "simple")
}

# Samantha Lynn Bell - June 1, 2021