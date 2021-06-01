#' A cleanAddresses Function
#' 
#' This function takes in a character vector of character address text
#'    The entry data MUST start with street numbers but has the option to include or exclude City, State, and Zip fields
#' The result is a vector of the same length as the input data, made up of simplified street addresses only. Any City, State, or Zip will be cut off. 
#'    The result can be applied directly as a new column in a dataset by running it as:
#'       myData$newField <- simplify_street(myData$rawStreetField)
#' @param street takes in the vector of character street names (usually a column from a dataset)
#' @keywords street, address
#' @export
#' @examples
#' simplify_street()

simplify_street <- function(street){
  # Make a column with just the beginning of street addresses (for easier grouping)
  pat1 <- "^\\d+\\s+[NSEW].{0,5}\\s*\\w+"  #match for a one letter N,S,E,W before the name
  pat2 <- "^(\\d+\\s+){1,2}(\\w+){1,2}" #match for 1-2 full words directly after the number (allow for 2 numbers if one is the road name) - only use if not expecting a directional letter
  Address_Simple <- ""  # initiate empty column
  for(i in 1:length(street)){
    if(!is.na(street[i])){
      Address_Simple[i] <- ifelse(
        !is.na(str_match(street[i], regex(pat1, ignore_case = TRUE))[1]), #Check for N,S,E,W type letter match
        (str_match(street[i], regex(pat1, ignore_case = TRUE))[1] %>% #if N,S,E,W letter was found, match pattern 1 and substitute NSEW to standard format
           gsub("N\\s|N\\.\\s|North\\s", "North ", x = ., ignore.case = TRUE) %>%
           gsub("S\\s|S\\.\\s|South\\s", "South ", x = ., ignore.case = TRUE) %>%
           gsub("E\\s|E\\.\\s|East\\s", "East ", x = ., ignore.case = TRUE) %>%
           gsub("W\\s|W\\.\\s|West\\s", "West ", x = ., ignore.case = TRUE)), 
        str_match(street[i], regex(pat2, ignore_case = TRUE))) #else take a full word after the number using pattern 2
    }else{"NA"} # If no address, NA simple address
  } 
  return(Address_Simple)
}


# Samantha Lynn Bell - June 1, 2021
