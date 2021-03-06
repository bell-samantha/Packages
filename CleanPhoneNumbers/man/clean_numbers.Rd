% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_numbers.R
\name{clean_numbers}
\alias{clean_numbers}
\title{Clean phone numbers}
\usage{
clean_numbers(phones, country)
}
\arguments{
\item{country}{\link{The country code that can appear at the beginning of a phone number}}

\item{phone}{\link{The column (dat$col) or vector of phone numbers}}
}
\description{
This function allows you to send a column/vector of dirty phone numbers through and supplies you with a list of standardized, clean phone numbers of the same vector length. Non-passing numbers will be set to NA.
}
\examples{
myDat$newPhoneNumbers <- clean_numbers(myDat$oldPhoneNumbers)
}
\keyword{[phone}
\keyword{digit]}
\keyword{number,}
\keyword{phone,}
