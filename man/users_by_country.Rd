% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{users_by_country}
\alias{users_by_country}
\title{Users Country}
\usage{
users_by_country(data)
}
\arguments{
\item{data}{Mixpanel data, an object of class \code{mixpanel_list_data}
as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
set to \code{FALSE}.}
}
\value{
A \link[tibble]{tibble} of the user, the country,
and the number of country each user has visited SIG from,
 useful if there is a tie (\code{n}).
}
\description{
Compute users most frequent country.
}
