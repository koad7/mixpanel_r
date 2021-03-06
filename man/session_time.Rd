% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sessions.R
\name{session_time}
\alias{session_time}
\alias{sessionise}
\title{Session Time}
\usage{
session_time(data, timeout = 15 * 60, events = ".")

sessionise(data, timeout = 15 * 60, events = ".")
}
\arguments{
\item{data}{Mixpanel data, an object of class \code{mixpanel_list_data}
as returned by \code{\link{parse_mixpanel_data}} where \code{tidy} was 
set to \code{FALSE}.}

\item{timeout}{Number of seconds before the user is considered logged out.}

\item{events}{Regular expression of events to keep where \code{.}
keeps all events, passed to \link[base]{grepl}.}
}
\value{
A \link[tibble]{tibble} of sessions containing the start and end time
of the session as well as the unique user id, the session duration in seconds,
and the number of clicks done during each session.
}
\description{
Compute session time.
}
\examples{
\dontrun{
data(sample_data)
session_time(sample_data)
}

}
