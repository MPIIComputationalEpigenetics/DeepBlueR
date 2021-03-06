% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepblue.R
\name{deepblue_list_requests}
\alias{deepblue_list_requests}
\title{list_requests}
\usage{
deepblue_list_requests(request_state = NULL,
  user_key = deepblue_options("user_key"))
}
\arguments{
\item{request_state}{- A string (Name of the state to get requests for. The valid states are: new, running, done, and failed.)}

\item{user_key}{- A string (users token key)}
}
\value{
data_state - A array (Request-IDs and their state)
}
\description{
List the Requests made by the user. It is possible to obtain only the requests of a given state.
}
\examples{
deepblue_list_requests(request_state = 'running')

}
\seealso{
Other Requests status information and results: \code{\link{deepblue_get_request_data}}
}
