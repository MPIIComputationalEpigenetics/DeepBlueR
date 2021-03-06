% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepblue.R
\name{deepblue_enrich_regions_fast}
\alias{deepblue_enrich_regions_fast}
\title{enrich_regions_fast}
\usage{
deepblue_enrich_regions_fast(query_id = NULL, genome = NULL,
  epigenetic_mark = NULL, biosource = NULL, sample = NULL,
  technique = NULL, project = NULL,
  user_key = deepblue_options("user_key"))
}
\arguments{
\item{query_id}{- A string (Query ID)}

\item{genome}{- A string or a vector of string (the target genome)}

\item{epigenetic_mark}{- A string or a vector of string (name(s) of selected epigenetic mark(s))}

\item{biosource}{- A string or a vector of string (name(s) of selected biosource(s))}

\item{sample}{- A string or a vector of string (id(s) of selected sample(s))}

\item{technique}{- A string or a vector of string (name(s) of selected technique(s))}

\item{project}{- A string or a vector of string (name(s) of selected projects)}

\item{user_key}{- A string (users token key)}
}
\value{
request_id - A string (Request ID - Use it to retrieve the result with info() and get_request_data(). The result is a list containing the datasets that overlap with the query_id regions.)
}
\description{
Enrich the regions based on regions bitmap signature comparison.
}
\examples{


}
\seealso{
Other Enrich the genome regions: \code{\link{deepblue_enrich_regions_go_terms}},
  \code{\link{deepblue_enrich_regions_overlap}}
}
