% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepblue.R
\name{deepblue_preview_experiment}
\alias{deepblue_preview_experiment}
\title{preview_experiment}
\usage{
deepblue_preview_experiment(experiment_name = NULL,
  user_key = deepblue_options("user_key"))
}
\arguments{
\item{experiment_name}{- A string (name(s) of selected experiment(s))}

\item{user_key}{- A string (users token key)}
}
\value{
experiment - A string (experiment's regions)
}
\description{
List the DeepBlue Experiments that matches the search criteria defined by this command parameters.
}
\examples{
deepblue_preview_experiment('S00JJRH1.ERX683143.H3K4me3.bwa.GRCh38.20150527.bed')

}
\seealso{
Other Inserting and listing experiments: \code{\link{deepblue_collection_experiments_count}},
  \code{\link{deepblue_faceting_experiments}},
  \code{\link{deepblue_list_experiments}},
  \code{\link{deepblue_list_recent_experiments}},
  \code{\link{deepblue_list_similar_experiments}}
}
