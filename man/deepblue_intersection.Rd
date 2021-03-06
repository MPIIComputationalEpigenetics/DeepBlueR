% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepblue.R
\name{deepblue_intersection}
\alias{deepblue_intersection}
\title{intersection}
\usage{
deepblue_intersection(query_data_id = NULL, query_filter_id = NULL,
  user_key = deepblue_options("user_key"))
}
\arguments{
\item{query_data_id}{- A string (query data that will be filtered.)}

\item{query_filter_id}{- A string (query containing the regions that the regions of the query_data_id must overlap.)}

\item{user_key}{- A string (users token key)}
}
\value{
id - A string (id of the new query)
}
\description{
Select genomic regions that intersect with at least one region of the second query. This command is a simplified version of the 'overlap' command.
}
\examples{
annotation_id = deepblue_select_annotations(
    annotation_name="CpG Islands",
    genome="hg19", chromosome="chr1")
data_id = deepblue_select_experiments(
    experiment_name="E002-H3K9ac.narrowPeak.bed")
deepblue_intersection(query_data_id = annotation_id,
    query_filter_id = data_id)

}
\seealso{
Other Operating on the data regions: \code{\link{deepblue_aggregate}},
  \code{\link{deepblue_binning}},
  \code{\link{deepblue_count_regions}},
  \code{\link{deepblue_coverage}},
  \code{\link{deepblue_distinct_column_values}},
  \code{\link{deepblue_extend}},
  \code{\link{deepblue_filter_regions}},
  \code{\link{deepblue_flank}},
  \code{\link{deepblue_get_experiments_by_query}},
  \code{\link{deepblue_get_regions}},
  \code{\link{deepblue_input_regions}},
  \code{\link{deepblue_merge_queries}},
  \code{\link{deepblue_overlap}},
  \code{\link{deepblue_query_cache}},
  \code{\link{deepblue_query_experiment_type}},
  \code{\link{deepblue_score_matrix}},
  \code{\link{deepblue_select_annotations}},
  \code{\link{deepblue_select_experiments}},
  \code{\link{deepblue_select_regions}},
  \code{\link{deepblue_tiling_regions}}
}
