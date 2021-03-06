% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepblue.R
\name{deepblue_chromosomes}
\alias{deepblue_chromosomes}
\title{chromosomes}
\usage{
deepblue_chromosomes(genome = NULL, user_key = deepblue_options("user_key"))
}
\arguments{
\item{genome}{- A string (the target genome)}

\item{user_key}{- A string (users token key)}
}
\value{
chromosomes - A array (A list containing all chromosomes, with theirs names and sizes)
}
\description{
List the chromosomes of a given Genome.
}
\examples{
deepblue_chromosomes(genome = "g1")

}
\seealso{
Other Inserting and listing genomes: \code{\link{deepblue_list_genomes}},
  \code{\link{deepblue_list_similar_genomes}}
}
