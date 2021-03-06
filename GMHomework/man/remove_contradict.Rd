% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_contradict.R
\name{remove_contradict}
\alias{remove_contradict}
\title{Remove contradict ID}
\usage{
remove_contradict(data, ..., score, id)
}
\arguments{
\item{data}{a data frame}

\item{...}{variables or computations to group by.}

\item{score}{variable which is used for score}

\item{id}{key variable of the data frame}
}
\value{
dataframe
}
\description{
Return data table that remove respondents provided different answer in the same questions combination
}
\examples{
\dontrun{

remove_contradict(experiment_data, rtb, answer, response_id)

}

}
