#' Age composition
#' 
#' A matrix of proportions of observed ages per age categories that are 
#' typically summarized by time period, data source, and sex.
#' 
#' @format 
#' \describe{
#' \item{Examples} 
#' \item{Rationale}{Composition is more commonly used than frequency but 
#' storing data as compositions removes information about the number of fish
#' that were included in the sample. Typically, the sample size associated
#' with a given composition will not be based on the number of fish and is
#' instead based on the number of tows, and thus, it is important to store the
#' data as frequencies rather than compositions such that information is not
#' lost.}
#' \item{Alternatives}{age frequency, age observation}
#' \item{Range of possible values}{0--1}
#' \item{Units}
#' }
#' @source see
AgeComposition <- NULL
