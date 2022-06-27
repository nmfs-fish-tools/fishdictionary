#' Instantaneous mortality rate (Z)
#' 
#' The sum of instantaneous natural mortality (M) and fishing mortality (F) 
#' rates or the fraction of the population that dies in a very short, i.e., 
#' instantaneous, period of time. Instantaneous rates are sometimes more easily
#'  interpreted as an annual rate, which can be done using the following formula:
#' \code{A = 1 - e^{-x}}.
#' 
#' @format A schema with the following fields:
#' \describe{
#' \item{Examples}{An instantaneous mortality rate of 0.0 means that all
#'  individual survive and the resulting annual survival rate is 100%.}
#' \item{Rationale}{Standardizing notation for instantaneous rates of 
#' mortality is easy because Z appears to be one of the most well-adopted 
#' acronyms across a great number of disciplines.}
#' \item{Alternatives}{}
#' \item{Range of possible values}{0--Inf}
#' \item{Units}{ \code{time^{-1}}}
#' }
InstaneousMortalityRate <- NULL