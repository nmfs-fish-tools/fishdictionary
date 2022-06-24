#' Biomass
#' 
#' Weight of fish within a stock. If referring to a certain portion of the stock, it should be made clear what portion of the stock the biomass pertains to. Model output related to biomass is assumed to be measured at the beginning of the year unless otherwise specified.
#' 
#' @format A schema with the following fields:
#' \describe{
#' \item{Examples}{spawning biomass, age three-plus biomass, exploitable 
#' biomass, January 1 biomass}
#' \item{Rationale}{Measurements of biomass should be better defined making
#'  it clear what year classes and sexes it includes. Historically, verbose 
#' labels, e.g., January 1 biomass of age-three plus fish in 2022, are not 
#' typically used; instead, labels are short, e.g., 2022 3+ biomass. 
#' Additional ambiguity can come from the lack of knowledge regarding the 
#' unit of measurement, which should always be metric tons. Some alternatives
#'  are not interchangeable because they are in different units, e.g., 
#' abundance, which is in terms of numbers rather than weight.}
#' \item{Alternatives}{stock biomass, total biomass, abundance (numbers of fish), biomass wet weight, biomass index}
#' \item{Range of possible values}{0--Inf}
#' \item{Units}{mt}
#' }
Biomass <- NULL
