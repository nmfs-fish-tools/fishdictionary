#' Spawning Biomass (SB)
#'
#' The mass of fish (males and females or females only) in the 
#' population that contribute to reproduction. Often conventionally 
#' defined as the product of weight at age and the proportion 
#' mature at age. Alternatively, it can be defined as the biomass of 
#' all individuals at or above “age at 50 percent maturity” or “size 
#' at 50 percent maturity” or the total biomass of fish of reproductive 
#' age during the breeding season of a population. Spawning biomass depends on the 
#' abundance of the various age classes composing the population and their 
#' past exploitation pattern, rate of growth, fishing and natural mortality
#' rates, onset of sexual maturity, and environmental conditions. Most often 
#' used as a proxy for measuring egg production.
#' 
#'
#' @format
#' \describe{
#' \item{Examples}{female spawning biomass}
#' \item{Rationale}{Spawning biomass and spawning stock biomass have
#' both been used historically, though the former is shorter without
#' sacrificing clarity. For single-sex models, spawning biomass often
#' pertains only to females but text should be specific, e.g., female
#' spawning biomass. The alternative, spawning output, is only viable
#' for species that exhibit multiple spawning events per fish within a
#' time period and have pelagic eggs.}
#' \item{Alternatives}{spawning stock biomass, spawning output (eggs),
#' spawning stock output (eggs)}
#' \item{Range of possible values}{0--Inf}
#' \item{Units}{mt}
#' }
#' @export
SpawningBiomass <- function(){
    w <- Weight()
    naa <- NumbersAtAge()
    return(w*naa)
}
