#' Instantaneous fishing mortality rate (F)
#'
#' Instantaneous annual exponential rate at which fish are
#' being removed from the population due to fishing. Many other platforms
#' distinguish between different F. For example, looking at ASAP, we have
#' Average F (across a certain age range), Total F (by age), N-weighted,
#' B-weighted. ICES calls Average F Fbar. I think these both (SAM definitely
#'  does) incorporate selectivity. Apical F - at the peak of selectivity curve?
#'
#' @format
#' \describe{
#' \item{Examples}{Fun (ICES) is fishing mortality of unwanted catch}
#' \item{Rationale}{It is often not clear whether fishing mortality refers to the
#' instantaneous rate or finite rate, where the latter would apply to a fixed
#' time period. Some suggest using upper case for instantaneous rates and lower
#' case for finite rates. For example an F of 0.5 would correspond to 40% of
#' the population being removed or a finite rate of 0.4. Furthermore, F can
#' sometimes pertain to just a range of the age structure and may or may not
#' account for selectivity.}
#' \item{Alternatives}{harvest rate, exploitation rate, finite fishing mortality,
#' apical F}
#' \item{Range of possible values}{0--Inf}
#' \item{Units}{\code{time^{-1}}}
#' }
#' @source see
InstantaneousFishingMortalityRate <- NULL
