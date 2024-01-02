#' Compute weight velocity
#'
#' This function compute weight velocity using the numeric method in the
#' long form data
#'
#' @param input data contains ID, time, age, weight
#'
#' @return a tibble that contains ID, time, age, weight, agediff, wtdiff, wvel
#' @export
#'
#' @examples
#' #Compute weight velocity
#' weightv<-wtvel(data)
wtvel <- function(data){

  require(dplyr)

  wvcalc <- data %>%
    select(ID, time, age, weight) %>%
    group_by(ID) %>%
    mutate(
      agediff = c(NA, diff(age)),
      wtdiff = c(NA, diff(weight)),
      wvel = wtdiff/agediff,
    ) %>%
    ungroup()
  mywdat<-wvcalc %>%
    select(ID, time, age, weight, agediff, wtdiff, wvel)
  return(mywdat)
}
