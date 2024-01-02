#' Compute height velocity
#'
#' This function compute height velocity using the numeric method in the long
#' form data
#'
#' @param input data that contains ID, time, age, height
#'
#' @return a tibble that contains ID, time, age, height, agediff, htdiff, hvel
#' @export
#'
#' @examples
#' ##calculate height velocity
#' heightv<-htvel(sample_gdata)
htvel <- function(data){

  require(dplyr)

  hvcalc <- data %>%
    select(ID, time, age, height)%>%
    group_by(ID) %>%
    mutate(
      agediff = c(NA, diff(age)),
      htdiff = c(NA, diff(height)),
      hvel = htdiff/agediff,
    ) %>%
    ungroup()
  myHVdat<-hvcalc %>%
    select(ID, time, age, height, agediff, htdiff, hvel)

  return(myHVdat)
}
