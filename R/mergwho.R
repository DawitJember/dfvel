#' Merge the WHO growth velocity standard with your dataset
#'
#' This function merge the WHO growth velocity standard with your dataset.
#'
#' @param data input dataset
#' @param whodata input WHO standard data
#' @param age a numeric value in the data and whodata
#' @param sex a numeric value 1 for male and 2 for female
#' @param interval a numeric value indicating between measurement interval
#'
#' @return a tibble with merged dataset
#' @export
#'
#' @examples
#' #First get the WHO growth velocity standard
#' data(package="dfvel")
#' #view the data that you want in the package (for example)
#' View(wholengthv09)
#' #merge data and whodata by age, sex and interval
#' #Make sure the variables used to merge the data set have similar names
#' dfmerg(data=data, whodata=wholengthv09, age=age, sex=sex, interval=interval)
dfmerg<-function(data, whodata, age, sex, interval){
  require(dplyr)
  mergedd<-merge(data, whodata, by=c("age", "sex", "interval"))
  return(mergedd)
}
