#' Compute length velocity z score
#'
#' This function compute length velocity z score among children under
#'    two years of age using the 2009 WHO growth velocity standard
#'
#' @param input data with ID, time, age, height, hl, hm, hs
#'
#' @return a tibble with individual z score
#' @export
#'
#' @examples
#' #example code
#' hvelzscore(data)
hvelzscore <- function(data){

  require(dplyr)

  calculated <- data %>%
    select(ID, time, age, height, hl, hm, hs) %>%
    group_by(ID) %>%
    mutate(
      agedif= c(NA, diff(age)),
      htd=c(NA, diff(height)),
      htdif=htd + 0.01,
      hvel=htdif/agedif,
      firsthz=((htdif/hm)^(hl) - 1) / (hs * hl),

      sd3psh = ifelse(firsthz > 2, hm *(1 + hl* hs*(3))^(1/hl), NA),
      sd2psh = ifelse(firsthz > 3, hm * (1 + hl * hs*(2))^(1/hl), NA),
      sd23psh = ifelse(firsthz > 3, sd3psh - sd2psh, NA),

      sd2ngh = ifelse(firsthz < -2, hm * (1 + hl * hs*(-2))^(1/hl), NA),
      sd3ngh = ifelse(firsthz < -3, hm * (1 + hl * hs*(-3))^(1/hl), NA),
      sd32ngh = ifelse(firsthz < -3, sd2ngh - sd3ngh, NA),

      finalhz = ifelse(firsthz > 3,
                       3 + (htdif - sd3psh)/sd23psh,
                       ifelse(firsthz < -3,
                              -3 + (htdif - sd3ngh)/sd32ngh,
                              firsthz))
    ) %>%
    ungroup()
  result <- calculated %>%
    select(ID, time, age, height, hl, hm, hs, agedif, htdif, hvel, firsthz, sd3psh, sd2psh, sd23psh, sd2ngh, sd3ngh, sd32ngh, finalhz)

  return(result)
}
