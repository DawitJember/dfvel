#' Compute weight velocity z score
#'
#' This function compute weight velocity Z score based on the WHO standard
#'
#' @param data
#'
#' @return weight velocity z score
#' @export
#'
#' @examples
#' #Get individual weight velocity z score
#' wvelzscore(data)
wvelzscore <- function(data){

  require(dplyr)

  calcw <- data %>%
    select(ID, time, age, weight, wl, wm, ws, Delta) %>%
    group_by(ID) %>%
    mutate(
      agedif = c(NA, diff(age)),
      wtdif = c(NA, diff(weight)),
      Delta<-min(abs(wtdif)),
      wvel = wtdif/agedif,
      firstwz = ((wtdif+Delta/wm)^(wl) - 1) / (ws * wl),

      sd2pw = ifelse(firstwz > 2, wm * (1 + wl * ws*(2))^(1/wl), NA),
      sd3pw = ifelse(firstwz > 3, wm *(1 + wl* ws*(3))^(1/wl), NA),
      sd23pw = ifelse(firstwz > 3, sd3pw - sd2pw, NA),

      sd2ngw = ifelse(firstwz < -2, wm * (1 + wl * ws*(-2))^(1/wl), NA),
      sd3ngw = ifelse(firstwz < -3, wm * (1 + wl * ws*(-3))^(1/wl), NA),
      sd32ngw = ifelse(firstwz < -3, sd2ngw - sd3ngw, NA),

      finalwz = ifelse(firstwz > 3,
                       3 + (wtdif - sd3pw)/sd23pw,
                       ifelse(firstwz < -3,
                              -3 + (wtdif - sd3ngw)/sd32ngw,
                              firstwz))
    ) %>%
    ungroup()
  rslt <- calcw %>%
    select(ID, time, age, weight, wl, wm, ws, Delta, agedif, wtdif, wvel, firstwz, sd3pw, sd2pw, sd23pw, sd2ngw, sd3ngw, sd32ngw, finalwz)

  return(rslt)
}
