
############################################
### Helper functions for Elo calculation ###
############################################

get_k <- function(m, k0 = 20) {
  k = case_when(
    m <= 1 ~ k0,
    m == 2 ~ k0 * 1.5,
    m == 3 ~ k0 * 1.75,
    m >= 4 ~ k0 * (1.75 + (m - 3) / 8)
  )
  return(k)
}

get_prediction <- function(elo_diff, home_adv) {
  d <- elo_diff + home_adv
  prediction <- 1 / (10 ^ (-d / 400) + 1)
  return(prediction)
}

get_elo_change <- function(prediction, result, margin) {
  k <- get_k(margin)
  elo_change <- (result - prediction) * k
  return(elo_change)
}