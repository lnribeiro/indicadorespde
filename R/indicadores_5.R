calc_indicador_5A <- function(df) {
  count_lpo <- df %>% count(NIVEL_LPO)
  indicador_5A <- (count_lpo[3,]$n + count_lpo[4,]$n)/nrow(df) # suficiente
  # indicador_5A <- (count_lpo[1,]$n)/nrow(df) # insuficiente
  return(indicador_5A)
}

calc_indicador_5B <- function(df) {
  count_lpd <- df %>% count(NIVEL_LPD)
  indicador_5B <- (count_lpd[4,]$n + count_lpd[5,]$n)/nrow(df) # suficiente
  # indicador_5B <- (count_lpd[1,]$n + count_lpd[2,]$n + count_lpd[3,]$n)/nrow(df) # insuficiente
  return(indicador_5B)
}

calc_indicador_5C <- function(df) {
  count_mt <- df %>% count(NIVEL_MT)
  indicador_5C <- (count_mt[3,]$n + count_mt[4,]$n)/nrow(df) # suficiente
  # indicador_5C <- (count_mt[1,]$n + count_mt[2,]$n)/nrow(df) # insuficiente
  return(indicador_5C)
}
