#' Calcula o indicador 5A: "Taxa de estudantes com proficiência insuficiente em Leitura"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @return Indicador 5A em porcentagem
#' @import dplyr
#' @export
calc_indicador_5A <- function(df) {
  count_lpo <-  df %>% count(NIVEL_LPO)
  indicador_5A <- 100*(count_lpo[1,]$n + count_lpo[2,]$n)/nrow(df)
  return(indicador_5A)
}

#' Calcula o indicador 5B: "Taxa de estudantes com proficiência insuficiente em Escrita"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @return Indicador 5B em porcentagem
#' @import dplyr
#' @export
calc_indicador_5B <- function(df) {
  count_lpd <- df %>% count(NIVEL_LPD)
  indicador_5B <- 100*(count_lpd[1,]$n + count_lpd[2,]$n + count_lpd[3,]$n)/nrow(df)
  return(indicador_5B)
}

#' Calcula o indicador 5C: "Taxa de estudantes com proficiência insuficiente em Matemática"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @return Indicador 5C em porcentagem
#' @import dplyr
#' @export
calc_indicador_5C <- function(df) {
  count_mt <- df %>% count(NIVEL_MT)
  indicador_5C <- 100*(count_mt[1,]$n + count_mt[2,]$n + count_mt[3,]$n)/nrow(df)
  return(indicador_5C)
}
