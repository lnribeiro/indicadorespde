#' Calcula o indicador 5A: "Taxa de estudantes com proficiência insuficiente em Leitura"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @param verbose exibe informações no console se True
#' @return Indicador 5A em porcentagem
#' @import dplyr
#' @export
calc_indicador_5A <- function(df, verbose = TRUE) {
  count_lpo <-  df %>% count(NIVEL_LPO)
  indicador_5A <- 100*(count_lpo[1,]$n)/sum(count_lpo[1:4,]$n)

  if (verbose == TRUE) {
    print(sprintf("Nível 1 Língua Portuguesa Leitura: %f", count_lpo[1,]$n))
    print(sprintf("Nível 2 Língua Portuguesa Leitura: %f", count_lpo[2,]$n))
    print(sprintf("Nível 3 Língua Portuguesa Leitura: %f", count_lpo[3,]$n))
    print(sprintf("Nível 4 Língua Portuguesa Leitura: %f", count_lpo[4,]$n))
    print(sprintf("Indicador 5A: %f", indicador_5A))
  }

  return(indicador_5A)
}

#' Calcula o indicador 5B: "Taxa de estudantes com proficiência insuficiente em Escrita"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @param verbose exibe informações no console se True
#' @return Indicador 5B em porcentagem
#' @import dplyr
#' @export
calc_indicador_5B <- function(df, verbose = TRUE) {
  count_lpd <- df %>% count(NIVEL_LPD)
  indicador_5B <- 100*(count_lpd[1,]$n + count_lpd[2,]$n + count_lpd[3,]$n)/sum(count_lpd[1:5,]$n)

  if (verbose == TRUE) {
    print(sprintf("Nível 1 Língua Portuguesa Escrita: %f", count_lpd[1,]$n))
    print(sprintf("Nível 2 Língua Portuguesa Escrita: %f", count_lpd[2,]$n))
    print(sprintf("Nível 3 Língua Portuguesa Escrita: %f", count_lpd[3,]$n))
    print(sprintf("Nível 4 Língua Portuguesa Escrita: %f", count_lpd[4,]$n))
    print(sprintf("Nível 5 Língua Portuguesa Escrita: %f", count_lpd[5,]$n))
    print(sprintf("Indicador 5B: %f", indicador_5B))
  }

  return(indicador_5B)
}

#' Calcula o indicador 5C: "Taxa de estudantes com proficiência insuficiente em Matemática"
#'
#' @param df DataFrame com dados carregados da Avaliação Nacional da Alfabetização
#' @param verbose exibe informações no console se True
#' @return Indicador 5C em porcentagem
#' @import dplyr
#' @export
calc_indicador_5C <- function(df, verbose = TRUE) {
  count_mt <- df %>% count(NIVEL_MT)
  indicador_5C <- 100*(count_mt[1,]$n + count_mt[2,]$n)/sum(count_mt[1:4,]$n)

  if (verbose == TRUE) {
    print(sprintf("Nível 1 Matemática: %f", count_mt[1,]$n))
    print(sprintf("Nível 2 Matemática: %f", count_mt[2,]$n))
    print(sprintf("Nível 3 Matemática: %f", count_mt[3,]$n))
    print(sprintf("Nível 4 Matemática: %f", count_mt[4,]$n))
    print(sprintf("Indicador 5C: %f", indicador_5C))
  }

  return(indicador_5C)
}
