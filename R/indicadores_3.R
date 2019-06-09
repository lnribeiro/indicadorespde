#' Calcula o indicador 3A: "Percentual da população de 15 a 17 anos que frequenta a escola"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @param ano ano do censo fornecido (>= 2014)
#' @param verbose be communicative?
#' @import dplyr
#' @export
calc_indicador_3A_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  # obter numerador
  if (ano == 2014) {
    num_mat_15_a_17 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA >= 15 & NUM_IDADE_REFERENCIA <= 17) %>%
      filter(!is.na(FK_COD_ETAPA_ENSINO)) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_15_a_17 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA >= 15 & NU_IDADE_REFERENCIA <= 17) %>%
      filter(!is.na(TP_ETAPA_ENSINO)) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_15_a_17 <- sum(df_pop[[colname]][16:18])

  indicador_3A <- (num_mat_15_a_17/num_pop_15_a_17)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 15 a 17 que frequenta escola: %f", num_mat_15_a_17))
    print(sprintf("num população 15 a 17: %f", num_pop_15_a_17))
    print(sprintf("indicador 3A: %f", indicador_3A))
  }

  return(indicador_3A)
}

#' Calcula o indicador 3B: "Taxa líquida de matrícula no Ensino Médio"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @param ano ano do censo fornecido (>= 2014)
#' @param verbose be communicative?
#' @import dplyr
#' @export
calc_indicador_3B_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  ens_fund14 <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 40, 41, 43, 44, 51, 60, 65)
  ens_fund <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 41, 65, 69, 70, 72, 73)

  # obter numerador
  if (ano == 2014) {
    num_mat_15_a_17 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA >= 15 & NUM_IDADE_REFERENCIA <= 17) %>%
      filter(!(FK_COD_ETAPA_ENSINO %in% ens_fund14)) %>%
      filter(!is.na(FK_COD_ETAPA_ENSINO)) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_15_a_17 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA >= 15 & NU_IDADE_REFERENCIA <= 17) %>%
      filter(! (TP_ETAPA_ENSINO %in% ens_fund) ) %>%
      filter(!is.na(TP_ETAPA_ENSINO)) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_15_a_17 <- sum(df_pop[[colname]][16:18])

  indicador_3B <- (num_mat_15_a_17/num_pop_15_a_17)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 15 a 17 anos ens fund conc: %f", num_mat_15_a_17))
    print(sprintf("num populacao 15 a 17 anos: %f", num_pop_15_a_17))
    print(sprintf("indicador 3B: %f", indicador_3B))
  }

  return(indicador_3B)
}
