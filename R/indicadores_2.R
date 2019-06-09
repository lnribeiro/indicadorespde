#' Calcula o indicador 2A: "Percentual a população de 6 a 14 anos que frequenta a escola"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @param ano ano do censo fornecido (>= 2014)
#' @param verbose be communicative?
#' @import dplyr
#' @export
calc_indicador_2A_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  # obter numerador
  if (ano == 2014) {
    num_mat_6_a_14 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA >= 6 & NUM_IDADE_REFERENCIA <= 14) %>%
      filter(!is.na(FK_COD_ETAPA_ENSINO)) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_6_a_14 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA >= 6 & NU_IDADE_REFERENCIA <= 14) %>%
      filter(!is.na(TP_ETAPA_ENSINO)) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_6_a_14 <- sum(df_pop[[colname]][7:15])

  indicador_2A <- (num_mat_6_a_14/num_pop_6_a_14)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 6 a 14: %f", num_mat_6_a_14))
    print(sprintf("num população 6 a 14: %f", num_pop_6_a_14))
    print(sprintf("indicador 2A: %f", indicador_2A))
  }

  return(indicador_2A)
}

#' Calcula o indicador 2B: "Percentual de pessoas de 16 anos com pelo menos o ensino fundamental concluído"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @param ano ano do censo fornecido (>= 2014)
#' @param verbose be communicative?
#' @import dplyr
#' @export
calc_indicador_2B_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  ens_fund14 <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 40, 41, 43, 44, 51, 60, 65)
  ens_fund <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 41, 65, 69, 70, 72, 73)

  # obter numerador
  if (ano == 2014) {
    num_mat_16 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA == 16) %>%
      filter(!(FK_COD_ETAPA_ENSINO %in% ens_fund14)) %>%
      filter(!is.na(FK_COD_ETAPA_ENSINO)) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_16 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA == 16) %>%
      filter(! (TP_ETAPA_ENSINO %in% ens_fund) ) %>%
      filter(!is.na(TP_ETAPA_ENSINO)) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_16 <- sum(df_pop[[colname]][17])

  indicador_2B <- (num_mat_16/num_pop_16)*100

  if (verbose == TRUE) {
    print(sprintf("num mat ens fund conc 16 anos: %f", num_mat_16))
    print(sprintf("num populacao 16: %f", num_pop_16))
    print(sprintf("indicador 2B: %f", indicador_2B))
  }

  return(indicador_2B)
}
