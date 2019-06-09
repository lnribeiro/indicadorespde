#' Calcula o indicador 1A: "Percentual de crianças de 4 a 5 anos na pre-escola"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @param ano ano do censo fornecido (>= 2014)
#' @param verbose be communicative?
#' @import dplyr
#' @export
calc_indicador_1A_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  # obter numerador
  if (ano == 2014) {
    num_mat_4_a_5 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA == 4 | NUM_IDADE_REFERENCIA == 5) %>%
      filter(FK_COD_ETAPA_ENSINO == 1 | FK_COD_ETAPA_ENSINO == 2) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_4_a_5 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA == 4 | NU_IDADE_REFERENCIA == 5) %>%
      filter(TP_ETAPA_ENSINO == 1 | TP_ETAPA_ENSINO == 2) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_4_a_5 <- sum(df_pop[[colname]][5:6])

  indicador_1A <- (num_mat_4_a_5/num_pop_4_a_5)*100

  if (verbose == TRUE) {
    print(sprintf("num mat pré-escola 4 a 5: %f", num_mat_4_a_5))
    print(sprintf("num população 4 a 5: %f", num_pop_4_a_5))
    print(sprintf("indicador 1A: %f", indicador_1A))
  }

  return(indicador_1A)
}

#' Calcula o indicador 1B: "Percentual da população de 0 a 3 anos que frequenta a escola"
#'
#' @param df_mat tabela matrícula do censo escolar (a partir de 2014)
#' @param df_pop tabela com total da população fornecido pela prefeitura
#' @import dplyr
#' @export
calc_indicador_1B_censo <- function(df_mat, df_pop, ano, verbose = TRUE) {
  # obter numerador
  if (ano == 2014) {
    num_mat_0_a_3 <- df_mat %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NUM_IDADE_REFERENCIA <= 3) %>%
      filter(FK_COD_ETAPA_ENSINO == 1 | FK_COD_ETAPA_ENSINO == 2) %>%
      nrow
  } else if (ano >= 2015) {
    num_mat_0_a_3 <- df_mat %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_IDADE_REFERENCIA <= 3) %>%
      filter(TP_ETAPA_ENSINO == 1 | TP_ETAPA_ENSINO == 2) %>%
      nrow
  } else {
    stop("Período não suportado.")
  }

  # obter denominador
  colname <- paste0('X', ano)
  num_pop_0_a_3 <- sum(df_pop[[colname]][1:4])

  indicador_1B <- (num_mat_0_a_3/num_pop_0_a_3)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 0 a 3: %f", num_mat_0_a_3))
    print(sprintf("num população 0 a 3: %f", num_pop_0_a_3))
    print(sprintf("indicador 1B: %f", indicador_1B))
  }

  return(indicador_1B)
}
