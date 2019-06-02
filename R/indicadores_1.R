#' Calcula o indicador 1A: "Percentual de crianças de 4 a 5 anos na pre-escola"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informacoes no console se True
#' @return Indicador 1A em porcentagem
#' @import dplyr
#' @export
calc_indicador_1A <- function(df, verbose = TRUE) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V1023, V2007, V2009, V3002, V1032, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 == 4 | V2009 == 5) %>%
    filter(V3002 != "")

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)

  total_criancas_4_5_anos_escola_ponderado <- sum(df_criancas_escola$V1032)
  total_criancas_4_5_anos_ponderado <- sum(df_criancas$V1032)
  indicador_1A <- (total_criancas_4_5_anos_escola_ponderado/total_criancas_4_5_anos_ponderado)*100

  if (verbose == TRUE) {
    print(sprintf("Total de crianças de 4 a 5 anos na escola: %f", total_criancas_4_5_anos_escola_ponderado))
    print(sprintf("Total de crianças de 4 a 5 anos: %f", total_criancas_4_5_anos_ponderado))
    print(sprintf("Indicador 1A: %f", indicador_1A))
  }

  return(indicador_1A)
}

#' Calcula o indicador 1B: "Percentual da população de 0 a 3 anos que frequenta a escola"
#'
#' @param df DataFrame com dados carregados da PNAD Continua trimestral
#' @param verbose exibe informacoes no console se True
#' @return Indicador 1B em porcentagem
#' @import dplyr
#' @export
calc_indicador_1B <- function(df, verbose = TRUE) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V20082,V1023, V3003, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 0 & V2009 <= 3) %>%
    filter(!is.na(V3002))

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)
  total_criancas_0_3_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
  total_criancas_0_3_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
  indicador_1B <- (total_criancas_0_3_anos_escola_ponderado/total_criancas_0_3_anos_ponderado)*100

  if (verbose == TRUE) {
    print(sprintf("Total de crianças de 0 a 3 anos na escola: %f", total_criancas_0_3_anos_escola_ponderado))
    print(sprintf("Total de crianças de 0 a 3 anos: %f", total_criancas_0_3_anos_ponderado))
    print(sprintf("Indicador 1B: %f", indicador_1B))
  }

  return(indicador_1B)
}

#' Calcula o indicador 1A: "Percentual de crianças de 4 a 5 anos na pre-escola"
#'
#' @param df_mat DataFrame com dados carregados da PNAD Contínua trimestral
#' @param df_pop DataFrame com dados carregados com a previsão da populacao de Recife
#' @param verbose exibe informacoes no console se True
#' @return Indicador 1A em porcentagem
#' @import dplyr
#' @export
calc_indicador_1A_censo <- function(df_mat, df_pop, verbose = TRUE) {
  num_mat_4_a_5 <- df_mat %>%
    filter(NU_IDADE_REFERENCIA == 4 | NU_IDADE_REFERENCIA == 5) %>%
    filter(TP_ETAPA_ENSINO == 2) %>% nrow

  ano <- df_mat$NU_ANO_CENSO[1]
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
#' @param df_mat DataFrame com dados carregados da PNAD Contínua trimestral
#' @param df_pop DataFrame com dados carregados com a previsão da populacao de Recife
#' @param verbose exibe informacoes no console se True
#' @return Indicador 1B em porcentagem
#' @import dplyr
#' @export
calc_indicador_1B_censo <- function(df_mat, df_pop, verbose = TRUE) {
  num_mat_0_a_3 <- df_mat %>%
    filter(NU_IDADE_REFERENCIA <= 3) %>% nrow

  ano <- df_mat$NU_ANO_CENSO[1]
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
