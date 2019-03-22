#' Calcula o indicador 1A: "Percentual de crianças de 4 a 5 anos na pré-escola"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @return Indicador 1A em porcentagem
#' @import dplyr
#' @export
calc_indicador_1A <- function(df) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V1023, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 == 4 | V2009 == 5) %>%
    filter(V3002 != "")

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)

  total_criancas_4_5_anos_escola_ponderado <- sum(df_criancas_escola$V1028)
  total_criancas_4_5_anos_ponderado <- sum(df_criancas$V1028)
  indicador_1A <- (total_criancas_4_5_anos_escola_ponderado/total_criancas_4_5_anos_ponderado)*100

  return(indicador_1A)
}

#' Calcula o indicador 1B: "Percentual da população de 0 a 3 anos que frequenta a escola"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @return Indicador 1B em porcentagem
#' @import dplyr
#' @export
calc_indicador_1B <- function(df) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V20082,V1023, V3003, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 0 & V2009 <= 3) %>%
    filter(!is.na(V3002))

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)
  total_criancas_0_3_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
  total_criancas_0_3_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
  indicador_1B <- (total_criancas_0_3_anos_escola_ponderado/total_criancas_0_3_anos_ponderado)*100

  return(indicador_1B)
}
