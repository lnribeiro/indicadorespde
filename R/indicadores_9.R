#' Calcula o indicador 9A: "Taxa de alfabetização da população de 15 anos ou mais de idade"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @return Indicador 9A em porcentagem
#' @import dplyr
#' @export
calc_indicador_9A <- function(df) {
  df_15 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "")

  df_15_le <- df_15 %>% filter(V3001 == 1)

  num <- sum(df_15_le$V1028)
  tot <- sum(df_15$V1028)
  indicador_9A <- (num/tot)*100

  return(indicador_9A)
}

#' Calcula o indicador 9B: "Taxa de analfabetismo funcional da população de 15 anos ou mais de idade"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @return Indicador 9B em porcentagem
#' @import dplyr
#' @export
calc_indicador_9B <- function(df) {
  df_15_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001, VD3002) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "")

  df_15_nao_le_b <- df_15_b %>%
    filter(V3001 == 2) %>%
    filter(as.numeric(VD3002) < 4)

  num_b <- sum(df_15_nao_le_b$V1028)
  tot_b <- sum(df_15_b$V1028)
  indicador_9B <- (num_b/tot_b)*100

  return(indicador_9B)
}
