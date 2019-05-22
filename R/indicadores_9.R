#' Calcula o indicador 9A: "Taxa de alfabetização da população de 15 anos ou mais de idade"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 9A em porcentagem
#' @import dplyr
#' @export
calc_indicador_9A <- function(df, verbose = TRUE) {
  df_15 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "")

  df_15_le <- df_15 %>% filter(V3001 == 1)

  num <- sum(df_15_le$V1028)
  tot <- sum(df_15$V1028)
  indicador_9A <- (num/tot)*100

  if (verbose == TRUE) {
    print(sprintf("Total (ponderado) da população de 15 ou mais anos que lê: %f", num))
    print(sprintf("Total (ponderado) da população de 15 ou mais anos: %f", tot))
    print(sprintf("Indicador 9A: %f", indicador_9A))
  }

  return(indicador_9A)
}

#' Calcula o indicador 9B: "Taxa de analfabetismo funcional da população de 15 anos ou mais de idade"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 9B em porcentagem
#' @import dplyr
#' @export
calc_indicador_9B <- function(df, verbose = TRUE) {
  # !!! agora VD3002 é VD3005 !!!
  df_15_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001, VD3005) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "")

  df_15_nao_le_b <- df_15_b %>%
    filter(V3001 == 2) %>%
    filter(as.numeric(VD3005) < 4)

  num_b <- sum(df_15_nao_le_b$V1028)
  tot_b <- sum(df_15_b$V1028)
  indicador_9B <- (num_b/tot_b)*100

  if (verbose == TRUE) {
    print(sprintf("Total (ponderado) da população de 15 ou mais anos que NÃO lê: %f", num_b))
    print(sprintf("Total (ponderado) da população de 15 ou mais anos: %f", tot_b))
    print(sprintf("Indicador 9B: %f", indicador_9B))
  }

  return(indicador_9B)
}
