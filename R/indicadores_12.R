#' Calcula o indicador 12A: "Taxa de escolarização bruta na educação superior da população de 18 a 24 anos"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 12A em porcentagem
#' @import dplyr
#' @export
calc_indicador_12A <- function(df, verbose = TRUE) {
  df_18_24 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3003, V3003A, V3002A) %>%
    filter(V2009 >= 18 & V2009 <= 24)

  df_18_24_grad <- df_18_24 %>% filter(V3003A == "08")

  num <- sum(df_18_24_grad$V1028)
  tot <- sum(df_18_24$V1028)
  indicador_12A <- (num/tot)*100

  if (verbose == TRUE) {
    print(sprintf("Número (ponderado) de graduados entre 18 a 24 anos: %f", num))
    print(sprintf("Número (ponderado) da população entre 18 a 24 anos: %f", tot))
    print(sprintf("Indicador 12A: %f", indicador_12A))
  }

  return(indicador_12A)
}

#' Calcula o indicador 12B: "Taxa de escolarização líquida ajustada na educação superior da população de 18 a 24 anos"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 12B em porcentagem
#' @import dplyr
#' @export
calc_indicador_12B <- function(df, verbose = TRUE) {
  df_18_24_b <- df %>% select(Ano, RM_RIDE,  V1023, UF, V2009, V1028, V3003, V3003A, V3009, V3009A, V3014) %>%
    filter(V2009 >= 18 & V2009 <= 24)

  df_18_24_grad_b <- df_18_24_b %>%
    filter( (V3003A == "08" | V3003A == "09" | V3003A == "10" | V3003A == "11") | (V3009A == "13" | V3009A == "14" | V3009A == "15") | (V3009A == "12" & V3014 == "1"))

  num_b <- sum(df_18_24_grad_b$V1028)
  tot_b <- sum(df_18_24_b$V1028)
  indicador_12B <- (num_b/tot_b)*100

  if (verbose == TRUE) {
    print(sprintf("Número (ponderado) de graduados entre 18 a 24 anos (ajustada): %f", num_b))
    print(sprintf("Número (ponderado) da população entre 18 a 24 anos: %f", tot_b))
    print(sprintf("Indicador 12A: %f", indicador_12B))
  }

  return(indicador_12B)
}
