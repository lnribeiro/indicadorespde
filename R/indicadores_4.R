#' Calcula o indicador 4: "Percentual da população de 4 a 17 anos com deficiência que frequenta a escola"
#'
#' @param df DataFrame com dados carregados do Censo Demográfico de Pernambuco
#' @param verbose exibe informações no console se True
#' @return Indicador 4 em porcentagem
#' @import dplyr
#' @export
calc_indicador_4 <- function(df, verbose = TRUE) {
  df_pop <- df %>% select(V6036, V0010, V0613, V0614, V0615, V0616, V0617, V0628, V0633, V0634, V1001, V0001, V0601, V0606, V1004, V6531) %>%
    filter(V1004 == "11") %>% # Recife
    filter(V6036 >= 4 & V6036 <= 17) # Idade

  df_alvo <- df_pop %>% filter(V0614 == "1" | V0614 == "2" | V0615 == "1" | V0615 == "2" | V0616 == "1" | V0616 == "2" | V0617 == "1")

  df_alvo_escola <- df_alvo %>% filter(V0628 == "1" | V0628 == "2" | V0634 == "1" | as.numeric(V0633) >= 9)

  num_escola_ponderado <- sum(df_alvo_escola$V0010)
  num_total_ponderado <- sum(df_alvo$V0010)
  # num_escola_ponderado <- nrow(df_alvo_escola)
  # num_total_ponderado <- nrow(df_alvo)
  indicador_4 <- (num_escola_ponderado/num_total_ponderado)*100

  if (verbose == TRUE) {
    print(sprintf("População (ponderada) de 4 a 17 anos com deficiência que frequenta a escola: %f", num_escola_ponderado))
    print(sprintf("População (ponderada) de 4 a 17 anos com deficiência: %f", num_total_ponderado))
    print(sprintf("Indicador 4: %f", indicador_4))
  }

  return(indicador_4)
}
