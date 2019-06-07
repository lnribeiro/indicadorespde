#' Calcula o indicador 4: "Percentual da população de 4 a 17 anos com deficiência que frequenta a escola"
#'
#' @import dplyr
#' @export
calc_indicador_4 <- function(df, peso = FALSE, verbose = TRUE) {
  df_pop <- df %>%
    filter(V0002 == "11606") %>% # Recife
    filter(V6036 >= 4 & V6036 <= 17) # Idade

  df_alvo <- df_pop %>% filter(V0614 == "1" | V0614 == "2" | V0615 == "1" | V0615 == "2" | V0616 == "1" | V0616 == "2" | V0617 == "1")
  df_alvo_escola <- df_alvo %>% filter(V0628 == "1" | V0628 == "2" | V0634 == "1" | as.numeric(V0633) >= 9)

  num_escola_ponderado <- sum(df_alvo_escola$V0010)
  num_total_ponderado <- sum(df_alvo$V0010)
  indicador_4_ponderado <- (num_escola_ponderado/num_total_ponderado)*100

  num_escola <- nrow(df_alvo_escola)
  num_total <- nrow(df_alvo)
  indicador_4 <- (num_escola/num_total)*100

  if (verbose == TRUE) {
    print(sprintf("População (ponderada) de 4 a 17 anos com deficiência que frequenta a escola: %f", num_escola_ponderado))
    print(sprintf("População (ponderada) de 4 a 17 anos com deficiência: %f", num_total_ponderado))
    print(sprintf("Indicador 4 (ponderado): %f", indicador_4_ponderado))

    print(sprintf("População de 4 a 17 anos com deficiência que frequenta a escola: %f", num_escola))
    print(sprintf("População de 4 a 17 anos com deficiência: %f", num_total))
    print(sprintf("Indicador 4: %f", indicador_4))
  }

  if (peso)
    return(indicador_4_ponderado)
  else
    return(indicador_4)
}
