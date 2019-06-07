#' Calcula o indicador 9A: "Taxa de alfabetização da população de 15 anos ou mais de idade"
#'
#' @import dplyr
#' @export
calc_indicador_9A_anual <- function(df_anual, peso = FALSE, verbose = TRUE) {
  df_15 <- df_anual %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "") # sabe ler / escrever?

  df_15_le <- df_15 %>% filter(V3001 == 1)

  num_ponderado <- sum(df_15_le$V1032)
  tot_ponderado <- sum(df_15$V1032)
  indicador_9A_ponderado <- (num_ponderado/tot_ponderado)*100

  num <- nrow(df_15_le)
  tot <- nrow(df_15)
  indicador_9A <- (num/tot)*100

  if (verbose) {
    print(sprintf("Total da população de 15 ou mais anos que lê (ponderado): %f", num_ponderado))
    print(sprintf("Total da população de 15 ou mais anos (ponderado): %f", tot_ponderado))
    print(sprintf("Indicador 9A (ponderado): %f", indicador_9A_ponderado))
    print(sprintf("Total da população de 15 ou mais anos que lê: %f", num))
    print(sprintf("Total da população de 15 ou mais anos: %f", tot))
    print(sprintf("Indicador 9A: %f", indicador_9A))
  }

  if (peso)
    return(indicador_9A_ponderado)
  else
    return(indicador_9A)
}

#' Calcula o indicador 9B: "Taxa de analfabetismo funcional da população de 15 anos ou mais de idade"
#'
#' @import dplyr
#' @export
calc_indicador_9B_anual <- function(df_anual, peso = FALSE, verbose = TRUE) {
  # !!! agora VD3002 é VD3005 !!!
  df_15_b <- df_anual %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 15) %>%
    filter(V3001 != "")

  df_15_nao_le_b <- df_15_b %>% filter(V3001 == 2 | as.numeric(VD3005) < 4)

  num_b_ponderado <- sum(df_15_nao_le_b$V1032)
  tot_b_ponderado <- sum(df_15_b$V1032)
  indicador_9B_ponderado <- (num_b_ponderado/tot_b_ponderado)*100

  num_b <- nrow(df_15_nao_le_b)
  tot_b <- nrow(df_15_b)
  indicador_9B <- (num_b/tot_b)*100

  if (verbose) {
    print(sprintf("Total da população de 15 ou mais anos que NÃO lê (ponderado): %f", num_b_ponderado))
    print(sprintf("Total da população de 15 ou mais anos (ponderado): %f", tot_b_ponderado))
    print(sprintf("Indicador 9B (ponderado): %f", indicador_9B_ponderado))
    print(sprintf("Total da população de 15 ou mais anos que NÃO lê: %f", num_b))
    print(sprintf("Total da população de 15 ou mais anos: %f", tot_b))
    print(sprintf("Indicador 9B: %f", indicador_9B))
  }

  if (peso)
    return(indicador_9B_ponderado)
  else
    return(indicador_9B)
}
