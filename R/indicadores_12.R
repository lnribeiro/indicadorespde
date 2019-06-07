# V3003A: qual curso frequenta
# V3009A: qual foi o curso mais elevado frequentado anteriormente?
# V3014: concluiu o curso mencionado anteriormente?

#' Calcula o indicador 12A: "Taxa de escolarização bruta na educação superior da população de 18 a 24 anos"
#'
#' @import dplyr
#' @export
calc_indicador_12A_anual <- function(df_anual, peso = FALSE, verbose = TRUE) {
  df_18_24 <- df_anual %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 18 & V2009 <= 24)

  if (df_anual$Ano[1] == "2014" | df_anual$Ano[1] == "2015") {
    df_18_24 <- df_18_24 %>% filter(!is.na(V3003))
    df_18_24_grad <- df_18_24 %>% filter(V3003 == "07")
  } else {
    df_18_24 <- df_18_24 %>% filter(!is.na(V3003A))
    df_18_24_grad <- df_18_24 %>% filter(V3003A == "08")
  }

  num_ponderado <- sum(df_18_24_grad$V1032)
  tot_ponderado <- sum(df_18_24$V1032)
  indicador_12A_ponderado <- (num_ponderado/tot_ponderado)*100

  num <- nrow(df_18_24_grad)
  tot <- nrow(df_18_24)
  indicador_12A <- (num/tot)*100

  if (verbose) {
    print(sprintf("Número (ponderado) de graduados entre 18 a 24 anos: %f", num_ponderado))
    print(sprintf("Número (ponderado) da população entre 18 a 24 anos: %f", tot_ponderado))
    print(sprintf("Indicador 12A (ponderado): %f", indicador_12A_ponderado))
    print(sprintf("Número de graduados entre 18 a 24 anos: %f", num))
    print(sprintf("Número da população entre 18 a 24 anos: %f", tot))
    print(sprintf("Indicador 12A: %f", indicador_12A))
  }

  if (peso)
    return(indicador_12A_ponderado)
  else
    return(indicador_12A)
}

#' Calcula o indicador 12B: "Taxa de escolarização líquida ajustada na educação superior da população de 18 a 24 anos"
#'
#' @import dplyr
#' @export
calc_indicador_12B_anual <- function(df_anual, peso = FALSE, verbose = TRUE) {
  df_18_24_b <- df_anual %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 18 & V2009 <= 24)

  if (df_anual$Ano[1] == "2014" | df_anual$Ano[1] == "2015") {
    df_18_24_b <- df_18_24_b %>% filter(!is.na(V3003)) %>% filter(!is.na(V3009))
    df_18_24_grad_b <- df_18_24_b %>%
      filter( (V3003 == "07" | V3003 == "08" | V3003 == "09" ) | (V3009 == "11" | V3009 == "12" ) | (V3009 == "11" & V3014 == "1"))
  } else {
    df_18_24_b <- df_18_24_b %>% filter(!is.na(V3003A)) %>% filter(!is.na(V3009A))
    df_18_24_grad_b <- df_18_24_b %>%
      filter( (V3003A == "08" | V3003A == "09" | V3003A == "10" | V3003A == "11") | (V3009A == "13" | V3009A == "14" | V3009A == "15") | (V3009A == "12" & V3014 == "1"))
  }

  num_b_ponderado <- sum(df_18_24_grad_b$V1032)
  tot_b_ponderado <- sum(df_18_24_b$V1032)
  indicador_12B_ponderado <- (num_b_ponderado/tot_b_ponderado)*100

  num_b <- nrow(df_18_24_grad_b)
  tot_b <- nrow(df_18_24_b)
  indicador_12B <- (num_b/tot_b)*100

  if (verbose) {
    print(sprintf("Número (ponderado) de graduados entre 18 a 24 anos (ajustada): %f", num_b_ponderado))
    print(sprintf("Número (ponderado) da população entre 18 a 24 anos: %f", tot_b_ponderado))
    print(sprintf("Indicador 12B (ponderado): %f", indicador_12B_ponderado))
    print(sprintf("Número de graduados entre 18 a 24 anos (ajustada): %f", num_b))
    print(sprintf("Número da população entre 18 a 24 anos: %f", tot_b))
    print(sprintf("Indicador 12B: %f", indicador_12B))
  }

  if (peso)
    return(indicador_12B_ponderado)
  else
    return(indicador_12B)
}
