#' Calcula o indicador 8A: "Escolaridade média da população de 18 a 29 anos"
#'
#' @import dplyr
#' @export
calc_indicador_8A_anual <- function(df_anual, ano, peso = FALSE, verbose = TRUE) {
  if (ano >= 2014) {
    # !!! VD3002 agora é VD3005 !!!
    df_alvo <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1023 == 1) %>%
      filter(V2009 >= 18 & V2009 <= 29) %>%
      filter(VD3005 != "") %>%
      mutate(anos_estudo_ponderado = as.numeric(VD3005)*V1032)

    total_anos_estudo_ponderado <- sum(df_alvo$anos_estudo_ponderado)
    total_anos_populacao_ponderado <- sum(df_alvo$V1032)
    indicador_8A_ponderado <- total_anos_estudo_ponderado/total_anos_populacao_ponderado

    total_anos_estudo <- sum(as.numeric(df_alvo$VD3005))
    total_anos_populacao <- nrow(df_alvo)
    indicador_8A <- total_anos_estudo/total_anos_populacao
  } else {
    stop("Período não suportado.")
  }

  if (verbose) {
    print(sprintf("Total de anos de estudo (ponderado): %f", total_anos_estudo_ponderado))
    print(sprintf("Total da população (ponderada): %f", total_anos_populacao_ponderado))
    print(sprintf("Indicador 8A (ponderado): %f", indicador_8A_ponderado))

    print(sprintf("Total de anos de estudo: %f", total_anos_estudo))
    print(sprintf("Total da população: %f", total_anos_populacao))
    print(sprintf("Indicador 8A: %f", indicador_8A))
  }

  if (peso)
    return(indicador_8A_ponderado)
  else
    return(indicador_8A)
}

#' Calcula o indicador 8B: "Escolaridade média da população de 18 a 29 anos de idade residentes em áreas com habitantes de menor escolaridade"
#'
#' @import dplyr
#' @export
calc_indicador_8B_anual <- function(df_anual, ano, peso = FALSE, verbose = TRUE) {
  if (ano >= 2014) {
    df_alvo <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1022 == 2) %>% # == 2 Rural
      filter(V2009 >= 18 & V2009 <= 29) %>%
      filter(VD3005 != "") %>%
      mutate(anos_estudo_ponderado = as.numeric(VD3005)*V1032)

    total_anos_estudo_ponderado <- sum(df_alvo$anos_estudo_ponderado)
    total_anos_populacao_ponderado <- sum(df_alvo$V1032)
    indicador_8B_ponderado <- total_anos_estudo_ponderado/total_anos_populacao_ponderado

    total_anos_estudo <- sum(as.numeric(df_alvo$VD3005))
    total_anos_populacao <- nrow(df_alvo)
    indicador_8B <- total_anos_estudo/total_anos_populacao
  } else {
    stop("Período não suportado.")
  }

  if (verbose) {
    print(sprintf("Total de anos de estudo (ponderado): %f", total_anos_estudo_ponderado))
    print(sprintf("Total da população (ponderada): %f", total_anos_populacao_ponderado))
    print(sprintf("Indicador 8B (ponderado): %f", indicador_8B_ponderado))

    print(sprintf("Total de anos de estudo: %f", total_anos_estudo))
    print(sprintf("Total da população: %f", total_anos_populacao))
    print(sprintf("Indicador 8B: %f", indicador_8B))
  }

  if (peso)
    return(indicador_8B_ponderado)
  else
    return(indicador_8B)
}

#' Calcula o indicador 8C: "Escolaridade média da população de 18 a 29 anos de idade entre os 25 porcento mais pobres da população recifense"
#'
#' @import dplyr
#' @importFrom stats quantile
#' @export
calc_indicador_8C_anual <- function(df_anual, ano, peso = FALSE, verbose = TRUE) {
  if (ano >= 2014) {
    df_anual_recife <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1023 == 1) %>%
      filter(!is.na(VD4020))

    # calcula primeiro quartil
    quantile_vals <- quantile(df_anual_recife$VD4020)
    primeiro_quartil <- quantile_vals[['25%']]

    # seleciona os 25% mais pobres da população
    df_anual_recife_quartil <- df_anual_recife %>% filter(VD4020 <= primeiro_quartil)

    # filtra para população alvo...
    df_alvo <- df_anual_recife_quartil %>% filter(V2009 >= 18 & V2009 <= 29) %>%
      filter(VD3005 != "") %>%
      mutate(anos_estudo_ponderado = as.numeric(VD3005)*V1032)

    total_anos_estudo_ponderado <- sum(df_alvo$anos_estudo_ponderado)
    total_anos_populacao_ponderado <- sum(df_alvo$V1032)
    indicador_8C_ponderado <- total_anos_estudo_ponderado/total_anos_populacao_ponderado

    total_anos_estudo <- sum(as.numeric(df_alvo$VD3005))
    total_anos_populacao <- nrow(df_alvo)
    indicador_8C <- total_anos_estudo/total_anos_populacao
  } else {
    stop("Período não disponível")
  }

  if (verbose) {
    print(sprintf("Total de anos de estudo (ponderado) do 1o quartil mais pobre de 18 a 29 anos: %f", total_anos_estudo_ponderado))
    print(sprintf("Total da população (ponderado) de 18 a 29 anos: %f", total_anos_populacao_ponderado))
    print(sprintf("Indicador 8C (ponderado): %f", indicador_8C_ponderado))

    print(sprintf("Total de anos de estudo do 1o quartil mais pobre de 18 a 29 anos: %f", total_anos_estudo))
    print(sprintf("Total da população de 18 a 29 anos: %f", total_anos_populacao))
    print(sprintf("Indicador 8C: %f", indicador_8C))
  }

  if (peso)
    return(indicador_8C_ponderado)
  else
    return(indicador_8C)
}

#' Calcula o indicador 8D: "Razão entre a escolaridade média da população negra e da população não negra de 18 a 29 anos"
#'
#' @import dplyr
#' @export
calc_indicador_8D_anual <- function(df_anual, ano, peso = FALSE, verbose = TRUE) {
  if (ano >= 2014) {
    ## pop. preta ##
    df_preta <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1023 == 1) %>%
      filter(V2010 == 2 | V2010 == 4) %>% # preta ou parda
      filter(V2009 >= 18 & V2009 <= 29) %>%
      filter(VD3005 != "") %>%
      mutate(anos_estudo_ponderado = as.numeric(VD3005)*V1032)

    total_anos_estudo_ponderado_preta <- sum(df_preta$anos_estudo_ponderado)
    total_anos_populacao_ponderado_preta <- sum(df_preta$V1032)
    indicador_8D_preta_ponderado <- total_anos_estudo_ponderado_preta/total_anos_populacao_ponderado_preta

    total_anos_estudo_preta <- sum(as.numeric(df_preta$VD3005))
    total_anos_populacao_preta <- nrow(df_preta)
    indicador_8D_preta <- total_anos_estudo_preta/total_anos_populacao_preta

    if (verbose) {
      print("[População preta]")
      print(sprintf("Total de anos de estudo (ponderado): %f", total_anos_estudo_ponderado_preta))
      print(sprintf("Total da população (ponderado): %f", total_anos_populacao_ponderado_preta))
      print(sprintf("Escolaridade média (ponderado): %f ", indicador_8D_preta))
      print(sprintf("Total de anos de estudo: %f", total_anos_estudo_preta))
      print(sprintf("Total da população: %f", total_anos_populacao_preta))
      print(sprintf("Escolaridade média: %f", indicador_8D_preta))
    }

    ## pop. branca ##
    df_branca <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1023 == 1) %>%
      filter(V2010 == 1 | V2010 == 3) %>% # branca ou amarela
      filter(V2009 >= 18 & V2009 <= 29) %>%
      filter(VD3005 != "") %>%
      mutate(anos_estudo_ponderado = as.numeric(VD3005)*V1032)

    total_anos_estudo_ponderado_branca <- sum(df_branca$anos_estudo_ponderado)
    total_anos_populacao_ponderado_branca <- sum(df_branca$V1032)
    indicador_8D_branca_ponderado <- total_anos_estudo_ponderado_branca/total_anos_populacao_ponderado_branca

    total_anos_estudo_branca <- sum(as.numeric(df_branca$VD3005))
    total_anos_populacao_branca <- nrow(df_branca)
    indicador_8D_branca <- total_anos_estudo_branca/total_anos_populacao_branca

    if (verbose) {
      print("[População branca]")
      print(sprintf("Total de anos de estudo (ponderado): %f", total_anos_estudo_ponderado_branca))
      print(sprintf("Total da população (ponderado): %f", total_anos_populacao_ponderado_branca))
      print(sprintf("Escolaridade média (ponderado): %f", indicador_8D_branca_ponderado))
      print(sprintf("Total de anos de estudo: %f", total_anos_estudo_branca))
      print(sprintf("Total da população: %f", total_anos_populacao_branca))
      print(sprintf("Escolaridade média: %f", indicador_8D_branca))
    }

    indicador_8D_ponderado <- (indicador_8D_preta_ponderado/indicador_8D_branca_ponderado)*100
    indicador_8D <- (indicador_8D_preta/indicador_8D_branca)*100
  } else {
    stop("Período não suportado.")
  }

  if (verbose) {
    print(sprintf("Indicador 8D (ponderdo): %f", indicador_8D_ponderado))
    print(sprintf("Indicador 8D: %f", indicador_8D))
  }

  if (peso)
    return(indicador_8D_ponderado)
  else
    return(indicador_8D)
}
