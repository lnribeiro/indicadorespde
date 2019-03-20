calc_indicador_1A <- function(df) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V1023, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 == 4 | V2009 == 5) %>%
    filter(!is.na(V3002))

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)

  total_criancas_4_5_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
  total_criancas_4_5_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
  indicador_1A <- total_criancas_4_5_anos_escola_ponderado/total_criancas_4_5_anos_ponderado

  # tot_idade_escola <- nrow(df_criancas_escola)
  # tot_idade        <- nrow(df_criancas)
  # print(tot_idade_escola/tot_idade)

  return(indicador_1A)
}

calc_indicador_1B <- function(df) {
  df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V20082,V1023, V3003, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
    filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 0 & V2009 <= 3) %>%
    filter(!is.na(V3002))

  df_criancas_escola <- df_criancas %>% filter(V3002 == 1)
  total_criancas_0_3_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
  total_criancas_0_3_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
  indicador_1B <- total_criancas_0_3_anos_escola_ponderado/total_criancas_0_3_anos_ponderado

  return(indicador_1B)
}
