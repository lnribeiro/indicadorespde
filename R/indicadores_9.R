calc_indicador_9A <- function(df) {
  df_15 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001) %>% filter(V2009 >= 15) %>% filter(!is.na(V3001))
  df_15_le <- df_15 %>% filter(V3001 == 1)

  num <- sum(as.numeric(df_15_le$V1028))
  tot <- sum(as.numeric(df_15$V1028))
  indicador_9A <- num/tot

  return(indicador_9A)
}

calc_indicador_9B <- function(df) {
  df_15_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001, VD3002) %>% filter(V2009 >= 15) %>% filter(!is.na(V3001))
  df_15_nao_le_b <- df_15_b %>% filter(V3001 == 2) %>% filter(VD3002 < 4)

  num_b <- sum(as.numeric(df_15_nao_le_b$V1028))
  tot_b <- sum(as.numeric(df_15_b$V1028))
  indicador_9B <- num_b/tot_b

  return(indicador_9B)
}
