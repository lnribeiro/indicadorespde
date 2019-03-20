calc_indicador_12A <- function(df) {
  df_18_24 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3003, V3003A, V3002A) %>% filter(V2009 >= 18 & V2009 <= 24)
  df_18_24_grad <- df_18_24 %>% filter(V3003A == 8)

  num <- sum(as.numeric(df_18_24_grad$V1028))
  tot <- sum(as.numeric(df_18_24$V1028))
  indicador_12A <- num/tot

  return(indicador_12A)
}

calc_indicador_12B <- function(df) {
  df_18_24_b <- df %>% select(Ano, RM_RIDE,  V1023, UF, V2009, V1028, V3003, V3003A, V3009, V3009A, V3014) %>% filter(V2009 >= 18 & V2009 <= 24)
  df_18_24_grad_b <- df_18_24_b %>% filter( (V3003A == 8 | V3003A == 9 | V3003A == 10 | V3003A == 11) | (V3009A == 13 | V3009A == 14 | V3009A == 15) | (V3009 == 12 & V3014 == 1))

  num_b <- sum(as.numeric(df_18_24_grad_b$V1028))
  tot_b <- sum(as.numeric(df_18_24$V1028))
  indicador_12B <- num_b/tot_b

  return(indicador_12B)
}
