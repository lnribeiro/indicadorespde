calc_indicador_10 <- function(df_matricula, df_escola) {
  # selecionar colunas de interesse
  df_cols <- df_matricula %>% select("IN_EJA", "TP_ETAPA_ENSINO")

  # filtrar linhas
  df_eja <- df_cols %>% filter(IN_EJA == 1)

  # contagem dos tipos de EJA
  count_eja <- df_eja %>% count(TP_ETAPA_ENSINO)

  # contagem das matrículas integradas ao ensino profissional
  etapas_integradas <- c(65, 67, 73, 74)
  num_matriculas_integradas <- 0
  for (etapa in etapas_integradas) {
    count_eja_integrado <-  count_eja %>% filter(TP_ETAPA_ENSINO == etapa)
    if (length(count_eja_integrado$n) == 0) next
    num_matriculas_integradas <- num_matriculas_integradas + count_eja_integrado$n
  }

  num_matriculas_eja <- nrow(df_eja)
  indicador_10 <- (num_matriculas_integradas/num_matriculas_eja)*100

  return(indicador_10)
}
