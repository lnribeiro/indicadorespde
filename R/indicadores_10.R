#' Calcula o indicador 10: "Percentual de matrículas de educação de jovens e adultos na forma integrada à educação profissional"
#'
#' @import dplyr
#' @export
calc_indicador_10 <- function(df_matricula, ano, verbose = TRUE) {
  if (ano == 2014) {
    # filtrar linhas
    df_eja <- df_matricula %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(ID_ETAPA_AGREGADA_MAT %in% c(6, 7)) # IN_EJA?

    # contagem dos tipos de EJA
    count_eja <- df_eja %>% count(FK_COD_ETAPA_ENSINO)

    # contagem das matrículas integradas ao ensino profissional
    etapas_integradas <- c(60, 61, 62, 63, 65)
    num_matriculas_integradas <- 0
    for (etapa in etapas_integradas) {
      count_eja_integrado <-  count_eja %>% filter(FK_COD_ETAPA_ENSINO == etapa)
      if (length(count_eja_integrado$n) == 0) next
      num_matriculas_integradas <- num_matriculas_integradas + count_eja_integrado$n
    }
  } else if (ano >= 2015) {
    # filtrar linhas
    df_eja <- df_matricula %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(IN_EJA == 1)

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
  } else {
    error("Período não suportado.")
  }

  num_matriculas_eja <- nrow(df_eja)
  indicador_10 <- (num_matriculas_integradas/num_matriculas_eja)*100

  if (verbose == TRUE) {
    print(sprintf("Número de matrículas EJA integrada: %f", num_matriculas_integradas))
    print(sprintf("Número de matrículas EJA: %f", num_matriculas_eja))
    print(sprintf("Indicador 10: %f", indicador_10))
  }

  return(indicador_10)
}
