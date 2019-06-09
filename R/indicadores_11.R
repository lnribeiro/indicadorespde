#' Calcula o indicador 11A: "Matrículas em educação profissional técnica de nível médio"
#'
#' @import dplyr
#' @export
calc_indicador_11 <- function(df_matricula, ano, verbose = TRUE) {
  if (ano == 2014) {
    indicador_11 <- df_matricula %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(FK_COD_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 39, 40, 62, 63)) %>%
      filter(ID_DEPENDENCIA_ADM_ESC %in% c(1, 2, 3)) %>% # apenas públicas
      nrow
  } else if (ano >= 2015) {
    indicador_11 <- df_matricula %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(TP_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 39, 40, 74)) %>%
      filter(TP_DEPENDENCIA %in% c(1, 2, 3)) %>% # apenas públicas
      nrow
  } else {
    error("Período não suportado.")
  }

  if (verbose == TRUE) print(sprintf("Número de matrículas em educação profissional técnica de nível médio no ensino público (indicador 11): %f", indicador_11))

  return(indicador_11)
}
