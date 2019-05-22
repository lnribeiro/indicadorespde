#' Calcula o indicador 11A: "Matrículas em educação profissional técnica de nível médio"
#'
#' @param df_matricula DataFrame com dados carregados da tabela "matricula" do Censo Escolar
#' @param verbose exibe informações no console se True
#' @return Indicador 11A
#' @import dplyr
#' @export
calc_indicador_11A <- function(df_matricula, verbose = TRUE) {
  df_cols <- df_matricula %>% select("TP_ETAPA_ENSINO")
  df_tec <- df_cols %>% filter(TP_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 74, 63))
  indicador_11A <- nrow(df_tec)

  if (verbose == TRUE) print(sprintf("Número de matrículas em educação profissional técnica de nível médio (indicador 11): %f", indicador_11A))

  return(indicador_11A)
}
