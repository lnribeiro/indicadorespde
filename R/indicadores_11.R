#' Calcula o indicador 11A: "Matrículas em educação profissional técnica de nível médio"
#'
#' @param df_matricula DataFrame com dados carregados da tabela "matricula" do Censo Escolar
#' @return Indicador 11A
#' @import dplyr
#' @export
calc_indicador_11A <- function(df_matricula) {
  df_cols <- df_matricula %>% select("TP_ETAPA_ENSINO")
  df_tec <- df_cols %>% filter(TP_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 74, 63))
  indicador_11A <- nrow(df_tec)
  return(indicador_11A)
}
