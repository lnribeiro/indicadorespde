#' Calcula o indicador 16: "Percentual de professores da educação básica com pós-graduação lato sensu ou stricto senso"
#'
#' @param df_docentes DataFrame com dados carregados da tabela "docentes" do Censo Escolar
#' @return Indicador 16 em porcentagem
#' @import dplyr
#' @export
calc_indicador_16 <- function(df_docentes) {
  df_total <- df_docentes %>% filter(TP_TIPO_DOCENTE == 1) %>% select("IN_ESPECIALIZACAO", "IN_MESTRADO", "IN_DOUTORADO")
  df_pos <- df_total %>% filter(IN_ESPECIALIZACAO == 1 | IN_MESTRADO == 1 | IN_DOUTORADO == 1)
  indicador_16 <- (nrow(df_pos)/nrow(df_total))*100
  return(indicador_16)
}
