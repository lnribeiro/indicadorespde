#' Calcula o indicador 16: "Percentual de professores da educação básica com pós-graduação lato sensu ou stricto senso"
#'
#' @param df_docentes DataFrame com dados carregados da tabela "docentes" do Censo Escolar
#' @param verbose exibe informações no console se True
#' @return Indicador 16 em porcentagem
#' @import dplyr
#' @export
calc_indicador_16 <- function(df_docentes, ano, verbose = TRUE) {
  if (ano == 2014) {
    df_total <- df_docentes %>%
      filter(FK_COD_MUNICIPIO == 2611606) %>%
      filter(ID_TIPO_DOCENTE == 1)

    df_pos <- df_total %>% filter(ID_ESPECIALIZACAO == 1 | ID_MESTRADO == 1 | ID_DOUTORADO == 1)
  } else if (ano >= 2015) {
    df_total <- df_docentes %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(TP_TIPO_DOCENTE == 1)

    df_pos <- df_total %>% filter(IN_ESPECIALIZACAO == 1 | IN_MESTRADO == 1 | IN_DOUTORADO == 1)
  } else {
    error("Período não suportado.")
  }

  indicador_16 <- (nrow(df_pos)/nrow(df_total))*100

  if (verbose == TRUE) {
    print(sprintf("Número de professores com pós-graduação: %f", nrow(df_pos)))
    print(sprintf("Número total de docentes: %f", nrow(df_total)))
    print(sprintf("Indicador 16: %f", indicador_16))
  }

  return(indicador_16)
}
