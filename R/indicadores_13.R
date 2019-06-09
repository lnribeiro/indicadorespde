#' Calcula o indicador 13A: "Percentual de funções docentes na educação superior com mestrado e doutorado"
#'
#' @param df_docentes DataFrame com dados carregados da tabela "docentes" do Censo da Educação Superior
#' @param df_ies DataFrame com dados carregados da tabela "ies" do Censo da Educação Superior
#' @param ano Ano do censo
#' @param verbose exibe informações no console se True
#' @return Indicador 13A em porcentagem
#' @import dplyr
#' @export
calc_indicador_13A <- function(df_docentes, df_ies, ano, verbose = TRUE) {
  if (ano %in% c(2014, 2015, 2016)) {
    df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO_IES == 2611606)
    CO_IES_recife <- df_ies_recife$CO_IES
    df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>% select("CO_DOCENTE", "CO_SITUACAO_DOCENTE", "CO_ESCOLARIDADE_DOCENTE")
    df_alvo <- df_docentes_recife %>% filter(CO_SITUACAO_DOCENTE == 1) %>% distinct(CO_DOCENTE, .keep_all = TRUE)
    df_mest_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 4 | CO_ESCOLARIDADE_DOCENTE == 5)
  } else if (ano >= 2017) {
    df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO == 2611606)
    CO_IES_recife <- df_ies_recife$CO_IES
    df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>% select("CO_DOCENTE", "TP_SITUACAO", "TP_ESCOLARIDADE")
    df_alvo <- df_docentes_recife %>% filter(TP_SITUACAO == 1) %>% distinct(CO_DOCENTE, .keep_all = TRUE)
    df_mest_dout <- df_alvo %>% filter(TP_ESCOLARIDADE == 4 | TP_ESCOLARIDADE == 5)
  } else {
    stop("Período não suportado.")
  }

  num_mest_dout <- nrow(df_mest_dout)
  num_total <- nrow(df_alvo)
  indicador_13A <- (num_mest_dout/num_total)*100

  if (verbose == TRUE) {
    print(sprintf("Número de mestres e doutores: %f", num_mest_dout))
    print(sprintf("Número total docentes: %f", num_total))
    print(sprintf("Indicador 13A: %f", indicador_13A))
  }

  return(indicador_13A)
}

#' Calcula o indicador 13B: "Percentual de funções docentes na educação superior com doutorado"
#'
#' @param df_docentes DataFrame com dados carregados da tabela "docentes" do Censo da Educação Superior
#' @param df_ies DataFrame com dados carregados da tabela "ies" do Censo da Educação Superior
#' @param ano Ano do censo
#' @param verbose exibe informações no console se True
#' @return Indicador 13B em porcentagem
#' @import dplyr
#' @export
calc_indicador_13B <- function(df_docentes, df_ies, ano, verbose = TRUE) {
  if (ano %in% c(2014, 2015, 2016)) {
    df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO_IES == 2611606)
    CO_IES_recife <- df_ies_recife$CO_IES
    df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>% select("CO_DOCENTE", "CO_SITUACAO_DOCENTE", "CO_ESCOLARIDADE_DOCENTE")
    df_alvo <- df_docentes_recife %>% filter(CO_SITUACAO_DOCENTE == 1) %>% distinct(CO_DOCENTE, .keep_all = TRUE)
    df_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 5)
  } else if (ano == 2017) {
    df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO == 2611606)
    CO_IES_recife <- df_ies_recife$CO_IES
    df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>% select("CO_DOCENTE", "TP_SITUACAO", "TP_ESCOLARIDADE")
    df_alvo <- df_docentes_recife %>% filter(TP_SITUACAO == 1) %>% distinct(CO_DOCENTE, .keep_all = TRUE)
    df_dout <- df_alvo %>% filter(TP_ESCOLARIDADE == 5)
  } else {
    stop("Período não suportado.")
  }

  num_dout <- nrow(df_dout)
  num_total <- nrow(df_alvo)
  indicador_13B <- (num_dout/num_total)*100

  if (verbose == TRUE) {
    print(sprintf("Número de doutores: %f", num_dout))
    print(sprintf("Número total docentes: %f", num_total))
    print(sprintf("Indicador 13B: %f", indicador_13B))
  }

  return(indicador_13B)
}
