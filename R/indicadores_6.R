#' Calcula o indicador 6A: "Percentual de escolas públicas com estudantes que permanecem pelo menos 7h em atividades escolares"
#'
#' @param df_matricula DataFrame com dados carregados do Censo Escolar
#' @return Indicador 6A em porcentagem
#' @import dplyr
#' @export
calc_indicador_6A <- function(df_matricula) {
  # selecionar colunas de interesse
  df_cols <- df_matricula %>% select("ID_MATRICULA", "CO_ENTIDADE", "CO_MUNICIPIO", "TP_MEDIACAO_DIDATICO_PEDAGO", "TP_DEPENDENCIA", "TP_ETAPA_ENSINO",
                                     "NU_DURACAO_TURMA", "NU_DUR_ATIV_COMP_MESMA_REDE", "NU_DUR_ATIV_COMP_OUTRAS_REDES", "NU_DUR_AEE_MESMA_REDE", "NU_DUR_AEE_OUTRAS_REDES")

  # filtrar linhas
  df_alvo <- df_cols %>% filter(TP_DEPENDENCIA %in% c(1, 2, 3)) %>%
    filter(TP_ETAPA_ENSINO %in% c(1, 2, 4, 5, 6, 7, 14, 15, 16, 17, 18, 8, 9, 10, 11, 19, 20, 21, 41, 25, 26, 27, 28, 29, 35, 36, 37, 38, 30, 31, 32, 33, 34)) %>%
    filter(NU_DURACAO_TURMA > 0) %>%
    filter(TP_MEDIACAO_DIDATICO_PEDAGO == 1)

  # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
  df_alvo_jornada <- df_alvo %>% mutate(NU_JORNADA = NU_DURACAO_TURMA + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NU_DUR_AEE_MESMA_REDE + NU_DUR_AEE_OUTRAS_REDES) %>%
    mutate(ETI = (NU_JORNADA >= 419))

  # seleciona matriculas de tempo integral
  df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)

  indicador_6A <- (nrow(df_alvo_eti)/nrow(df_alvo))*100

  return(indicador_6A)
}

#' Calcula o indicador 6B: "Percentual de estudantes que permanecem pelo menos 7h em atividades escolares"
#'
#' @param df_matricula DataFrame com dados carregados do Censo Escolar
#' @return Indicador 6B em porcentagem
#' @import dplyr
#' @export
calc_indicador_6B <- function(df_matricula) {
  # selecionar colunas de interesse
  df_cols <- df_matricula %>% select("ID_MATRICULA", "CO_ENTIDADE", "CO_MUNICIPIO", "TP_MEDIACAO_DIDATICO_PEDAGO", "TP_DEPENDENCIA", "TP_ETAPA_ENSINO",
                                     "NU_DURACAO_TURMA", "NU_DUR_ATIV_COMP_MESMA_REDE", "NU_DUR_ATIV_COMP_OUTRAS_REDES", "NU_DUR_AEE_MESMA_REDE", "NU_DUR_AEE_OUTRAS_REDES")

  # filtrar linhas
  df_alvo <- df_cols %>% filter(TP_DEPENDENCIA %in% c(1, 2, 3)) %>%
    filter(TP_ETAPA_ENSINO %in% c(1, 2, 4, 5, 6, 7, 14, 15, 16, 17, 18, 8, 9, 10, 11, 19, 20, 21, 41, 25, 26, 27, 28, 29, 35, 36, 37, 38, 30, 31, 32, 33, 34)) %>%
    filter(NU_DURACAO_TURMA > 0) %>%
    filter(TP_MEDIACAO_DIDATICO_PEDAGO == 1)

  # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
  df_alvo_jornada <- df_alvo %>% mutate(NU_JORNADA = NU_DURACAO_TURMA + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NU_DUR_AEE_MESMA_REDE + NU_DUR_AEE_OUTRAS_REDES) %>%
    mutate(ETI = (NU_JORNADA >= 419))

  # seleciona matriculas de tempo integral
  df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)

  num_total_escolas <- df_matricula %>% count(CO_ENTIDADE) %>% nrow
  num_eti_escolas <- df_alvo_eti %>% count(CO_ENTIDADE) %>% nrow
  indicador_6B <- (num_eti_escolas/num_total_escolas)*100

  return(indicador_6B)
}
