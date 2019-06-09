#' Calcula o indicador 6A: "Percentual de escolas públicas com estudantes que permanecem pelo menos 7h em atividades escolares"
#'
#' @import dplyr
#' @export
calc_indicador_6A <- function(df_matricula, ano, verbose = TRUE) {
  if (ano == 2014) {
    # filtrar linhas
    df_alvo <- df_matricula %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NU_DUR_ESCOLARIZACAO > 0) %>%
      # variável TP_MEDIACAO_DIDATICO_PEDAGO não existe em 2014!
      filter(ID_DEPENDENCIA_ADM_ESC %in% c(1, 2, 3))

    # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
    df_alvo_jornada <- df_alvo %>%
      mutate(NU_JORNADA = NU_DUR_ESCOLARIZACAO + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NUM_DUR_AEE_MESMA_REDE + NUM_DUR_AEE_OUTRAS_REDES) %>%
      mutate(ETI = (NU_JORNADA >= 419))

    # seleciona matriculas de tempo integral
    df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)

    num_total_escolas <- df_alvo %>% count(PK_COD_ENTIDADE) %>% nrow
    num_eti_escolas <- df_alvo_eti %>% count(PK_COD_ENTIDADE) %>% nrow
    indicador_6A <- (num_eti_escolas/num_total_escolas)*100
  } else if (ano >= 2015) {
    # filtrar linhas
    df_alvo <- df_matricula %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_DURACAO_TURMA > 0) %>%
      filter(TP_MEDIACAO_DIDATICO_PEDAGO == 1) %>% # Presencial
      filter(TP_DEPENDENCIA %in% c(1, 2, 3))

    # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
    df_alvo_jornada <- df_alvo %>%
      mutate(NU_JORNADA = NU_DURACAO_TURMA + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NU_DUR_AEE_MESMA_REDE + NU_DUR_AEE_OUTRAS_REDES) %>%
      mutate(ETI = (NU_JORNADA >= 419))

    # seleciona matriculas de tempo integral
    df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)

    num_total_escolas <- df_alvo %>% count(CO_ENTIDADE) %>% nrow
    num_eti_escolas <- df_alvo_eti %>% count(CO_ENTIDADE) %>% nrow
    indicador_6A <- (num_eti_escolas/num_total_escolas)*100
  } else {
    stop("Período não suportado.")
  }

  if (verbose == TRUE) {
    print(sprintf("número total escolas públicas com estudantes de 7h em atividades: %f", num_eti_escolas))
    print(sprintf("número total escolas públicas: %f", num_total_escolas))
    print(sprintf("Indicador 6A: %f", indicador_6A))
  }

  return(indicador_6A)
}

#' Calcula o indicador 6B: "Percentual de estudantes que permanecem pelo menos 7h em atividades escolares"
#'
#' @import dplyr
#' @export
calc_indicador_6B <- function(df_matricula, ano, verbose = TRUE) {
  if (ano == 2014) {
    # filtrar linhas
    df_alvo <- df_matricula %>%
      filter(COD_MUNICIPIO_ESCOLA == 2611606) %>%
      filter(NU_DUR_ESCOLARIZACAO > 0)
      # variável TP_MEDIACAO_DIDATICO_PEDAGO não existe em 2014!

    # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
    df_alvo_jornada <- df_alvo %>%
      mutate(NU_JORNADA = NU_DUR_ESCOLARIZACAO + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NUM_DUR_AEE_MESMA_REDE + NUM_DUR_AEE_OUTRAS_REDES) %>%
      mutate(ETI = (NU_JORNADA >= 419))

    # seleciona matriculas de tempo integral
    df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)
  } else if (ano >= 2015) {
    # filtrar linhas
    df_alvo <- df_matricula %>%
      filter(CO_MUNICIPIO == 2611606) %>%
      filter(NU_DURACAO_TURMA > 0) %>%
      filter(TP_MEDIACAO_DIDATICO_PEDAGO == 1) # Presencial

    # calcula jornada média do aluno e determina se a matrícula é tempo integral (7h = 419')
    df_alvo_jornada <- df_alvo %>%
      mutate(NU_JORNADA = NU_DURACAO_TURMA + NU_DUR_ATIV_COMP_MESMA_REDE + NU_DUR_ATIV_COMP_OUTRAS_REDES + NU_DUR_AEE_MESMA_REDE + NU_DUR_AEE_OUTRAS_REDES) %>%
      mutate(ETI = (NU_JORNADA >= 419))

    # seleciona matriculas de tempo integral
    df_alvo_eti <- df_alvo_jornada %>% filter(ETI == TRUE)
  } else {
    stop("Período não suportado.")
  }

  indicador_6B <- (nrow(df_alvo_eti)/nrow(df_alvo))*100

  if (verbose == TRUE) {
    print(sprintf("# total alunos que permanecem pelo menos 7h: %f", nrow(df_alvo_eti)))
    print(sprintf("# total alunos: %f", nrow(df_alvo)))
    print(sprintf("Indicador 6B: %f", indicador_6B))
  }

  return(indicador_6B)
}
