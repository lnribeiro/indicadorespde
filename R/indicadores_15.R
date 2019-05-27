# Disciplinas a serem investigadas
cols_discpl <- c("IN_DISC_QUIMICA",
                 "IN_DISC_FISICA",
                 "IN_DISC_MATEMATICA",
                 "IN_DISC_BIOLOGIA",
                 "IN_DISC_CIENCIAS",
                 "IN_DISC_LINGUA_PORTUGUESA",
                 "IN_DISC_LINGUA_INGLES",
                 "IN_DISC_LINGUA_ESPANHOL",
                 "IN_DISC_LINGUA_FRANCES",
                 "IN_DISC_LINGUA_OUTRA",
                 "IN_DISC_ARTES",
                 "IN_DISC_EDUCACAO_FISICA",
                 "IN_DISC_HISTORIA",
                 "IN_DISC_GEOGRAFIA",
                 "IN_DISC_FILOSOFIA",
                 "IN_DISC_ENSINO_RELIGIOSO",
                 "IN_DISC_ESTUDOS_SOCIAIS",
                 "IN_DISC_SOCIOLOGIA",
                 "IN_DISC_EST_SOCIAIS_SOCIOLOGIA")

# Colunas a serem selecionadas
cols_select <- c(cols_discpl, "CO_CURSO_1", "CO_CURSO_2", "CO_CURSO_3",
                 "TP_SITUACAO_CURSO_1", "TP_SITUACAO_CURSO_2", "TP_SITUACAO_CURSO_3")

#' Verifica se a disciplina lecionada por um docente e a sua formação são coerentes
#'
#' @param disc Disciplina lecionada por um docente
#' @param codcurso Código do curso da formação do docente (ver Anexo 5 dos microdados do censo escolar)
#' @return TRUE se a disciplina e o código forem coerente; FALSE caso contrário
#' @import dplyr
checa_coerencia_disciplina_curso <- function(disc, codcurso) {
  if (disc == "PORTUGUES" & codcurso %in% c("145F15", "145F17", "223L01", "220L03")) return(TRUE)
  else if (disc == "ESTRANGEIRA" & codcurso %in% c("145F14", "145F17", "222L01", "220L03")) return(TRUE)
  else if (disc == "ARTE" & codcurso %in% c("146F02", "146F04", "146F07", "146F20", "146F22", "210A01", "211A02", "212D01", "212M02", "212T01")) return(TRUE)
  else if (disc == "EDUCACAO_FISICA" & codcurso %in% c("146F15", "720E01")) return(TRUE)
  else if (disc == "MATEMATICA" & codcurso %in% c("145F18", "461M01")) return(TRUE)
  else if (disc == "CIENCIAS" & codcurso %in% c("145F01", "145F02", "145F09", "145F21", "442Q01", "441F01", "421C01", "440C01")) return(TRUE)
  else if (disc == "QUIMICA" & codcurso %in% c("145F02", "145F21", "442Q01")) return(TRUE)
  else if (disc == "FISICA" & codcurso %in% c("145F02", "145F09", "441F01")) return(TRUE)
  else if (disc == "BIOLOGIA" & codcurso %in% c("145F01", "145F02", "421C01")) return(TRUE)
  else if (disc == "ESTUDOS_SOCIAIS" & codcurso %in% c("144F12", "145F10", "145F11", "145F24", "310C02", "312A01", "220H01", "225H01", "443G05")) return(TRUE)
  else if (disc == "HISTORIA" & codcurso %in% c("145F11", "225H01")) return(TRUE)
  else if (disc == "GEOGRAFIA" & codcurso %in% c("145F10", "443G05")) return(TRUE)
  else if (disc == "SOCIOLOGIA" & codcurso %in% c("145F24", "310C02", "312A01")) return(TRUE)
  else if (disc == "FILOSOFIA" & codcurso %in% c("145F08", "226F01")) return(TRUE)
  else if (disc == "ENSINO_RELIGIOSO" & codcurso %in% c("145F05", "221T01")) return(TRUE)
  else return(FALSE)
}

#' Calcula o indicador 15B: "Percentual de docentes e de profissionais de apoio à docência da educação básica com formação específica de nível superior, obtida em curso de licenciatura na área de conhecimento em que atuam"
#'
#' @param df_docente DataFrame com dados carregados da tabela "docentes" do Censo da Educação Básica
#' @param verbose exibe informações no console se True
#' @return Indicador 15B em porcentagem
#' @import dplyr
#' @export
calc_indicador_15B <- function(df_docente, verbose = TRUE) {
  # seleciona apenas docentes de disciplinas de interesse (de acordo com a norma técnica da Meta 15)
  df_filter <- df_docente %>% filter(IN_DISC_QUIMICA == 1 | IN_DISC_FISICA == 1 | IN_DISC_MATEMATICA == 1 | IN_DISC_BIOLOGIA == 1 | IN_DISC_CIENCIAS == 1 | IN_DISC_LINGUA_PORTUGUESA == 1 | IN_DISC_LINGUA_INGLES == 1 | IN_DISC_LINGUA_ESPANHOL == 1 | IN_DISC_LINGUA_FRANCES == 1 | IN_DISC_LINGUA_INDIGENA == 1 | IN_DISC_ARTES == 1 | IN_DISC_EDUCACAO_FISICA == 1 | IN_DISC_HISTORIA == 1 | IN_DISC_GEOGRAFIA == 1 | IN_DISC_FILOSOFIA == 1 | IN_DISC_ENSINO_RELIGIOSO == 1 | IN_DISC_ESTUDOS_SOCIAIS == 1 | IN_DISC_SOCIOLOGIA == 1 | IN_DISC_EST_SOCIAIS_SOCIOLOGIA == 1 )

  # seleciona colunas de interesse e corrige a classe das colunas CO_CURSO_1 2 e 3
  df_filter_select <- df_filter %>% select(cols_select) %>%
    mutate(CO_CURSO_1 = as.character(CO_CURSO_1)) %>%
    mutate(CO_CURSO_2 = as.character(CO_CURSO_2)) %>%
    mutate(CO_CURSO_3 = as.character(CO_CURSO_3))

  # substitui NA por 0 nas colunas de disciplina
  dfna <- df_filter_select
  dfna[cols_discpl][is.na(dfna[cols_discpl])] <- 0

  # cria coluna com qtd. de disciplinas lecionadas
  df_alvo <- dfna %>% mutate(sum_disc = IN_DISC_QUIMICA + IN_DISC_FISICA + IN_DISC_MATEMATICA + IN_DISC_BIOLOGIA + IN_DISC_CIENCIAS + IN_DISC_LINGUA_PORTUGUESA + IN_DISC_LINGUA_INGLES + IN_DISC_LINGUA_ESPANHOL + IN_DISC_LINGUA_FRANCES + IN_DISC_LINGUA_OUTRA + IN_DISC_ARTES + IN_DISC_EDUCACAO_FISICA + IN_DISC_HISTORIA + IN_DISC_GEOGRAFIA + IN_DISC_FILOSOFIA + IN_DISC_ENSINO_RELIGIOSO + IN_DISC_ESTUDOS_SOCIAIS + IN_DISC_SOCIOLOGIA + IN_DISC_EST_SOCIAIS_SOCIOLOGIA)

  # Seleciona apenas os professores que lecionam 1 disciplina.
  df_alvo_1_disc <- df_alvo %>% filter(sum_disc == 1)

  # Verifica se a disciplinas lecionada são coerentes com uma das formações. Também verifica-se se a formação foi concluída
  df_disc <- df_alvo_1_disc %>% mutate(disc = case_when(IN_DISC_QUIMICA == 1 ~ "QUIMICA",
                                                        IN_DISC_FISICA == 1 ~ "FISICA",
                                                        IN_DISC_MATEMATICA == 1 ~ "MATEMATICA",
                                                        IN_DISC_BIOLOGIA == 1 ~ "BIOLOGIA",
                                                        IN_DISC_CIENCIAS == 1 ~ "CIENCIAS",
                                                        IN_DISC_LINGUA_PORTUGUESA == 1 ~ "PORTUGUES",
                                                        IN_DISC_LINGUA_INGLES == 1 ~ "ESTRANGEIRA",
                                                        IN_DISC_LINGUA_ESPANHOL == 1 ~ "ESTRANGEIRA",
                                                        IN_DISC_LINGUA_FRANCES == 1 ~ "ESTRANGEIRA",
                                                        IN_DISC_LINGUA_OUTRA == 1 ~ "ESTRANGEIRA",
                                                        IN_DISC_ARTES == 1 ~ "ARTES",
                                                        IN_DISC_EDUCACAO_FISICA == 1 ~ "EDUCACAO_FISICA",
                                                        IN_DISC_HISTORIA == 1 ~ "HISTORIA",
                                                        IN_DISC_GEOGRAFIA == 1 ~ "GEOGRAFIA",
                                                        IN_DISC_SOCIOLOGIA == 1 ~ "SOCIOLOGIA",
                                                        IN_DISC_FILOSOFIA == 1 ~ "FILOSOFIA",
                                                        IN_DISC_ENSINO_RELIGIOSO == 1 ~ "ENSINO_RELIGIOSO",
                                                        IN_DISC_ESTUDOS_SOCIAIS == 1 ~ "ESTUDOS_SOCIAIS",
                                                        IN_DISC_EST_SOCIAIS_SOCIOLOGIA == 1 ~ "ESTUDOS_SOCIAIS")
  ) %>%
    rowwise %>%
    mutate(curso1_coerente = checa_coerencia_disciplina_curso(disc, CO_CURSO_1)) %>%
    mutate(curso2_coerente = checa_coerencia_disciplina_curso(disc, CO_CURSO_2)) %>%
    mutate(curso3_coerente = checa_coerencia_disciplina_curso(disc, CO_CURSO_3)) %>%
    ungroup %>%
    mutate(formacao_coerente = (curso1_coerente == TRUE) | (curso2_coerente == TRUE) | (curso3_coerente == TRUE) ) %>%
    mutate(formacao_coerente_e_finalizada = (curso1_coerente == TRUE & TP_SITUACAO_CURSO_1 == 1) | (curso2_coerente == TRUE & TP_SITUACAO_CURSO_2 == 1) | (curso3_coerente == TRUE & TP_SITUACAO_CURSO_3 == 1) )

  # cálculo indicador 15B
  num_profs_alvo <- nrow(df_alvo)
  num_profs_alvo_coerente <- nrow(df_disc %>% filter(formacao_coerente == TRUE))
  num_profs_alvo_coerente_e_finalizada <- nrow(df_disc %>% filter(formacao_coerente_e_finalizada == TRUE))
  indicador_15B <- 100*num_profs_alvo_coerente_e_finalizada/num_profs_alvo

  if (verbose == TRUE) {
    print(sprintf("número docentes que lecionam apenas 1 disciplina e possuem formacao adequada e finalizada: %f", num_profs_alvo_coerente_e_finalizada))
    print(sprintf("número de docentes total: %f", num_profs_alvo))
    print(sprintf("Indicador 15B: %f", indicador_15B))
  }

  return(indicador_15B)
}
