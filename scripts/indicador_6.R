require(dplyr)

############# Ler csvs #############
data_path <- path.expand("C:/Users/lnr46/Documents/PNAD-Analysis/pde_indices/dados")
df_matricula <- read.csv2(paste0(data_path, "/censo-2018-matricula.csv"), header = TRUE, sep = ";")
df_escola <- read.csv2(paste0(data_path, "/censo-2018-escola.csv"), header = TRUE, sep = ";")

############# Indicador 6A #############

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

############# Indicador 6B #############

num_total_escolas <- df_matricula %>% count(CO_ENTIDADE) %>% nrow
num_eti_escolas <- df_alvo_eti %>% count(CO_ENTIDADE) %>% nrow

indicador_6B <- (num_eti_escolas/num_total_escolas)*100

############# Indicador 6B nacional #############

# obtém o código daquelas entidades que possuem pelo menos um aluno do público alvo de ETI
df_escolas_total <- df_alvo_eti %>% count(CO_ENTIDADE)
CO_ENTIDADES_INTERESSE <- df_escolas_total$CO_ENTIDADE 

df_alunos_entidades_interesse <- df_alvo_jornada %>% filter(CO_ENTIDADE %in% CO_ENTIDADES_INTERESSE)

df_escolas_eti <- df_alunos_entidades_interesse %>% select(CO_ENTIDADE, ETI) %>% arrange(CO_ENTIDADE) %>%
                                                    group_by(CO_ENTIDADE) %>% 
                                                    summarise(N_ETI_TRUE = sum(ETI==TRUE), 
                                                              N_ETI_FALSE = sum(ETI==FALSE), 
                                                              N_TOTAL = n(), 
                                                              RATIO = N_ETI_TRUE/N_TOTAL) %>%
                                                    mutate(ESC_ETI = ( RATIO*100 >= 25 ) )

df_num_escolas_eti_true <- df_escolas_eti %>% count(ESC_ETI) %>%
                                              filter(ESC_ETI == TRUE)

num_escolas_eti_total <- nrow(df_escolas_eti)
num_escolas_eti_true  <- df_num_escolas_eti_true$n

indicador_6B_nacional <- (num_escolas_eti_true/num_escolas_eti_total)*100

############# Output #############

print(paste("Indicador 6A: ", indicador_6A))
print(paste("Indicador 6B: ", indicador_6B))
print(paste("Indicador 6B nacional: ", indicador_6B_nacional))


