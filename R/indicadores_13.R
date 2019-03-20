# require(dplyr)
#
# ############# Ler csvs #############
# data_path <- path.expand("C:/Users/lnr46/Documents/PNAD-Analysis/pde_indices/dados")
#
# df_docentes <- read.csv2(paste0(data_path, "/censo-superior-2017-docente.csv"), header = TRUE, sep = ",")
# df_ies <- read.csv2(paste0(data_path, "/censo-superior-2017-ies.csv"), header = TRUE, sep = ",")
#
# ############ filtrar para Recife (2611606) #############
#
# # descobrir IES em Recife
# # df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO_IES == 2611606) # 2016
# df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO == 2611606) # 2017
# CO_IES_recife <- df_ies_recife$CO_IES
#
# # filtrar docentes em IES no Recife e seleciona colunas de interesse
#
# # 2016
# # df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>%
# #                                       select("CO_DOCENTE", "CO_SITUACAO_DOCENTE", "CO_ESCOLARIDADE_DOCENTE")
#
# # 2017
# df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>%
#                                       select("CO_DOCENTE", "TP_SITUACAO", "TP_ESCOLARIDADE")
#
# ############# Indicador 13A #############
#
# # seleciona docentes ativos e remove duplicatas
#
# # 2016
# # df_alvo <- df_docentes_recife %>% filter(CO_SITUACAO_DOCENTE == 1) %>%
# #                                   distinct(CO_DOCENTE, .keep_all = TRUE)
#
# # 2017
# df_alvo <- df_docentes_recife %>% filter(TP_SITUACAO == 1) %>%
#                                   distinct(CO_DOCENTE, .keep_all = TRUE)
#
# # calcular a qt de docentes com mestrado ou doutorado
#
# # 2016
# # df_mest_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 4 | CO_ESCOLARIDADE_DOCENTE == 5)
#
# # 2017
# df_mest_dout <- df_alvo %>% filter(TP_ESCOLARIDADE == 4 | TP_ESCOLARIDADE == 5)
#
# num_mest_dout <- nrow(df_mest_dout)
# num_total <- nrow(df_alvo)
#
# indicador_13A <- (num_mest_dout/num_total)*100
# print(indicador_13A)
#
# ############# Indicador 13B #############
#
# # 2016
# # df_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 5)
#
# # 2017
# df_dout <- df_alvo %>% filter(TP_ESCOLARIDADE == 5)
#
# num_dout <- nrow(df_dout)
#
# indicador_13B <- (num_dout/num_total)*100
# print(indicador_13B)
