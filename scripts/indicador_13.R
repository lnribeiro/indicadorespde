require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")
ano <- "2016"
df_docentes <- read.csv2(paste0(data_path, "/censo-superior-", ano, "-docente.csv"), header = TRUE, sep = "|")
df_ies <- read.csv2(paste0(data_path, "/censo-superior-", ano, "-ies.csv"), header = TRUE, sep = "|")


############ filtrar para Recife (2611606) #############

# descobrir IES em Recife
df_ies_recife <- df_ies %>% filter(CO_MUNICIPIO_IES == 2611606)
CO_IES_recife <- df_ies_recife$CO_IES

# filtrar docentes em IES no Recife e seleciona colunas de interesse
df_docentes_recife <- df_docentes %>% filter(CO_IES %in% CO_IES_recife) %>%
                                      select("CO_DOCENTE", "CO_SITUACAO_DOCENTE", "CO_ESCOLARIDADE_DOCENTE")

############# Indicador 13A #############

# seleciona docentes ativos e remove duplicatas
df_alvo <- df_docentes_recife %>% filter(CO_SITUACAO_DOCENTE == 1) %>%
                                  distinct(CO_DOCENTE, .keep_all = TRUE)

# calcular a qt de docentes com mestrado ou doutorado
df_mest_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 4 | CO_ESCOLARIDADE_DOCENTE == 5)

num_mest_dout <- nrow(df_mest_dout)
num_total <- nrow(df_alvo)

indicador_13A <- (num_mest_dout/num_total)*100
print(indicador_13A)

############# Indicador 13B #############

df_dout <- df_alvo %>% filter(CO_ESCOLARIDADE_DOCENTE == 5)
num_dout <- nrow(df_dout)

indicador_13B <- (num_dout/num_total)*100
print(indicador_13B)