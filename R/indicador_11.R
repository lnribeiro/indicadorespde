require(dplyr)

############# Ler csvs #############
data_path <- path.expand("C:/Users/lnr46/Documents/PNAD-Analysis/pde_indices/dados")
df_matricula <- read.csv2(paste0(data_path, "/censo-2018-matricula.csv"), header = TRUE, sep = ";")
df_escola <- read.csv2(paste0(data_path, "/censo-2018-escola.csv"), header = TRUE, sep = ";")

############# Indicador 11A #############

# selecionar colunas de interesse
df_cols <- df_matricula %>% select("TP_ETAPA_ENSINO")

# filtrar linhas
df_tec <- df_cols %>% filter(TP_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 74, 63))

indicador_11A <- nrow(df_tec)
print(paste("Indicador 11A:", indicador_11A))