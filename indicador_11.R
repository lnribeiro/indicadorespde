require(dplyr)

############# Ler csvs #############
data_path <- "./dados"
ano <- "2016"
df_matricula <- read.csv2(paste0(data_path, "/censo-", ano, "-matricula.csv"), header = TRUE, sep = ";")

############# Indicador 11A #############

# selecionar colunas de interesse
df_cols <- df_matricula %>% select("TP_ETAPA_ENSINO")

# filtrar linhas
df_tec <- df_cols %>% filter(TP_ETAPA_ENSINO %in% c(30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 74, 63))

indicador_11A <- nrow(df_tec)
print(paste("Ano:", ano))
print(paste("Indicador 11A:", indicador_11A))