require(dplyr)

############# Ler csvs #############
data_path <- "./dados"
ano <- "2016"
df_matricula <- read.csv2(paste0(data_path, "/censo-", ano, "-matricula.csv"), header = TRUE, sep = ";")

############# Indicador 10 #############

# selecionar colunas de interesse
cols_int <- c("IN_EJA", "TP_ETAPA_ENSINO")
df_cols <- df_matricula[cols_int]

# filtrar linhas
df_eja <- df_cols %>% filter(IN_EJA == 1)

# contagem dos tipos de EJA
count_eja <- df_eja %>% count(TP_ETAPA_ENSINO)

# contagem das matrículas integradas ao ensino profissional
etapas_integradas <- c(65, 67, 73, 74)
num_matriculas_integradas <- 0
for (etapa in etapas_integradas) {
  count_eja_integrado <-  count_eja %>% filter(TP_ETAPA_ENSINO == etapa)
  if (length(count_eja_integrado$n) == 0) next
  num_matriculas_integradas <- num_matriculas_integradas + count_eja_integrado$n
}

num_matriculas_eja <- nrow(df_eja)

indicador_10 <- (num_matriculas_integradas/num_matriculas_eja)*100
print(paste("Ano:", ano))
print(paste("Indicador 10:", indicador_10))
