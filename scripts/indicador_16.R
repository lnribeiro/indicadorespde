require(dplyr)

############# Ler csvs #############
data_path <- path.expand("C:/Users/lnr46/Documents/PNAD-Analysis/pde_indices/dados")
df_docentes <- read.csv2(paste0(data_path, "/censo-2018-docente.csv"), header = TRUE, sep = ";")

############# Indicador 16 #############

df_total <- df_docentes %>% filter(TP_TIPO_DOCENTE == 1) %>% select("IN_ESPECIALIZACAO", "IN_MESTRADO", "IN_DOUTORADO")
df_pos <- df_total %>% filter(IN_ESPECIALIZACAO == 1 | IN_MESTRADO == 1 | IN_DOUTORADO == 1)

indicador_16 <- (nrow(df_pos)/nrow(df_total))*100

print(indicador_16)