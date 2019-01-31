require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")
ano <- "2016"
trimestre <- "1q"
df <- read.csv2(paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv"), header = TRUE, sep = ",")

# filtrar para Recife (26) e seleciona variáveis de interesse
df_recife <- df %>% filter(Capital == 26) %>%
                    select(Ano, UF, V2007, V20081, V20082, V2009, V2010, V3002, V1028, V1022)