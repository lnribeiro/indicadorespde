require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")

ano <- "2016"
trimestre <- "1q"
df <- read.csv2(paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv"), header = TRUE, sep = ",", colClasses=c("V1028"="character"))

######## Indicador 9A ########

df_15 <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001) %>% filter(V2009 >= 15) %>% filter(!is.na(V3001))
df_15_le <- df_15 %>% filter(V3001 == 1)

num <- sum(as.numeric(df_15_le$V1028))
tot <- sum(as.numeric(df_15$V1028))

indicador_9A <- num/tot
print(indicador_9A)

######## Indicador 9B ########

df_15_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2009, V1028, V3001, VD3002) %>% filter(V2009 >= 15) %>% filter(!is.na(V3001))
df_15_nao_le_b <- df_15_b %>% filter(V3001 == 2) %>% filter(VD3002 < 4)

num_b <- sum(as.numeric(df_15_nao_le_b$V1028))
tot_b <- sum(as.numeric(df_15_b$V1028))

indicador_9B <- num_b/tot_b
print(indicador_9B)
