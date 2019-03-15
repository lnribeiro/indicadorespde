require(dplyr)

############# Ler csvs #############
# data_path <- path.expand("~/lab/pde_indices/dados")
data_path <- path.expand("C:/Users/lnr46/Documents/PNAD-Analysis/pde_indices/dados")
df <- read.csv2(paste0(data_path, "/pnadc-2016-1q.csv"), header = TRUE, sep = ",", colClasses=c("V1028"="character"))

######## Indicador 1A ########
df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V1023, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
  filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 == 4 | V2009 == 5) %>%
  filter(!is.na(V3002))

df_criancas_escola <- df_criancas %>% filter(V3002 == 1)

total_criancas_4_5_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
total_criancas_4_5_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
indicador_1A <- total_criancas_4_5_anos_escola_ponderado/total_criancas_4_5_anos_ponderado
print(indicador_1A)

# tot_idade_escola <- nrow(df_criancas_escola)
# tot_idade        <- nrow(df_criancas)
# print(tot_idade_escola/tot_idade)

######## Indicador 1B ########

df_criancas <- df %>% select(Ano, UF, Capital, RM_RIDE, V20082,V1023, V3003, V2007, V2009, V3002, V1028, Estrato, UPA) %>%
  filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 >= 0 & V2009 <= 3) %>%
  filter(!is.na(V3002))

df_criancas_escola <- df_criancas %>% filter(V3002 == 1)
total_criancas_0_3_anos_escola_ponderado <- sum(as.numeric(df_criancas_escola$V1028))
total_criancas_0_3_anos_ponderado <- sum(as.numeric(df_criancas$V1028))
indicador_1B <- total_criancas_0_3_anos_escola_ponderado/total_criancas_0_3_anos_ponderado
print(indicador_1B)






