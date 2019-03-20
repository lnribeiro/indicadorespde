require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")
df <- read.csv2(paste0(data_path, "/censo-demografico-2010-pessoas.csv"), header = TRUE, sep = ",", colClasses=c("V0010"="character"))

# converte V0010[char] para variável de peso no formato double
df_peso <- df %>% mutate(peso = as.numeric(V0010)/1e13)

df_pop <- df_peso %>% select(V6036, peso, V0613, V0614, V0615, V0616, V0617, V0628, V0633, V0634, V1001, V0001, V0601, V0606, V1004, V6531) %>%
  filter(V1004 == 11) %>% # Recife
  filter(V6036>=4 & V6036 <=17) # Idade

df_alvo <- df_pop %>% filter(V0614 == 1 | V0614 == 2 | V0615 == 1 | V0615 == 2 | V0616 == 1 | V0616 == 2 | V0617 == 1)

df_alvo_escola <- df_alvo %>% filter(V0628 == 1 | V0628 == 2 | V0634 == 1 | V0633 >= 9)

num_escola_ponderado <- sum(df_alvo_escola$peso)
num_total_ponderado <- sum(df_alvo$peso)
# num_escola_ponderado <- nrow(df_alvo_escola)
# num_total_ponderado <- nrow(df_alvo)

indicador_4 <- num_escola_ponderado/num_total_ponderado
print(paste("Indicador 4: ", indicador_4))
  