require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")

ano <- "2016"
trimestre <- "1q"
df <- read.csv2(paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv"), header = TRUE, sep = ",", colClasses=c("V1028"="character"))

######## Indicador 2A ########

# criação das variáveis dummy
df_dummies <- df %>% select(Ano, RM_RIDE, V1023, UF, V2007, V20081, V20082, V2009, V2010, V3002, V3003A, V3008, V3009A, V3014, V1028, V1022) %>%
  mutate(idade_cne = V2009) %>%
  mutate(EF_regular = (V3003A == 4)) %>%
  mutate(EF_EJA = (V3003A == 5)) %>%
  mutate(EF_concl =  ( (  (V3003A == 6)  | (V3003A == 7)  | (V3003A == 8)  | (V3003A == 9)    | (V3003A == 10) | 
                          (V3003A == 11) | (V3009A == 6)  | (V3009A == 9)  | (V3009A == 10)   | (V3009A == 11) |
                          (V3009A == 12) | (V3009A == 13) | (V3009A == 14) | (V3009A == 15) ) | ((V3009A == 7 & V3014 == 1) | (V3009A == 8 & V3014 == 1)) ) ) %>%
  mutate(V2A = (EF_EJA | EF_regular | EF_concl))


df_dummies_6_14 <- df_dummies %>% filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 >= 6 & V2009 <= 14)

df_dummies_6_14_term <- df_dummies_6_14 %>% filter(V2A == TRUE)

tot <- sum(as.numeric(df_dummies_6_14$V1028))
num <- sum(as.numeric(df_dummies_6_14_term$V1028))

indicador_2A <- num/tot

print(indicador_2A)

######## Indicador 2B ############

df_dummies_16 <- df_dummies %>% filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 == 16)

df_dummies_16_term <- df_dummies_16 %>% filter(V2A == TRUE)

