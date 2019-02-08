require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")

ano <- "2016"
trimestre <- "1q"
df <- read.csv2(paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv"), header = TRUE, sep = ",", colClasses=c("V1028"="character"))

######## Indicador 3A ########

# criação das variáveis dummy
df_dummies <- df %>% select(Ano, RM_RIDE, V1023, UF, V2007, V20081, V20082, V2009, V2010, V3002, V3003A, V3008, V3009A, V3014, V1028, V1022) %>%
  mutate(idade_cne = V2009) %>%
  mutate(estuda = (V3002 == TRUE)) %>%
  # mutate(EM_regular = (V3003A == 6)) %>%
  # mutate(EM_EJA = (V3003A == 7)) %>%
  mutate(EM_concl =  ( (  (V3003A == 8)  | (V3003A == 9)    | (V3003A == 10) |
                            (V3003A == 11) |  (V3009A == 12) | (V3009A == 13) | (V3009A == 14) | (V3009A == 15) ) |
                         ((V3009A == 8 & V3014 == 1) | (V3009A == 9 & V3014 == 1) | (V3009A == 10 & V3014 == 1) | (V3009A == 11 & V3014 == 1) | (V3009A == 12 & V3014 == 1)) ) ) %>%
  mutate(V3A = ( estuda | EM_concl))

df_dummies_15_17 <- df_dummies %>% filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 >= 15 & V2009 <= 17)

df_dummies_15_17_term <- df_dummies_15_17 %>% filter(V3A == TRUE)

tot <- sum(as.numeric(df_dummies_15_17$V1028))
num <- sum(as.numeric(df_dummies_15_17_term$V1028))

indicador_3A <- num/tot
print(indicador_3A)

######### Indicador 3B ############

# criação das variáveis dummy
df_dummies_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2007, V20081, V20082, V2009, V2010, V3002, V3003A, V3008, V3009A, V3014, V1028, V1022) %>%
  mutate(idade_cne = V2009) %>%
  mutate(EM_regular = (V3003A == 6)) %>%
  mutate(EM_EJA = (V3003A == 7)) %>%
  mutate(EF_concl =  ( (  (V3003A == 6)  | (V3003A == 7)  | (V3003A == 8)  | (V3003A == 9)    | (V3003A == 10) | 
                            (V3003A == 11) | (V3009A == 6)  | (V3009A == 9)  | (V3009A == 10)   | (V3009A == 11) |
                            (V3009A == 12) | (V3009A == 13) | (V3009A == 14) | (V3009A == 15) ) | ((V3009A == 7 & V3014 == 1) | (V3009A == 8 & V3014 == 1)) ) ) %>%
  mutate(V3A = (EM_regular|EM_EJA|EF_concl))


df_dummies_15_17_b <- df_dummies_b %>% filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(V2009 >= 15 & V2009 <= 17)

df_dummies_15_17_term_b <- df_dummies_15_17_b %>% filter(V3A == TRUE)

tot <- sum(as.numeric(df_dummies_15_17_b$V1028))
num <- sum(as.numeric(df_dummies_15_17_term_b$V1028))

indicador_3B <- num/tot
print(indicador_3B)
