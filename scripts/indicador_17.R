require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")
df <- read.csv2(paste0(data_path, "/pnadc-2018-1q.csv"), header = TRUE, sep = ",", colClasses=c("V1028"="character"))

######## Indicador 17 ########

# (1) Primeiro seleciona pessoas com carga horaria entre 10 e 60
df_select <- df %>% select(Ano, UF, Capital, RM_RIDE, V1023, V2007, V2009, V3002, V1028, V403312, V4039, VD3001, V4014, V4010, V4012,  Estrato, UPA) %>%
  filter(RM_RIDE == 26) %>%
  filter(V1023 == 1) %>%
  filter(!is.na(V403312)) %>%
  mutate(horas_ok = (V4039 >= 10 & V4039 <= 60)) 

# (2) depois seleciona-se professores da rede publica com sup completo
df_prof <- df_select %>% filter(horas_ok == TRUE) %>% 
  filter(VD3001 == 7 & (V4014 == 1 | V4014 == 2 | V4014 == 3) & (V4010 == 2320 | V4010 == 2330 | V4010 == 2341 | V4010 == 2342 | V4010 == 2351 | V4010 == 2352 | V4010 == 2353 | V4010 == 2354 | V4010 == 2355 | V4010 == 2356 | V4010 == 2359 ))

# (3) calcula-se rendimento médio dos professores de interesse
rend_medio_prof <- (sum(df_prof$V403312)/sum(df_prof$V4039)) * 40

# (4) seleciona-se demais profissionais com ensino superior 
df_sup <- df_select %>% filter(horas_ok == TRUE) %>% 
  filter(VD3001 == 7 & (V4012 == 2 | V4012 == 3 | V4012 == 4) & (V4010 != 2310 | V4010 != 2320 | V4010 != 2330 | V4010 != 2341 | V4010 != 2342 | V4010 != 2351 | V4010 != 2352 | V4010 != 2353 | V4010 != 2354 | V4010 != 2355 | V4010 != 2356 | V4010 != 2359 ))

# (5) calcula-se rendimento médio dos demais profissionais
rend_medio_sup <- (sum(df_sup$V403312))/sum(df_sup$V4039) * 40

indicador_17 <- rend_medio_prof/rend_medio_sup

print(paste("Indicador 17", indicador_17))



