require(dplyr)

############# Ler csvs #############
data_path <- path.expand("~/lab/pde_indices/dados")
df <- read.csv2(paste0(data_path, "/ana_recife_2016.csv"), header = TRUE, sep = ";")


#### Indicador 5A ######
count_lpo <- df %>% count(NIVEL_LPO)
indicador_5A <- (count_lpo[3,]$n + count_lpo[4,]$n)/nrow(df) # suficiente
# indicador_5A <- (count_lpo[1,]$n)/nrow(df) # insuficiente

print(indicador_5A)

#### Indicador 5B ######
count_lpd <- df %>% count(NIVEL_LPD)
indicador_5B <- (count_lpd[4,]$n + count_lpd[5,]$n)/nrow(df) # suficiente
# indicador_5B <- (count_lpd[1,]$n + count_lpd[2,]$n + count_lpd[3,]$n)/nrow(df) # insuficiente

print(indicador_5B)

#### Indicador 5C ######
count_mt <- df %>% count(NIVEL_MT)
indicador_5C <- (count_mt[3,]$n + count_mt[4,]$n)/nrow(df) # suficiente
# indicador_5C <- (count_mt[1,]$n + count_mt[2,]$n)/nrow(df) # insuficiente

print(indicador_5C)
