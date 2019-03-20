require(ff)
require(ffbase)

data_path <- path.expand("/Users/lucas/Downloads/MICRODADOS_2016/DADOS")
df <- read.csv2.ffdf(file = paste0(data_path, "/TS_ALUNO.csv"), header=TRUE, VERBOSE=TRUE, sep=",", first.rows=240000)

idx <- ffwhich(df, df[["ID_MUNICIPIO"]] == "2611606") 
df_recife <- df[idx,]

write.csv2.ffdf(df_recife, file="ana_recife_2016.csv")

