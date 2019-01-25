require(dplyr)

data_path <- "./dados"
anos <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")
trimestres <- c("1q", "2q", "3q", "4q")

calc_indicador_1A <- function(ano, trimestre) {
  filename <- paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv")
  df <- read.csv2(filename, header = TRUE, sep=",")
  
  pessoas_total <- df %>% 
    filter(V2009 >= 4 & V2009 <= 5)
  
  if ((ano == "2015" & trimestre == "4q") | ano == "2016" | ano == "2017" | ano == "2018") {
    escola <- pessoas_total %>% 
      count(V3003A) %>%
      filter(!is.na(V3003A)) %>% 
      summarise(total=sum(n))
  } else {
    escola <- pessoas_total %>% 
      count(V3003) %>%
      filter(!is.na(V3003)) %>% 
      summarise(total=sum(n))    
  }
  
  return(escola$total/nrow(pessoas_total)) 
}

calc_indicador_1B <- function(ano, trimestre) {
  filename <- paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv")
  df <- read.csv2(filename, header = TRUE, sep=",")
  
  pessoas_total <- df %>% 
    filter(V2009 >= 0 & V2009 <= 3)
  
  escola <- pessoas_total %>% 
    count(V3002) %>%
    filter(V3002 == 1)
  
  return(escola$n/nrow(pessoas_total)) 
}

calc_indicador_2A <- function(ano, trimestre) {
  filename <- paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv")
  df <- read.csv2(filename, header = TRUE, sep=",")
  
  pessoas_total <- df %>% 
    filter(V2009 >= 6 & V2009 <= 14)
  
  escola <- pessoas_total %>% 
    count(V3002) %>%
    filter(V3002 == 1)
  
  return(escola$n/nrow(pessoas_total))
}

calc_indicador_2B <- function(ano, trimestre) {
  filename <- paste0(data_path, "/pnadc-", ano, "-", trimestre, ".csv")
  df <- read.csv2(filename, header = TRUE, sep=",")
  
  pessoas_16_anos <- df %>% 
    filter(V2009 == 16)
  
  if ((ano == "2015" & trimestre == "4q") | ano == "2016" | ano == "2017" | ano == "2018") {
    pessoas_16_anos_fund_completo <- pessoas_16_anos %>%
      filter(V3003A > 4)
  } else {
    pessoas_16_anos_fund_completo <- pessoas_16_anos %>%
      filter(V3003 > 4)
  }
  
  return(nrow(pessoas_16_anos_fund_completo)/nrow(pessoas_16_anos)) 
}

indicadores <- list()
indicadores[['1A']] <- list()
indicadores[['2A']] <- list()
indicadores[['3A']] <- list()
indicadores[['4A']] <- list()

for (ano in anos) {
  for (trimestre in trimestres) {
    
    if (ano == "2018" & trimestre == "4q") break
    
    id <- paste0(ano, "-", trimestre)
    indicadores[['1A']][[id]] <- calc_indicador_1A(ano, trimestre)
    indicadores[['1B']][[id]] <- calc_indicador_1B(ano, trimestre)
    indicadores[['2A']][[id]] <- calc_indicador_2A(ano, trimestre)
    indicadores[['2B']][[id]] <- calc_indicador_2B(ano, trimestre)
    
    print(paste("Ano", ano))
    print(paste("Trimestre", trimestre))
    print(paste("1A:", indicadores[['1A']][[id]]))
    print(paste("1B:", indicadores[['1B']][[id]]))
    print(paste("2A:", indicadores[['2A']][[id]]))
    print(paste("2B:", indicadores[['2B']][[id]]))
  }
}

write.csv(indicadores[['1A']], "indicadores1A.csv", sep=";")
write.csv(indicadores[['1B']], "indicadores1B.csv", sep=";")
write.csv(indicadores[['2A']], "indicadores2A.csv", sep=";")
write.csv(indicadores[['2B']], "indicadores2B.csv", sep=";")
