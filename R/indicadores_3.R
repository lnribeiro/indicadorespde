#' Calcula o indicador 3A: "Percentual da população de 15 a 17 anos que frequenta a escola"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 3A em porcentagem
#' @import dplyr
#' @export
calc_indicador_3A <- function(df, verbose = TRUE) {
  df_dummies <- df %>% select(Ano, RM_RIDE, V1023, UF, V2007, V20081, V20082, V2009, V2010, V3002, V3003A, V3008, V3009A, V3014, V1028, V1022) %>%
    mutate(idade_cne = V2009) %>%
    mutate(estuda = (V3002 == "1")) %>%
    mutate(EM_concl =  ( (  (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                            (V3003A == "11") |  (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) |
                           ((V3009A == "08" & V3014 == "1") | (V3009A == "09" & V3014 == "1") | (V3009A == "10" & V3014 == "1") | (V3009A == "11" & V3014 == "1") | (V3009A == "12" & V3014 == "1")) ) ) %>%
    mutate(V3A = ( estuda | EM_concl))

  df_dummies_15_17 <- df_dummies %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 15 & V2009 <= 17)

  df_dummies_15_17_term <- df_dummies_15_17 %>% filter(V3A == TRUE)

  tot <- sum(df_dummies_15_17$V1028)
  num <- sum(df_dummies_15_17_term$V1028)
  indicador_3A <- (num/tot)*100

  if (verbose == TRUE) {
    print(sprintf("População (ponderada) de 15 a 17 anos que frequenta a escola: %f", num))
    print(sprintf("População (ponderada) de 15 a 17 anos: %f", tot))
    print(sprintf("Indicador 3A: %f", indicador_3A))
  }

  return(indicador_3A)
}

#' Calcula o indicador 3B: "Taxa líquida de matrícula no Ensino Médio"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral
#' @param verbose exibe informações no console se True
#' @return Indicador 3B em porcentagem
#' @import dplyr
#' @export
calc_indicador_3B <- function(df, verbose = TRUE) {
  df_dummies_b <- df %>% select(Ano, RM_RIDE, V1023, UF, V2007, V20081, V20082, V2009, V2010, V3002, V3003A, V3008, V3009A, V3014, V1028, V1022) %>%
    mutate(idade_cne = V2009) %>%
    mutate(EM_regular = (V3003A == "06")) %>%
    mutate(EM_EJA = (V3003A == "07")) %>%
    mutate(EF_concl =  ( ( (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09") | (V3003A == "10") |
                              (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                              (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "1") | (V3009A == "08" & V3014 == "1")) ) ) %>%
    mutate(V3A = (EM_regular | EM_EJA | EF_concl))


  df_dummies_15_17_b <- df_dummies_b %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 15 & V2009 <= 17)

  df_dummies_15_17_term_b <- df_dummies_15_17_b %>% filter(V3A == TRUE)

  tot <- sum(as.numeric(df_dummies_15_17_b$V1028))
  num <- sum(as.numeric(df_dummies_15_17_term_b$V1028))
  indicador_3B <- (num/tot)*100

  if (verbose == TRUE) {
    print(sprintf("População (ponderada) de 15 a 17 anos que frequenta o ensino médio ou concluiu o ensino fundamental: %f", num))
    print(sprintf("População (ponderada) de 15 a 17 anos: %f", tot))
    print(sprintf("Indicador 3B: %f", indicador_3B))
  }

  return(indicador_3B)
}
