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

#' @import dplyr
#' @export
calc_indicador_3A_censo <- function(df_mat, df_pop, verbose = TRUE) {
  num_mat_15_a_17 <- df_mat %>%
    filter(NU_IDADE_REFERENCIA >= 15 & NU_IDADE_REFERENCIA <= 17) %>%
    filter(!is.na(TP_ETAPA_ENSINO)) %>% nrow

  ano <- df_mat$NU_ANO_CENSO[1]
  colname <- paste0('X', ano)
  num_pop_15_a_17 <- sum(df_pop[[colname]][16:18])

  indicador_3A <- (num_mat_15_a_17/num_pop_15_a_17)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 15 a 17 que frequenta escola: %f", num_mat_15_a_17))
    print(sprintf("num população 15 a 17: %f", num_pop_15_a_17))
    print(sprintf("indicador 3A: %f", indicador_3A))
  }

  return(indicador_3A)
}

#' @import dplyr
#' @export
calc_indicador_3B_censo <- function(df_mat, df_pop, verbose = TRUE) {
  ens_fund <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 41, 65, 69, 70, 72, 73)

  num_mat_15_a_17 <- df_mat %>%
    filter(NU_IDADE_REFERENCIA >= 15 & NU_IDADE_REFERENCIA <= 17) %>%
    filter(! (TP_ETAPA_ENSINO %in% ens_fund) ) %>%
    filter(!is.na(TP_ETAPA_ENSINO)) %>% nrow

  ano <- df_mat$NU_ANO_CENSO[1]
  colname <- paste0('X', ano)
  num_pop_15_a_17 <- sum(df_pop[[colname]][16:18])

  indicador_3B <- (num_mat_15_a_17/num_pop_15_a_17)*100

  if (verbose == TRUE) {
    print(sprintf("num mat 15 a 17 anos ens fund conc: %f", num_mat_15_a_17))
    print(sprintf("num populacao 15 a 17 anos: %f", num_pop_15_a_17))
    print(sprintf("indicador 3B: %f", indicador_3B))
  }

  return(indicador_3B)
}
