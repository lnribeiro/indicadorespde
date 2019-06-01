#' Calcula o indicador 2A: "Percentual a população de 6 a 14 anos que frequenta a escola"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral (ou anual)
#' @param verbose exibe informações no console se True
#' @return Indicador 2A em porcentagem
#' @import dplyr
#' @export
calc_indicador_2A <- function(df, verbose = TRUE) {
  df_dummies <- df %>%
    mutate(idade_cne = V2009) %>%
    mutate(EF_regular = (V3003A == "04")) %>%
    mutate(EF_EJA = (V3003A == "05")) %>%
    mutate(EF_concl =  ( (  (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                              (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                              (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "01") | (V3009A == "08" & V3014 == "01")) ) ) %>%
    mutate(V2A = (EF_EJA | EF_regular | EF_concl))

  df_dummies_6_14 <- df_dummies %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 >= 6 & V2009 <= 14)

  df_dummies_6_14_term <- df_dummies_6_14 %>% filter(V2A == TRUE)

  num <- nrow(df_dummies_6_14_term)
  tot <- nrow(df_dummies_6_14)
  indicador_2A <- (num/tot)*100

  #tot <- sum(df_dummies_6_14$V1028)
  #num <- sum(df_dummies_6_14_term$V1028)
  #indicador_2A <- (num/tot)*100

  if (verbose == TRUE) {
    print(sprintf("População de 6 a 14 anos que frequenta a escola: %f", num))
    print(sprintf("População de 6 a 14 anos: %f", tot))
    print(sprintf("Indicador 2A: %f", indicador_2A))
  }

  return(indicador_2A)
}

#' Calcula o indicador 2B: "Percentual de pessoas de 16 anos com pelo menos o ensino fundamental concluído"
#'
#' @param df DataFrame com dados carregados da PNAD Contínua trimestral (ou anual)
#' @param verbose exibe informações no console se True
#' @return Indicador 2B em porcentagem
#' @import dplyr
#' @export
calc_indicador_2B <- function(df, verbose = TRUE) {
  df_dummies_b <- df %>%
    mutate(idade_cne = V2009) %>%
    mutate(EF_concl =  ( (  (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                              (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                              (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "01") | (V3009A == "08" & V3014 == "01")) ) )

  df_dummies_16 <- df_dummies_b  %>% filter(RM_RIDE == 26) %>%
    filter(V1023 == 1) %>%
    filter(V2009 == 16)

  df_dummies_16_term <- df_dummies_16 %>% filter(EF_concl == TRUE)

  # tot_b <- sum(as.numeric(df_dummies_16$V1028))
  # num_b <- sum(as.numeric(df_dummies_16_term$V1028))
  # indicador_2B <- (num_b/tot_b)*100

  num_b <- nrow(df_dummies_16_term)
  tot_b <- nrow(df_dummies_16)
  indicador_2B <- (num_b/tot_b)*100

  if (verbose == TRUE) {
    print(sprintf("População de 16 anos com fundamental concluído: %f", num_b))
    print(sprintf("População de 16 anos: %f", tot_b))
    print(sprintf("Indicador 2B: %f", indicador_2B))
  }

  return(indicador_2B)
}
