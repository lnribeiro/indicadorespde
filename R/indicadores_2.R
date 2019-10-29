#' @import dplyr
#' @export
calc_indicador_2A <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade6a14 = (V2009 >= 6 & V2009 <= 14)) %>%
    mutate(EF_regular = (V3003A == "04")) %>%
    mutate(EF_EJA = (V3003A == "05")) %>%
    mutate(EF_concl =  ( (  (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                              (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                              (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "01") | (V3009A == "08" & V3014 == "01")) ) ) %>%
    mutate(V2A = (EF_EJA | EF_regular | EF_concl))

  df_num <- df %>% filter( (idade6a14 == TRUE) & (V2A == TRUE) )
  df_den <- df %>% filter( (idade6a14 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)

  indicador_2A <- 100*num/den
  return(indicador_2A)
}

#' @import dplyr
#' @export
calc_indicador_2B <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade16 = (V2009 == 16)) %>%
    mutate(EF_concl =  ( (  (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                              (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                              (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "01") | (V3009A == "08" & V3014 == "01")) ) )

  df_num <- df %>% filter( (idade16 == TRUE) & (EF_concl == TRUE) )
  df_den <- df %>% filter( (idade16 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)
  indicador_2B <- 100*num/den
  return(indicador_2B)
}
