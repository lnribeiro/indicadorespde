#' @import dplyr
#' @export
calc_indicador_3A <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade15a17 = (V2009 >= 15 & V2009 <= 17)) %>%
    mutate(estuda = (V3002 == "1")) %>%
    mutate(EM_concl =  ( (  (V3003A == "08")  | (V3003A == "09")    | (V3003A == "10") |
                              (V3003A == "11")  |  (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) |
                           ((V3009A == "08" & V3014 == "1") | (V3009A == "09" & V3014 == "1") | (V3009A == "10" & V3014 == "1") | (V3009A == "11" & V3014 == "1") | (V3009A == "12" & V3014 == "1")) ) ) %>%
    mutate(V3A = ( estuda | EM_concl))

  df_num <- df %>% filter( (idade15a17 == TRUE) & (V3A == TRUE) )
  df_den <- df %>% filter( (idade15a17 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)
  indicador_3A <- 100*num/den
  return(indicador_3A)
}

#' @import dplyr
#' @export
calc_indicador_3B <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade15a17 = (V2009 >= 15 & V2009 <= 17)) %>%
    mutate(EM_regular = (V3003A == "06")) %>%
    mutate(EM_EJA = (V3003A == "07")) %>%
    mutate(EF_concl =  ( ( (V3003A == "06")  | (V3003A == "07")  | (V3003A == "08")  | (V3003A == "09") | (V3003A == "10") |
                             (V3003A == "11") | (V3009A == "06")  | (V3009A == "09")  | (V3009A == "10")   | (V3009A == "11") |
                             (V3009A == "12") | (V3009A == "13") | (V3009A == "14") | (V3009A == "15") ) | ((V3009A == "07" & V3014 == "1") | (V3009A == "08" & V3014 == "1")) ) ) %>%
    mutate(V3B = (EM_regular | EM_EJA | EF_concl))

  df_num <- df %>% filter( (idade15a17 == TRUE) & (V3B == TRUE) )
  df_den <- df %>% filter( (idade15a17 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)
  indicador_3B <- 100*num/den
  return(indicador_3B)
}
