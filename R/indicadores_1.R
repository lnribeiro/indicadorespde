#' @import dplyr
#' @export
calc_indicador_1A <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade4a5 = (V2009 %in% c(4,5))) %>%
    mutate(estuda = (V3002==1)) %>%
    select(V2009,V1028,idade4a5,V3002,estuda)

  df_num <- df %>% filter( (idade4a5 == TRUE) & (estuda == TRUE) )
  df_den <- df %>% filter( (idade4a5 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)
  indicador_1A <- 100*num/den
  return(indicador_1A)
}

#' @import dplyr
#' @export
calc_indicador_1B <- function(df_pnadc_ed, verbose = TRUE) {
  df <- df_pnadc_ed %>%
    filter(RM_RIDE == 26) %>%
    mutate(idade0a3 = (V2009 <= 3)) %>%
    mutate(estuda = (V3002==1)) %>%
    select(V2009,V1028,idade0a3,V3002,estuda)

  df_num <- df %>% filter( (idade0a3 == TRUE) & (estuda == TRUE) )
  df_den <- df %>% filter( (idade0a3 == TRUE) )

  num <- sum(df_num$V1028)
  den <- sum(df_den$V1028)
  indicador_1B <- 100*num/den
  return(indicador_1B)
}
