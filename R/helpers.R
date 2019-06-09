#' Wrapper para as funções de cálculo de indicador
#'
#' @param indicador Identificação do indicador a ser calculado, e.g., "1A", "17", etc
#' @param arg1 Primeiro argumento do indicador
#' @param arg2 Segundo argumento do indicador
#' @param arg3 Terceiro argumento do indicador
#' @return Valor do indicador
#' @export
calc_indicador <- function(indicador, arg1, arg2, arg3) {
  switch(indicador,
         "1A" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_1A_censo(df_mat, df_pop, ano)
         },
         "1B" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_1B_censo(df_mat, df_pop, ano)
         },
         "2A" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_2A_censo(df_mat, df_pop, ano)
         },
         "2B" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_2B_censo(df_mat, df_pop, ano)
         },
         "3A" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_3A_censo(df_mat, df_pop, ano)
         },
         "3B" = {
           df_mat <- arg1
           df_pop <- arg2
           ano <- arg3
           calc_indicador_3B_censo(df_mat, df_pop, ano)
         },
         "4" = {
           df <- arg1
           ano <- arg2
           calc_indicador_4(df, ano)
         },
         "5A" = {
           df <- arg1
           ano <- arg2
           calc_indicador_5A(df, ano)
         },
         "5B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_5B(df, ano)
         },
         "5C" = {
           df <- arg1
           ano <- arg2
           calc_indicador_5C(df, ano)
         },
         "6A" = {
           df <- arg1
           ano <- arg2
           calc_indicador_6A(df, ano)
         },
         "6B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_6B(df, ano)
         },
         "8A" = {
           df <- arg1
           ano <- arg2
           calc_indicador_8A_anual(df, ano)
         },
         "8B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_8B_anual(df, ano)
         },
         "8C" = {
           df <- arg1
           ano <- arg2
           calc_indicador_8C_anual(df, ano)
         },
         "8D" = {
           df <- arg1
           ano <- arg2
           calc_indicador_8D_anual(df, ano)
         },
         "9A" = {
           df <- arg1
           ano <- arg2
           calc_indicador_9A_anual(df, ano)
         },
         "9B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_9B_anual(df, ano)
         },
         "10" = {
           df <- arg1
           ano <- arg2
           calc_indicador_10(df, ano)
         },
         "11" = {
           df <- arg1
           ano <- arg2
           calc_indicador_11(df, ano)
         },
         "12A" = {
           df <- arg1
           ano <- arg2
           calc_indicador_12A_anual(df, ano)
         },
         "12B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_12B_anual(df, ano)
         },
         "13A" = {
           df_docentes <- arg1
           df_ies <- arg2
           ano <- arg3
           calc_indicador_13A(df_docentes, df_ies, ano)
         },
         "13B" = {
           df_docentes <- arg1
           df_ies <- arg2
           periodo <- arg3
           calc_indicador_13B(df_docentes, df_ies, periodo)
         },
         "15B" = {
           df <- arg1
           ano <- arg2
           calc_indicador_15B(df, ano)
         },
         "16" = {
           df <- arg1
           ano <- arg2
           calc_indicador_16(df, ano)
         },
         "17" = {
           df <- arg1
           ano <- arg2
           calc_indicador_17_anual(df, ano)
         }
  )
}
