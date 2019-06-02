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
           calc_indicador_1A_censo(df_mat, df_pop)
         },
         "1B" = {
           df_mat <- arg1
           df_pop <- arg2
           calc_indicador_1B_censo(df_mat, df_pop)
         },
         "2A" = {
           df_mat <- arg1
           df_pop <- arg2
           calc_indicador_2A_censo(df_mat, df_pop)
         },
         "2B" = {
           df_mat <- arg1
           df_pop <- arg2
           calc_indicador_2B_censo(df_mat, df_pop)
         },
         "3A" = {
           df_mat <- arg1
           df_pop <- arg2
           calc_indicador_3A_censo(df_mat, df_pop)
         },
         "3B" = {
           df_mat <- arg1
           df_pop <- arg2
           calc_indicador_3B_censo(df_mat, df_pop)
         },
         "4" = {
           df <- arg1
           calc_indicador_4(df)
         },
         "5A" = {
           df <- arg1
           calc_indicador_5A(df)
         },
         "5B" = {
           df <- arg1
           calc_indicador_5B(df)
         },
         "5C" = {
           df <- arg1
           calc_indicador_5C(df)
         },
         "6A" = {
           df <- arg1
           calc_indicador_6A(df)
         },
         "6B" = {
           df <- arg1
           calc_indicador_6B(df)
         },
         "8A" = {
           df <- arg1
           calc_indicador_8A_anual(df)
         },
         "8B" = {
           df <- arg1
           calc_indicador_8B_anual(df)
         },
         "8C" = {
           df <- arg1
           calc_indicador_8C_anual(df)
         },
         "8D" = {
           df <- arg1
           calc_indicador_8D_anual(df)
         },
         "9A" = {
           df <- arg1
           calc_indicador_9A_anual(df)
         },
         "9B" = {
           df <- arg1
           calc_indicador_9B_anual(df)
         },
         "10" = {
           df <- arg1
           calc_indicador_10(df)
         },
         "11A" = {
           df <- arg1
           calc_indicador_11A(df)
         },
         "12A" = {
           df <- arg1
           calc_indicador_12A_anual(df)
         },
         "12B" = {
           df <- arg1
           calc_indicador_12B_anual(df)
         },
         "13A" = {
           df_docentes <- arg1
           df_ies <- arg2
           periodo <- arg3
           calc_indicador_13A(df_docentes, df_ies, periodo)
         },
         "13B" = {
           df_docentes <- arg1
           df_ies <- arg2
           periodo <- arg3
           calc_indicador_13B(df_docentes, df_ies, periodo)
         },
         "15B" = {
           df <- arg1
           calc_indicador_15B(df)
         },
         "16" = {
           df <- arg1
           calc_indicador_16(df)
         },
         "17" = {
           df <- arg1
           calc_indicador_17_anual(df)
         }
  )
}
