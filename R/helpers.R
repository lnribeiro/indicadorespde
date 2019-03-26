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
           df <- arg1
           calc_indicador_1A(df)
         },
         "1B" = {
           df <- arg1
           calc_indicador_1B(df)
         },
         "2A" = {
           df <- arg1
           calc_indicador_2A(df)
         },
         "2B" = {
           df <- arg1
           calc_indicador_2B(df)
         },
         "3A" = {
           df <- arg1
           calc_indicador_3A(df)
         },
         "3B" = {
           df <- arg1
           calc_indicador_3B(df)
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
           df_matricula <- arg1
           calc_indicador_6A(df_matricula)
         },
         "6B" = {
           df_matricula <- arg1
           calc_indicador_6B(df_matricula)
         },
         "8A" = {
           df <- arg1
           calc_indicador_8A(df)
         },
         "8B" = {
           df <- arg1
           calc_indicador_8B(df)
         },
         "8C" = {
           df_anual <- arg1
           calc_indicador_8C(df_anual)
         },
         "8D" = {
           df <- arg1
           calc_indicador_8D(df)
         },
         "9A" = {
           df <- arg1
           calc_indicador_9A(df)
         },
         "9B" = {
           df <- arg1
           calc_indicador_9B(df)
         },
         "10" = {
           df_matricula <- arg1
           df_escola <- arg2
           calc_indicador_10(df_matricula, df_escola)
         },
         "11A" = {
           df_matricula <- arg1
           calc_indicador_11A(df_matricula)
         },
         "12A" = {
           df <- arg1
           calc_indicador_12A(df)
         },
         "12B" = {
           df <- arg1
           calc_indicador_12B(df)
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
         # "15B" = {
         #   df_docentes <- arg1
         #   calc_indicador_15B(df_docentes)
         # },
         "16" = {
           df_docentes <- arg1
           calc_indicador_16(df_docentes)
         },
         "17" = {
           df <- arg1
           calc_indicador_17(df)
         }
  )
}
