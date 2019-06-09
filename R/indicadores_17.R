#' Calcula o indicador 17: "Razão entre salários dos professores da educação básica, na rede pública (não federal), e não professores, com escolaridade equivalente"
#'
#' @import dplyr
#' @export
calc_indicador_17_anual <- function(df_anual, ano, peso = FALSE, verbose = TRUE) {
  if (ano >= 2014) {
    # !!!VD3001 agora é VD3004!!!
    # (1) Primeiro seleciona pessoas com carga horaria entre 10 e 60
    df_select <- df_anual %>%
      filter(RM_RIDE == 26) %>%
      filter(V1023 == 1) %>%
      filter(!is.na(V403312)) %>%
      mutate(horas_ok = (V4039 >= 10 & V4039 <= 60)) %>%
      mutate(rend_ponderado = V403312*V1032) %>%
      mutate(hora_ponderado = V4039*V1032)

    # (2) depois seleciona-se professores da rede publica com sup completo
    df_prof <- df_select %>% filter(horas_ok == TRUE) %>%
      filter(VD3004 == 7 & (V4014 == "1" | V4014 == "2" | V4014 == "3") & (V4010 == "2320" | V4010 == "2330" | V4010 == "2341" | V4010 == "2342" | V4010 == "2351" | V4010 == "2352" | V4010 == "2353" | V4010 == "2354" | V4010 == "2355" | V4010 == "2356" | V4010 == "2359" ))

    # (3) calcula-se rendimento médio dos professores de interesse
    rend_medio_prof_ponderado <- (sum(df_prof$rend_ponderado)/sum(df_prof$hora_ponderado)) * 40
    rend_medio_prof <- (sum(df_prof$V403312)/sum(df_prof$V4039)) * 40

    # (4) seleciona-se demais profissionais com ensino superior
    df_sup <- df_select %>% filter(horas_ok == TRUE) %>%
      filter(VD3004 == 7 & (V4012 == "2" | V4012 == "3" | V4012 == "4") & (V4010 != "2310" | V4010 != "2320" | V4010 != "2330" | V4010 != "2341" | V4010 != "2342" | V4010 != "2351" | V4010 != "2352" | V4010 != "2353" | V4010 != "2354" | V4010 != "2355" | V4010 != "2356" | V4010 != "2359" ))

    # (5) calcula-se rendimento médio dos demais profissionais
    rend_medio_sup_ponderado <- (sum(df_sup$rend_ponderado)/sum(df_sup$hora_ponderado)) * 40
    rend_medio_sup <- (sum(df_sup$V403312))/sum(df_sup$V4039) * 40

    indicador_17_ponderado <- (rend_medio_prof_ponderado/rend_medio_sup_ponderado)*100
    indicador_17 <- (rend_medio_prof/rend_medio_sup)*100
  } else {
    stop("Período não suportado.")
  }

  if (verbose) {
    print(sprintf("Rendimento medio professores (ponderado) [norm. 40h/semanais]: %f", rend_medio_prof_ponderado))
    print(sprintf("Rendimento medio outros profissionais (ponderado) [norm. 40h/semanais]: %f", rend_medio_sup_ponderado))
    print(sprintf("Indicador 17 (ponderado): %f", indicador_17_ponderado))
    print(sprintf("Rendimento medio professores [norm. 40h/semanais]: %f", rend_medio_prof))
    print(sprintf("Rendimento medio outros profissionais [norm. 40h/semanais]: %f", rend_medio_sup))
    print(sprintf("Indicador 17: %f", indicador_17))
  }

  if (peso)
    return(indicador_17_ponderado)
  else
    return(indicador_17)
}
