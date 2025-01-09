# Realiza a busca
data <- buscar_jurisprudencia_tjrs("acolhimento institucional E convivencia familiar")
# Aplica o filtro de acórdãos e seleciona as colunas úteis
data_selec <- data |> dplyr::filter(tipo_documento == "Acordao") |> dplyr::select(origem, ementa_completa, tipo_processo, data_publicacao, secao, data_julgamento, nome_tribunal, numero_processo, cod_ementa, orgao_julgador, ind_segredo_justica, nome_assunto_cnj, nome_classe_cnj, nome_relator, relator_redator)
# Retira espaços em branco inúteis das ementas
data_selec$ementa_completa <- data_selec$ementa_completa |> stringr::str_trim() |> stringr::str_squish()
# Salva arquivo RDS para economia de processamento
saveRDS(data_selec, file = "DATA/data_selec.rds")
# Tratamentos básicos para iramuteq
data_iramuteq <- data_selec |> 
  dplyr::mutate(
    tipo_processo = stringr::str_to_lower(stringr::str_replace_all(tipo_processo, "\\s", "_")),
    nome_relator = stringr::str_to_upper(stringr::str_replace_all(nome_relator, '\\s', '_'))
  ) |>
  # Cria código para identificação de cada ementa
  dplyr::mutate(code = paste0("\n****", 
               " *cod_",
               cod_ementa,
               " *ori_",
               origem,
               " *tipo_proc_",
               tipo_processo,
               " *ano_",
               lubridate::year(data_julgamento),
               " *rel_",
               nome_relator
)) |>
  dplyr::select(code, ementa_completa)

# Salva RDS dos dados iramuteq para economia de processamento
saveRDS(data_iramuteq, file = "DATA/data.rds")
# Exporta txt para análises iramuteq
readr::write_delim(data_iramuteq, 'data.txt', delim = '\n', col_names = FALSE, quote = 'none')
