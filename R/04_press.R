## Cria tabela xlsx com número de processo, ementa, data de publicação, tipo de processo, nome do relator:

data_press <- readRDS("DATA/data_selec.rds") |>
  dplyr::select(
    cod_ementa,
    numero_processo,
    ementa_completa,
    data_publicacao,
    tipo_processo,
    nome_relator,
    origem
  )

data_press <- data_press |>
  dplyr::mutate(
    data_publicacao = lubridate::year(data_publicacao)
  ) |>
  dplyr::rename(
    cod_unico = cod_ementa,
    ano_publicacao = data_publicacao,
    tipo_processo = tipo_processo,
    relator = nome_relator,
    comarca = origem
  )

data_press$comarca <- data_press$comarca |> decJ::utilitario_remover_acentos()
data_press$relator <- data_press$relator |> 
  decJ::utilitario_remover_acentos() |>
  stringr::str_to_upper()
data_press$tipo_processo <- data_press$tipo_processo |> 
  decJ::utilitario_remover_acentos() |>
  stringr::str_to_upper()

## Exporta em xlsx
xlsx::write.xlsx(
  data_press,
  "DATA/data_press.xlsx",
  row.names = FALSE
)

