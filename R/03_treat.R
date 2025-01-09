data_selec <- readRDS("DATA/data_selec.rds")

data_selec <- data_selec |>
  dplyr::mutate(
    ementa_completa = ementa_completa |> stringr::str_remove("\\(.*\\)")
  )

## Cria corpus

kids_corpus <- quanteda::corpus(
  data_selec,
  docid_field = "cod_ementa",
  text_field = "ementa_completa"
)

kids_tokens <- quanteda::tokens(
  kids_corpus,
  remove_punct = T,
  remove_symbols = T,
  remove_url = T,
  remove_separators = T,
  remove_numbers = T,
  split_hyphens = T
) |>
  quanteda::tokens_tolower()

  quanteda::tokens_ngrams(
     kids_tokens,
     n = 3,
    concatenator = ' '
  ) |>
    quanteda::dfm() |>
    quanteda::topfeatures(n = 500) |>
    utils::write.csv2(file = "DIC/ngram3.csv",
              quote = F,
              row.names = T,
              fileEncoding = 'UTF-8')

# Carrega o dic
  
dic3 <- read.delim("DIC/dic3.txt", header = FALSE)
  
# APlica
  
  kids_tokens <-
    quanteda::tokens_compound(
      kids_tokens,
      pattern = quanteda::phrase(dic3$V1)
    )

  quanteda::tokens_ngrams(
    kids_tokens,
    n = 2,
    concatenator = ' '
  ) |>
    quanteda::dfm() |>
    quanteda::topfeatures(n = 500) |>
    utils::write.csv2(file = "DIC/ngram2.csv",
                      quote = F,
                      row.names = T,
                      fileEncoding = 'UTF-8')

  dic2 <- read.delim("DIC/dic2.txt", header = FALSE)

  kids_tokens <-
    quanteda::tokens_compound(
      kids_tokens,
      pattern = quanteda::phrase(dic2$V1)
    )
    
  data_ira <- sapply(kids_tokens, paste, collapse = " ") |> as.data.frame() |> tibble::rownames_to_column()
  
  data_ira <- 
    data_ira |>
    dplyr::left_join(
      data_selec,
      by = c("rowname" = 'cod_ementa')
    )
  

data_ira <- data_ira |> dplyr::select(-ementa_completa)

data_ira$ementa_completa <- data_ira$`sapply(kids_tokens, paste, collapse = " ")`

data_iramuteq <- data_ira |> 
  dplyr::mutate(
    tipo_processo = stringr::str_to_lower(stringr::str_replace_all(tipo_processo, "\\s", "_")),
    nome_relator = stringr::str_to_lower(stringr::str_replace_all(nome_relator, '\\s', '_')),
    origem = stringr::str_to_lower(stringr::str_replace_all(origem, '\\s', '_'))
  ) |>
  dplyr::mutate(code = paste0("\n****", 
                              " *cod_",
                              rowname,
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

readr::write_delim(data_iramuteq, 'data2.txt', delim = '\n', col_names = FALSE, quote = 'none')
