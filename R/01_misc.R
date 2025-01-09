## Cria função de buscar jurisprudencia com base na palavra-chave e data de julgamento
buscar_jurisprudencia_tjrs <- function(palavra_chave, data_julgamento_de, data_julgamento_ate){
  if(missing(data_julgamento_de)) data_julgamento_de <- ""
  if(missing(data_julgamento_ate)) data_julgamento_ate <- ""
  # Salva os parâmetros da busca
  corpo <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual=1&q_palavra_chave={palavra_chave}&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={data_julgamento_de}&data_julgamento_ate={data_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
  )
  # Salva a URL para requisição dos dados
  url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"
  # Salva o USER-AGENT
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36 Edg/131.0.0.0"
  # Realiza a requisição
  req <- httr::POST(url, body = corpo, headers = httr::add_headers(`User-Agent` = UA))
  # Explora os conteúdos da requisição
  resp <- httr::content(req, as = "text") |> jsonlite::fromJSON()
  n_pages <- resp$response$numFound
  n_pages <- ceiling(n_pages / 10)
  # Junta todas as páginas em um único data.frame
  data <- purrr::map_df(1:n_pages, purrr::slowly(~{
    corpo <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={.x}&q_palavra_chave={palavra_chave}&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={data_julgamento_de}&data_julgamento_ate={data_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )
    req <- httr::POST(url, body = corpo, headers = httr::add_headers(`User-Agent` = UA))
    resp <- httr::content(req, as = "text") |> jsonlite::fromJSON() |> purrr::pluck("response", "docs")
  }, rate = purrr::rate_delay(5)), .progress = TRUE)
  # Retorna dataframe com as decisões
  return(data)
}
