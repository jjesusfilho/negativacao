a <- JurisMiner::listar_arquivos("data-raw/cpopg")

partes <- tjsp::tjsp_ler_partes(a)
saveRDS(partes, "data/partes.rds")

dados <- tjsp::tjsp_ler_dados_cpopg(a)
saveRDS(dados,"data/dados.rds")
