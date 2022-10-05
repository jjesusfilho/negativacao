#!/usr/bin/env Rscript

processos <- readRDS(here::here("data/processos.rds"))

grupos <- JurisMiner::dividir_sequencia(processos, 10)


purrr::walk(grupos, ~{

  tjsp::tjsp_autenticar()

  tjsp::tjsp_baixar_cpopg(.x, diretorio = here::here("data-raw/cpopg"))

})
