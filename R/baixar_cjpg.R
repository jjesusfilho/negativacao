#!/usr/bin/env Rscript


classe = 8501 ## Procedimento comum cível
assunto = 6226 ## Inclusão indevida no cadastro de inadimplentes

dt_ini<- "01/01/2021"
dt_fim <- "31/12/2021"

tjsp::tjsp_baixar_cjpg(classe = classe,
                       assunto = assunto,
                       inicio = dt_ini,
                       fim = dt_fim,
                       diretorio = here::here("data-raw/cjpg")
)

