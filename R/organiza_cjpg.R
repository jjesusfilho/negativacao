### Carregar as bibliotecas necessárias

library(tidyverse)

### Ler a busca de primeiro grau.

a <- list.files("data-raw/cjpg",full.names = T)

cjpg <- tjsp::tjsp_ler_cjpg(a)


### Remove documentos idênticos
cjpg <- cjpg |>
     distinct(cd_doc, .keep_all = T)


### Classifica as decisões

cjpg <- cjpg |>
        mutate(decisao = tjsp::tjsp_classificar_sentenca(julgado))
## Mantêm somente o que for procedente ou improcedente
cjpg <- cjpg |>
     filter(str_detect(decisao,"procedente|parcial"))

cjpg <- cjpg |>
     mutate(decisao = ifelse(decisao == "parcial", "procedente", decisao))


### Salva versão parcial

saveRDS(cjpg, "data/cjpg.rds")

saveRDS(unique(cjpg$processo),"data/processos.rds")

