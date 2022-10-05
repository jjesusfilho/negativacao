
base <- cjpg |>
        filter(str_detect(decisao,"(parcial|procedente)")) |>
        mutate(decisao = ifelse(decisao == "parcial", "procedente",decisao)) |>
       select(processo, magistrado, comarca, foro, vara,
              disponibilizacao,
              julgado,
              cd_doc,
              decisao)


base <- base |>
     mutate(dano_moral = str_detect(julgado,"danos? mora"))

base <- base |>
      mutate(dispositivo = str_extract(julgado, "(?i)\\bjulgo\\b.+"))


base <- base |>
       mutate(valor = tjsp::tjsp_obter_valor_max(dispositivo))

base <- base |>
    mutate(gratuidade = str_detect(julgado, "gratu[íi]"))




## Partes

partes <- partes |>
     filter(str_detect(tipo_parte, "(?i)^(req|aut)"))


partes <- partes |>
     mutate(tipo_parte = case_when(
       str_detect(tipo_parte, "(?i)reqd") ~ "requerido",
       str_detect(tipo_parte, "(?i)autor") ~ "requerente",
       str_detect(tipo_parte, "(?i)reqte") ~ "requerente"

     ))


partes <- partes |>
  dplyr::mutate(pessoa = dplyr::case_when(
    stringr::str_detect(parte, "(?i)(banco|finac|cr.edito)") ~ "banco",
    stringr::str_detect(parte, "(?i)(segur)") ~ "seguradora",
    stringr::str_detect(parte,"(?i)(\\bs[./]?a\\.?$|\\bs\\.\\a\\.|\\bs/a.?\\b|s\\sa$|ltda\\.?|\\b[aá]gua\\b|usina|empreend|com[ée]rci|representa|\\bME\\.?\\b|\\bMEI\\.?\\b|\\bEPP\\.?\\b|eirel[ei]|\\bs/?c\\b|companhia|\\bcia\\b)") ~ "PJ",
    TRUE ~ "PF"
  ))


partes <- partes |>
     select(processo, tipo_parte, pessoa) |>
     drop_na()

partes <- partes |>
     pivot_wider(names_from = tipo_parte, values_from = pessoa)

partes <- partes |>
     mutate(requerente = map(requerente, ~{
       .x |>
          str_sort() |>
          unique() |>
          str_c(collapse = ",")


     }))

partes <- partes |>
     mutate(requerente = unlist(requerente))


partes <- partes |>
      mutate(requerido = map(requerido,~{
        .x |>
           str_sort() |>
            unique() |>
            str_c(collapse = ",")

      }) |> unlist())

partes <- partes |>
    mutate(requerido = case_when(
      str_detect(requerido, "banco") ~ "banco",
      str_detect(requerido, "seguradora") ~  "seguradora",
      str_detect(requerido,"PJ") ~ "PJ",
      str_detect(requerido,"PF") ~ "PF",
      TRUE ~ NA_character_
    ))

partes <- partes |>
          drop_na()

partes <- partes |>
          filter(requerente !="")

partes <- partes |>
        mutate(requerente = ifelse(str_detect(requerente,"(PJ|seguradora)"),"PJ", requerente))

dados <- base |>
        select(processo, decisao, dano_moral, gratuidade, valor) |>
        inner_join(partes, by = "processo")

dados <- dados |>
        mutate(across(where(is.logical), ~ifelse(.x == TRUE, "sim", "não") ))


dados <- dados |>
      mutate(across(c(decisao, dano_moral, gratuidade, requerente, requerido), as.factor))



save(c_decisao, dados, file= "docs/base.rda")

dados_glm <- dados |>
         select(-valor)

dados_glm <- dados_glm |>
         drop_na()

dados_lm <- dados |>
      filter(!is.infinite(valor)) |>
      filter(valor <= 100000) |>
      drop_na()



modelo1 <- lm(log(valor) ~ requerente+gratuidade+requerido+dano_moral, data = dados_lm)

modelo2 <- glm(decisao ~ requerente + requerido+gratuidade+dano_moral, data = dados_glm, family = binomial())

options(digits = 3, scipen = 20)

mlm <- broom::tidy(modelo1) |>
       mutate(p.value = round(p.value, 3))
mlogit <- broom::tidy(modelo2) |>
  mutate(p.value = round(p.value, 3))

amostra <- dados_glm |>
       sample_n(10)

predi <-  predict(modelo2, amostra, "response")

amostra$prob <- predi

amostra <- amostra |>
     mutate(predito = ifelse(prob > .50, "procedente","improcedente"))

dados_glm$prob <- predict(modelo2, dados_glm, "response")

dados_glm <- dados_glm |>
  mutate(predito = ifelse(prob > .59, "procedente","improcedente"))

m_confusao <- count(dados_glm, decisao, predito)

save(c_decisao, dados, mlm, mlogit, amostra,m_confusao, file = "docs/base.rda")
