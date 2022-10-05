c_decisao <- c_decisao |>
     mutate(decisao = ifelse(decisao =="parcial","parcialmente procedente", decisao))

names(c_decisao) <- c("decisao", "frequencia")

saveRDS(c_decisao,"docs/c_decisao.rds")

dados |>
     count(dano_moral, decisao) |>
     mutate(dano_moral = ifelse(dano_moral == "sim", "Com dano moral", "Sem dano moral")) |>
     ggplot(aes(x = decisao, y = n, fill = decisao))+
     geom_col(show.legend = F)+
     scale_fill_viridis_d()+
     facet_wrap(~dano_moral)+
     labs(title = "Julgados conforme o mérito e a presença ou não de dano moral",
          caption = "Fonte: TJSP")+
     theme_minimal()


dados |>
  count(gratuidade, decisao) |>
  mutate(gratuidade = ifelse(gratuidade == "sim", "Com gratuidade", "Sem gratuidade")) |>
  ggplot(aes(x = decisao, y = n, fill = decisao))+
  geom_col(show.legend = F)+
  scale_fill_viridis_d()+
  facet_wrap(~gratuidade)+
  labs(title = "Julgados conforme o mérito e a presença ou não de gratuidade",
       caption = "Fonte: TJSP")+
  theme_minimal()


dados |>
  count(dano_moral, decisao) |>
  mutate(dano_moral = ifelse(dano_moral == "sim", "Com dano moral", "Sem dano moral")) |>
  ggplot(aes(x = decisao, y = n, fill = dano_moral))+
  geom_col(show.legend = F)+
  scale_fill_viridis_d()+
  facet_wrap(~dano_moral)+
  labs(title = "Julgados conforme o mérito e a presença ou não de dano moral",
       caption = "Fonte: TJSP")+
  theme_minimal()




