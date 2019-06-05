nm = m_categoria[1:7, 1:9]

qtd_obito_por_tipo_por_idade <-
  matrix(nm[seq(1, 7), seq(1, 9, by = 3)]
         ,
         ncol = 3,
         nrow = 7,
         byrow = FALSE)

colnames(qtd_obito_por_tipo_por_idade) <-
  colunas[seq(1, 9, by = 3)]
rownames(qtd_obito_por_tipo_por_idade) <- linhas[5:11]
