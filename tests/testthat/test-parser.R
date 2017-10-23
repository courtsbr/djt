context("Parser")

test_that("remove_headers deveria substituir as regexes de cabeçalho por '\n'.", {

  content_0 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 2\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 3\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\n"

  content_1 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nCódigo para aferir autenticidade deste caderno: 90953\n\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nCódigo para aferir autenticidade deste caderno: 90953\n\n"

  expect_equal(remove_headers(content_0), content_1)
})

test_that("remove_footers deveria substituir as regexes de rodapé por '\n'.", {

  content_0 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 2\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 3\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\n"

  content_1 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 2\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 3\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\n"

  expect_equal(remove_footers(content_0), content_1)
})

test_that("remove_first_header deveria substituir a regex do primeiro cabeçalho por '\n'.", {

  content_0 <- "Caderno Judiciário do Tribunal Regional do Trabalho da 1ª Região\nDIÁRIO ELETRÔNICO DA JUSTIÇA DO TRABALHO\nPODER JUDICIÁRIO REPÚBLICA FEDERATIVA DO BRASIL\nNº1866/2015 Data da disponibilização: Terça-feira, 01 de Dezembro de 2015. DEJT Nacional\nTribunal Regional do Trabalho da 1ª Região\nDESEMBARGADORA MARIA DAS GRAÇAS CABRAL"

  content_1 <- "\nDESEMBARGADORA MARIA DAS GRAÇAS CABRAL"

  expect_equal(remove_first_header(content_0), content_1)
})

test_that("remove_first_header, remove_footers e remove_headers deveriam funcionar juntos.", {

  content_0 <- "Caderno Judiciário do Tribunal Regional do Trabalho da 1ª Região\nDIÁRIO ELETRÔNICO DA JUSTIÇA DO TRABALHO\nPODER JUDICIÁRIO REPÚBLICA FEDERATIVA DO BRASIL\nNº1866/2015 Data da disponibilização: Terça-feira, 01 de Dezembro de 2015. DEJT Nacional\nTribunal Regional do Trabalho da 1ª Região\nCitado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 2\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nCódigo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1ª Região 3\nData da Disponibilização: Terça-feira, 01 de Dezembro de 2015\n"

  content_1 <- "\nCitado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinatário: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\n\nEm 1 de Dezembro de 2015.\n9t Secretaria Judiciária de 2ª Instância\nNotificação\nProcesso Nº RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\n\n"

  expect_equal(remove_first_header(remove_headers(remove_footers(content_0))), content_1)
})
