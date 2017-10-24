context("Parser")

test_that("remove_headers deveria substituir as regexes de cabe\u00e7alho por '\n'.", {

  content_0 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 2\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 3\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\n"

  content_1 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\n"

  expect_equal(remove_headers(content_0), content_1)
})

test_that("remove_footers deveria substituir as regexes de rodap\u00e9 por '\n'.", {

  content_0 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 2\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 3\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\n"

  content_1 <- "Citado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 2\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 3\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\n"

  expect_equal(remove_footers(content_0), content_1)
})

test_that("remove_first_header deveria substituir a regex do primeiro cabe\u00e7alho por '\n'.", {

  content_0 <- "Caderno Judici\u00e1rio do Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o\nDI\u00c1RIO ELETR\u00d4NICO DA JUSTI\u00c7A DO TRABALHO\nPODER JUDICI\u00c1RIO REP\u00daBLICA FEDERATIVA DO BRASIL\nN\u00ba1866/2015 Data da disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015. DEJT Nacional\nTribunal Regional do Trabalho da 1\u00aa Regi\u00e3o\nDESEMBARGADORA MARIA DAS GRA\u00c7AS CABRAL"

  content_1 <- "\nDESEMBARGADORA MARIA DAS GRA\u00c7AS CABRAL"

  expect_equal(remove_first_header(content_0), content_1)
})

test_that("remove_first_header, remove_footers e remove_headers deveriam funcionar juntos.", {

  content_0 <- "Caderno Judici\u00e1rio do Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o\nDI\u00c1RIO ELETR\u00d4NICO DA JUSTI\u00c7A DO TRABALHO\nPODER JUDICI\u00c1RIO REP\u00daBLICA FEDERATIVA DO BRASIL\nN\u00ba1866/2015 Data da disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015. DEJT Nacional\nTribunal Regional do Trabalho da 1\u00aa Regi\u00e3o\nCitado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 2\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\nC\u00f3digo para aferir autenticidade deste caderno: 90953\n\f1866/2015 Tribunal Regional do Trabalho da 1\u00aa Regi\u00e3o 3\nData da Disponibiliza\u00e7\u00e3o: Ter\u00e7a-feira, 01 de Dezembro de 2015\n"

  content_1 <- "\nCitado(s):\n- MUNICIPIO DE DUQUE DE CAXIAS\nDestinat\u00e1rio: MUNICIPIO DE DUQUE DE CAXIAS\nIndeferido o recurso de revista.\n\nEm 1 de Dezembro de 2015.\n9t Secretaria Judici\u00e1ria de 2\u00aa Inst\u00e2ncia\nNotifica\u00e7\u00e3o\nProcesso N\u00ba RO-0010054-35.2014.5.01.0049\nRelator JOSE DA FONSECA MARTINS\nJUNIOR\nRECORAS\n\n"

  expect_equal(remove_first_header(remove_headers(remove_footers(content_0))), content_1)
})
