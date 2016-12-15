dados_req <- function(...) {
  l <- list(...)
  cad <- ifelse(l$caderno == 'jud', '1', ifelse(l$caderno == 'adm', '0', ''))
  reg <- ifelse(l$regiao == 0, '', l$regiao)
  list('corpo:formulario:dataIni' = format(l$dts[1], '%d/%m/%Y'),
       'corpo:formulario:dataFim' = format(l$dts[2], '%d/%m/%Y'),
       'corpo:formulario:tipoCaderno' = cad,
       'corpo:formulario:tribunal' = reg,
       'corpo:formulario:ordenacaoPlc' = '',
       'navDe' = l$de,
       'detCorrPlc' = '',
       'tabCorrPlc' = '',
       'detCorrPlcPaginado' = '',
       'exibeEdDocPlc' = '',
       'indExcDetPlc' = '',
       'org.apache.myfaces.trinidad.faces.FORM' = 'corpo:formulario',
       '_noJavaScript' = 'false',
       'javax.faces.ViewState' = l$state,
       'source' = 'corpo:formulario:botaoAcaoPesquisar')
}

dados_req_pdf <- function(...) {
  l <- list(...)
  cad <- ifelse(l$caderno == 'jud', '1', ifelse(l$caderno == 'adm', '0', ''))
  reg <- ifelse(l$regiao == 0, '', l$regiao)
  .id <- l$.id
  list('corpo:formulario:dataIni' = format(l$dts[1], '%d/%m/%Y'),
       'corpo:formulario:dataFim' = format(l$dts[2], '%d/%m/%Y'),
       'corpo:formulario:tipoCaderno' = cad,
       'corpo:formulario:tribunal' = reg,
       'corpo:formulario:ordenacaoPlc' = '',
       'navDe' = l$de,
       'detCorrPlc' = '',
       'tabCorrPlc' = '',
       'detCorrPlcPaginado' = '',
       'exibeEdDocPlc' = '',
       'indExcDetPlc' = '',
       'org.apache.myfaces.trinidad.faces.FORM' = 'corpo:formulario',
       '_noJavaScript' = 'false',
       'javax.faces.ViewState' = l$state,
       'source' = sprintf('corpo:formulario:plcLogicaItens:%d:j_id135', l$.id))
}
