n_pag <- function(r_djt) {
  deate <- '([0-9]+)[[:space:]]+at\u00e9[[:space:]]+([0-9]+)[[:space:]]+de[[:space:]]+([0-9]+)'
  docs <- r_djt %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('.celulaFormulario') %>%
    rvest::html_text() %>%
    stringr::str_match(deate) %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(V1)) %>%
    `[`(1,) %>%
    with(V4) %>%
    as.numeric()
  (docs %/% 30) + 1
}

parse_table <- function(r_djt) {
  tab <- r_djt %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#diarioCon > fieldset > table.tabelaSelecao')
  tab_tibble <- rvest::html_table(tab) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c('data_disp', 'titulo', 'baixar')) %>%
    dplyr::mutate(data_disp = lubridate::dmy(data_disp)) %>%
    dplyr::select(1:2)
  forms <- tab %>%
    rvest::html_nodes('button') %>%
    rvest::html_attr('onclick')
  dplyr::mutate(tab_tibble, form = forms, result = 'OK')
}

pegar_state <- function(r) {
  httr::content(r, 'text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@name="javax.faces.ViewState"]') %>%
    rvest::html_attr('value')
}
