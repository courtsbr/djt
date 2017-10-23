
# Get number of pages in search results
n_pages <- function(r_djt) {
  regex <- "([0-9]+)[[:space:]]+at\u00e9[[:space:]]+([0-9]+)[[:space:]]+de[[:space:]]+([0-9]+)"
  r_djt %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_nodes(".celulaFormulario") %>%
    rvest::html_text() %>%
    stringr::str_match(regex) %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(V1)) %>%
    magrittr::extract(1, ) %>%
    with(V4) %>%
    as.numeric() %>%
    magrittr::divide_by_int(30) %>%
    magrittr::add(1)
}

# Parse results table
parse_table <- function(r_djt) {
  tab <- r_djt %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_node("#diarioCon > fieldset > table.tabelaSelecao")
  table_tibble <- rvest::html_table(tab) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("date", "title", "download")) %>%
    dplyr::mutate(date = lubridate::dmy(date)) %>%
    dplyr::select(1:2)
  forms <- tab %>%
    rvest::html_nodes("button") %>%
    rvest::html_attr("onclick")
  dplyr::mutate(table_tibble, form = forms, result = "ok")
}

# Auxiliary function for getting the state from a page
get_state <- function(r) {
  httr::content(r, 'text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@name="javax.faces.ViewState"]') %>%
    rvest::html_attr('value')
}
