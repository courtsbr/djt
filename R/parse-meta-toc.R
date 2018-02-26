#' Parse a section from TOC obtained from pdftools::pdf_toc()
#' This is a helper function for djt_meta_toc
parse_single_section <- function(x) {
  subsection <- purrr::pluck(x, "children") %>%
    purrr::map_chr("title")
  tibble::tibble(
    section = purrr::pluck(x, "title"),
    subsection = subsection) %>%
    dplyr::mutate_all(stringr::str_trim)
}
#' a palavra sumário aparece várias vezes, por conta do "RITO SUMÁRIO"
#' mas ela aparece com 2 espaços antes uma vez só
get_id_summary <- function(txt) {
  which(str_detect(txt, "[[:space:]]{2}SUMÁRIO[[:space:]]{2}"))
}
djt_meta_toc <- function(pdf_file) {
  pdf_file %>%
    pdftools::pdf_toc() %>%
    purrr::pluck("children", 1, "children") %>%
    purrr::map_dfr(parse_single_section, .id = "id_section") %>%
    dplyr::mutate(id_section = as.integer(id_section)) %>%
    tidyr::gather(type, name, -id_section) %>%
    dplyr::distinct() %>%
    dplyr::arrange(id_section) %>%
    dplyr::mutate(name = djt_better_names(name))
}
