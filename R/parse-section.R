# auxiliary functions ----------------------------------------------------------

#' Remove regex from DJT
#'
#' Remove sprcific regex from DJT
#'
#' @param txt DJT text
#' @param re character atomic or regex object
#'
#' @return processed text
#' @export
djt_remove_regex <- function(txt, re) {
  stringr::str_replace_all(txt, re, "")
}

#' remove header from DJT.
#'
#' Tested in TRT01, TRT02 and TRT17
#'
#' @param txt DJT text.
#'
#' @return processed text
#' @export
djt_remove_header <- function(txt) {
  re_court <- "^[[:space:]]*[0-9]+/[0-9]+[[:space:]]{5,}Tribunal Regional.*\n"
  re_date <- "[[:space:]]*Data d[ae] Disponibilização:.*\n"
  re <- paste0(re_court, re_date)
  djt_remove_regex(txt, re)
}

#' remove footer from DJT.
#'
#' Tested in TRT01, TRT02 and TRT17
#'
#' @param txt DJT text.
#'
#' @return processed text
#' @export
djt_remove_footer <- function(txt) {
  re <- "\n[[:space:]]*Código para aferir autenticidade deste caderno.*"
  djt_remove_regex(txt, re)
}

#' Stack columns
#'
#' Stack two columns from each page text position
#'
#' @param txt DJT text
#' @param col1_init index to start first column
#' @param col1_end index to end first column
#' @param col2_init index to start second column
#' @param col2_end index to end second column
#'
#' @return processed text
#' @export
djt_stack_columns <- function(txt, col1_init = 1, col1_end = 86,
                              col2_init = 87, col2_end = -1) {
  lines <- txt %>%
    purrr::map(stringr::str_split, "\n") %>%
    purrr::modify_depth(1, purrr::flatten_chr)
  column1 <- lines %>%
    purrr::map(stringr::str_sub, col1_init, col1_end) %>%
    purrr::map(stringr::str_c, collapse = "\n")
  column2 <- lines %>%
    purrr::map(stringr::str_sub, col2_init, col2_end) %>%
    purrr::map(stringr::str_c, collapse = "\n")
  purrr::map2_chr(column1, column2, stringr::str_c, sep = "\n")
}

#' DJT subsection list
#'
#' List of all subsections found until now. There is no easy way to list all
#'
#' @export
djt_subsection_dict <- function() {
  c("Acórdão", "Ata", "Ato", "Aviso", "Certidão",
    "Decisão Monocrática", "Despacho",
    "Edital", "Notificação",
    "Pauta", "Portaria", "Provimento",
    "Recomendação", "Resolução", "Sentença")
}

#' Heuristics to DJT file into TOC table
#'
#' Heuristics to DJT file into TOC table. Tested in TRT01, TRT02 and TRT17
#'
#' @param txt_summary subset of raw text that contains the summary
#'
#' @export
djt_parse_summary <- function(txt_summary) {
  re_page <- paste0("[^-][[:space:]]([0-9]+)[[:space:]]*$|",
                    "[[:space:]]([0-9]{2,})[[:space:]]*$")
  txt_summary %>%
    stringr::str_c(collapse = "\n") %>%
    stringr::str_replace("(.|\n)*[[:space:]]{2}SUMÁRIO", "") %>%
    stringr::str_split("\n") %>%
    purrr::flatten_chr() %>%
    tibble::enframe("key", "name") %>%
    dplyr::filter(stringr::str_trim(name) != "") %>%
    dplyr::mutate(
      page = stringr::str_extract(name, re_page),
      page = as.integer(stringr::str_replace_all(page, "[^0-9]", ""))
    ) %>%
    # hack to solve court names that end with numbers
    dplyr::mutate(page_lag = as.integer(dplyr::lag(page, default = 0))) %>%
    tidyr::fill(page_lag) %>%
    dplyr::mutate(page = dplyr::if_else(page < page_lag, NA_integer_, page)) %>%
    # end hack
    dplyr::mutate(name = clean_right_side(name, is.na(page))) %>%
    dplyr::select(-page_lag) %>%
    # juntando varas com nome grande que pulam linha
    dplyr::mutate(is_na = is.na(page), group = 1:n() - cumsum(is_na)) %>%
    tidyr::fill(page) %>%
    dplyr::group_by(page, group) %>%
    dplyr::summarise(name = clean_all_but_first(name)) %>%
    dplyr::ungroup() %>%
    dplyr::select(page, name) %>%
    # maybe this part is not easy to replicate
    dplyr::mutate(name = djt_better_names(name),
                  type = dplyr::if_else(name %in% djt_subsection_dict(),
                                        "subsection", "section"))
  ## OLD algorithm to get subsections that does not work on TRT02
  # dplyr::mutate(type = dplyr::if_else(
  #   stringr::str_count(name, "[A-Z]") < 5, "subsection", "section"),
  #   name = stringr::str_trim(name))
}

#' Gets toc from text of pdf file and text
#'
#' Gets summary of raw text, identifies sections and tries to
#'   associate sections to each page. Also, compares with meta-toc
#'   to check if there are any errors on the parsing process
#'
#' @param txt text after remove headers and footers and stack columns
#' @param pdf_file pdf file path to get the meta toc
#'
#' @return tibble containing sections
djt_toc <- function(txt, pdf_file) {
  id_summary <- get_id_summary(txt)
  # o sumário vai até a última página
  txt_summary <- txt[id_summary:length(txt)]
  djt_toc <- txt_summary %>%
    djt_parse_summary() %>%
    dplyr::mutate(id_section = cumsum(type == "section")) %>%
    # is this really necessary?
    dplyr::group_by(id_section) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_section, page, type, name) %>%
    dplyr::arrange(page)
  # merge with TOC
  pdf_toc <- djt_meta_toc(pdf_file)
  errors <- djt_toc %>%
    dplyr::anti_join(pdf_toc, c("type", "name", "id_section"))
  errors2 <- pdf_toc %>%
    dplyr::anti_join(djt_toc, c("type", "name", "id_section"))
  if (nrow(errors) > 0 || nrow(errors2) > 0) {
    all_errors <- c(errors$name, errors2$name)
    message(paste("\nGot errors:\n", all_errors, collapse = ""))
  }
  djt_toc
}

# parse one DJT using other helper functions
# probably the problems will appear in djt_toc inside
# djt_break_sections algorithm
djt_parse_ <- function(pdf_file, verbose) {
  if (verbose) message(paste("\nParsing:", pdf_file))
  txt_raw <- pdftools::pdf_text(pdf_file)
  txt <- txt_raw %>%
    djt_remove_header() %>%
    djt_remove_footer() %>%
    djt_stack_columns()
  txt %>%
    djt_break_sections(pdf_file) %>%
    djt_split_and_trim() %>%
    djt_nest_lawsuits() %>%
    dplyr::mutate(result = "ok")
}

#' Parse DJT files
#'
#' Parse many pdf files into nested section-page structure
#'
#' @param pdf_file character vector of pdf files
#' @param verbose print warning messages? Default true
#'
#' @return tibble having
#' @export
djt_parse <- function(pdf_file, verbose = TRUE) {
  safe_parse <- purrr::possibly(djt_parse_, tibble::tibble(result = "error"))
  pdf_file %>%
    set_names(.) %>%
    {pb <<- progress::progress_bar$new(total = length(.));.} %>%
    purrr::map_chr(~{
      pb$tick()
      result <- safe_parse(.x, verbose = verbose)
      rds_file <- stringr::str_replace(.x, "pdf$", "rds")
      write_rds(result, rds_file)
      invisible(rds_file)
    })
}
