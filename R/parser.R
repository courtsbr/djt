
#' Converts a PDF file to text (wraps a call to Poppler's `pdftotext` function)
#'
#' @param file Path to file
#' @param new_file Path to file where to save converted text (not used if
#' `return_text = TRUE`)
#' @param start_pg Page where conversion should start (defaults to `NULL`,
#' equivalent to first page)
#' @param end_pg Page where conversion should end (defaults to `NULL`,
#' equivalent to last page)
#' @param raw Whether conversion should use Poppler's `-raw`
#' @param return_text If `TRUE` this function will return the converted
#' text as a character vector, and if `FALSE` (the default) it will
#' return the path to a `.txt` file containing the converted text
#'
#' @return A character vector either with the converted text or with
#' the path to the new file
#'
#' @export
pdf_to_text <- function(file, new_file = NULL, start_pg = NULL,
                        end_pg = NULL, raw = FALSE, return_text = FALSE) {

  # Check if file is all good
  if (!file.exists(file)) { stop(paste("Coudn't find", file)) }
  if (!stringr::str_detect(file, '.pdf')) { stop(paste(file, "isn't a PDF")) }

  # Create name of new file if necessary
  new_file <- ifelse(
    is.null(new_file),
    stringr::str_replace(file, ".pdf$", ".txt"),
    normalizePath(new_file, mustWork = FALSE))

  # Generate command
  command <- stringr::str_c(
    "pdftotext", file, ifelse(raw, "-raw", ""),
    ifelse(!is.null(start_pg), paste("-f", start_pg), ""),
    ifelse(!is.null(end_pg), paste("-l", end_pg), ""),
    new_file, sep = " ")

  # Run command
  system(command)

  # Remove file if necessary
  if (return_text) {
    out = readr::read_file(new_file)
    file.remove(new_file)
  } else {
    out <- normalizePath(new_file)
  }

  return(out)
}

#' Find all occurrences of multiple patterns
#'
#' @param string Input string
#' @param ... Patterns to be matched
#'
#' @export
find_all <- function(string, ...) {

  # Conver dots into list of patterns
  patterns <- rlang::dots_list(...) %>% rlang::squash_chr()

  # Locate all patterns
  string %>%
    stringr::str_locate_all(patterns) %>%
    purrr::modify(dplyr::as_tibble) %>%
    purrr::map2(patterns, ~dplyr::mutate(.x, pattern = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(start)
}

#' Chop string at given positions
#'
#' @param string Input string
#' @param positions Object returned by [find_all()]
#'
#' @seealso [find_all()]
#'
#' @export
chop_at <- function(string, positions) {

  # Manipulate positions
  patterns <- positions$pattern
  positions <- positions %>%
    dplyr::mutate(end = lead(start) - 1) %>%
    dplyr::mutate(end = ifelse(is.na(end), stringr::str_length(string), end))

  # Split and set names
  chunks <- stringr::str_sub(string, start = positions$start, end = positions$end)
  purrr::set_names(chunks, patterns)
}

#' Remove text's first header
#'
#' @param string Input string
#'
#' @export
remove_first_header <- function(string) {
  regex <- stringr::regex("Caderno Judiciário do Tribunal Regional do Trabalho da [0-9]{1,2}ª Região\nDIÁRIO ELE.*\nPODER JUD.*\nNº[0-9]{4,5}/[0-9]{4}.*\nTribunal .*[0-9]{1,2}ª Região\n")
  stringr::str_replace_all(string, regex, "\n")
}

#' Remove text's headers (except first)
#'
#' @param string Input string
#'
#' @export
remove_headers <- function(string) {
 regex <- stringr::regex("\f[0-9]{3,5}/[0-9]{4}.*Região [0-9]{1,2}\nData da Disponibilização.*\n")
 stringr::str_replace_all(string, regex, "\n")
}

#' Remove text's footers
#'
#' @param string Input string
#'
#' @export
remove_footers <- function(string) {
  regex <- stringr::regex("\nCódigo para aferir autenticidade deste caderno.*\n")
  stringr::str_replace_all(string, regex, "\n")
}

#' Preprocess text (remove headers and footers)
#'
#' @param string Input string
#'
#' @export
pre_process <- function(string) {
  string %>%
    remove_first_header %>%
    remove_headers %>%
    remove_footers
}

#' Find HTML styles
#'
#' @param html Input HTML
#'
#' @export
find_html_styles <- function(html) {
  html %>%
    xml2::xml_find_all("//style") %>%
    xml2::xml_text()
}

build_classes_cf <- function(styles) {

  plyr::ldply(styles, function(x) {
                       stringr::str_extract_all(x,
                        c("ft[0-9]{1,5}","font-size:[0-9]{1,2}px",
                        "font-family:[:alpha:]{1,15}",
                        "color:#[0-9]{5,8}"), simplify = T) %>%
                        t}) %>%
    setNames(c("class","font_size","font_family","color")) %>%
    dplyr::group_by(font_size, font_family, color)
}

build_xpath_query <- function(classes_df) {

  classes_df %>%
    dplyr::summarise(xpath =
                       paste(sapply(class, function(x) {
                         paste0("' or @class = '",x)
                       }), collapse = '')) %>%
    dplyr::mutate(
      xpath = paste0("./p[", stringr::str_sub(xpath,6),"']"),
      nro_car = nchar(xpath),
      size = stringr::str_extract(font_size, "[0-9]+")) %>%
    dplyr::filter(size >= 13) %>%
    with(xpath)
}
