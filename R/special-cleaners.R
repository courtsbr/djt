#' Special cleaner
#' trim just the right side of a text and remove numbers in the end.
#' Doesn't clean if num is NA
clean_right_side <- function(x, na_num) {
  x %>%
    purrr::map_if(!na_num, ~{
      .x %>%
        stringi::stri_trim_right() %>%
        stringr::str_replace_all(., "[0-9]+$", "") %>%
        stringi::stri_trim_right()
    }) %>%
    purrr::flatten_chr()
}
#' Special cleaner
#' trim all texts but keeps left trim on first element
clean_all_but_first <- function(x) {
  if (length(x) == 1) return(x)
  x[2:length(x)] <- stringr::str_trim(x[2:length(x)])
  stringr::str_c(x, collapse = " ")
}
#' Special cleaner
#' better names to help merge work
djt_better_names <- function(name) {
  name %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[[:space:]]+", " ") %>%
    stringr::str_replace_all("-([0-9]+)", "- \\1")
}
#' Special cleaner
#' split raw text into escapes and trim output
djt_trim <- function(txt_raw) {
  txt_raw %>%
    stringr::str_split("\n") %>%
    purrr::map(stringr::str_trim) %>%
    purrr::map(tibble::enframe, "id_line", "txt_line")
}
