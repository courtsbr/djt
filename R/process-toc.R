# break text into pages and unscramble it
djt_break_one <- function(page, next_page, name, next_sect, txt) {
  txt_page <- txt[page:next_page]
  N <- length(txt_page)
  # retirar pedaços do texto antes da primeira aparição do padrão atual
  re_section <- name %>%
    stringr::str_replace_all("[[:space:]]+", "[[:space:]\n]+") %>%
    sprintf("[[:space:]]{2}%s", .)
  re_start <- stringr::str_locate(txt_page[1], re_section)[1,1]
  txt_page[1] <- stringr::str_sub(txt_page[1], re_start)
  # retirar pedaços do texto depois da primeira aparição do próximo padrão
  # estou considerando que nunca temos subsections seguidas com o mesmo nome
  re_next <- next_sect %>%
    stringr::str_replace_all("[[:space:]]+", "[[:space:]\n]+") %>%
    sprintf("[[:space:]]{2}%s(.|\n)*", .)
  txt_page[N] <- stringr::str_replace(txt_page[N], re_next, "")
  # cola e retorna
  paste(stringr::str_trim(txt_page), collapse = "\n")
}

# all combinations of party names
# names that appear in two lines are written in both ways
djt_party_dict <- function() {
  c("RECORRENTE",
    "ADVOGAD[OA]",
    "RECORRIDO",
    "IMPETRANTE",
    "IMPETRADO",
    "TERCEIRO INTERESSADO",
    "TERCEIRO", "INTERESSADO",
    "TESTEMUNHA",
    "CUSTOS LEGIS",
    "RECLAMANTE",
    "RECLAMADO",
    "ARREMATANTE",
    "AUTORIDADE COATORA",
    "AUTORIDADE", "COATORA",
    "SUSCITANTE",
    "SUSCITADO",
    "LITISCONSORTE",
    "RELATORA?",
    "PROCESSO",
    "INTIMADO",
    "EXEQUENTE",
    "EXECUTAD[AO]",
    "CLASSE",
    # outros tipos de partes
    "R[qc]te",
    "R[qc]do",
    "Exte[^[:alpha:]]",
    "Agte",
    "Agdo",
    "Aut[^[:alpha:]]",
    "Réu[^[:alpha:].]",
    "Autor[^[:alpha:]]",
    "Destinatário",
    "Procuradoria",
    "Ebgte",
    "Carga[^[:alpha:]]",
    "Ebgdo")
}

#' verifies if the trimmed column has a lawsuit
is_lawsuit <- function(column) {
  re_lawsuit <- paste0(
    "^processo.{1,15}",
    "[0-9]{7}-[0-9]{2}\\.[0-9]{4}\\.[0-9]\\.[0-9]{2}\\.[0-9]{4}")
  re_lawsuit <- stringr::regex(re_lawsuit, ignore_case = TRUE)
  stringr::str_detect(stringr::str_trim(column), re_lawsuit)
}

#' extract name of the part from trimmed column based on djt_party_dict()
extract_part <- function(column) {
  re_part <- djt_party_dict() %>%
    stringr::str_c("^", .) %>%
    stringr::str_c(collapse = "|") %>%
    stringr::regex(ignore_case = TRUE)
  extracted <- stringr::str_extract(column, re_part)
  extracted[stringr::str_detect(extracted, "^[a-z]")] <- NA_character_
  extracted
}

# breaks raw text into sections according to the result of djt_toc()
djt_break_sections <- function(txt, pdf_file) {
  txt_summary <- djt_toc(txt, pdf_file)
  id_summary <- get_id_summary(txt)
  partial_djt_break_one <- purrr::partial(djt_break_one, txt = txt)
  # adiciona uma coluna txt_raw com os pedaços quebrados
  txt_summary %>%
    dplyr::mutate(next_page = dplyr::lead(page, default = id_summary),
                  next_sect = dplyr::lead(name, default = "SUMÁRIO")) %>%
    dplyr::mutate(.id = 1:n()) %>%
    dplyr::select(.id, id_section, page, next_page, name, next_sect) %>%
    dplyr::group_by(.id, id_section, page, name) %>%
    dplyr::mutate(txt_raw = purrr::pmap(list(page, next_page, name, next_sect),
                                        partial_djt_break_one)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(txt_raw)
}

# split by lines and trim output
# takes the result of djt_break_sections as input
djt_split_and_trim <- function(txt_broken) {
  txt_broken %>%
    dplyr::mutate(trimmed = djt_trim(txt_raw)) %>%
    dplyr::select(.id, id_section, page, name, trimmed) %>%
    tidyr::unnest(trimmed) %>%
    dplyr::mutate(is_lawsuit = is_lawsuit(txt_line),
                  part = extract_part(txt_line),
                  is_part = !is.na(part))
}

# nest into lawsuits bd
# takes the result of djt_trim as input
djt_nest_lawsuits <- function(d_trimmed) {
  re_bugs <- stringr::regex("^INTERESSADO|^COATORA", ignore_case = TRUE)
  re_remove <- "DEJT Nacional|^SIL$|da [0-9]{1,2}ª Região"
  d_lawsuits <- d_trimmed %>%
    dplyr::filter(txt_line != "") %>%
    dplyr::filter(!stringr::str_detect(txt_line, re_remove)) %>%
    dplyr::mutate(id_lawsuit = cumsum(is_lawsuit)) %>%
    dplyr::filter(id_lawsuit > 0) %>%
    dplyr::mutate(txt_line = stringr::str_replace(txt_line, re_bugs, ""),
                  txt_line = stringr::str_trim(txt_line)) %>%
    dplyr::group_by(.id, id_section, id_lawsuit) %>%
    dplyr::mutate(id_part = as.integer(cumsum(is_part))) %>%
    dplyr::ungroup() %>%
    tidyr::fill(part) %>%
    dplyr::group_by(.id, id_section, id_lawsuit, id_part, part) %>%
    dplyr::summarise(txt_line = clean_all_but_first(txt_line)) %>%
    dplyr::group_by(.id, id_section, id_lawsuit) %>%
    tidyr::nest()
}
