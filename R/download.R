
# [Safe] Rerun search for a page os results
pagination <- purrr::possibly(function(trt, path, booklet, dates, page, state) {

  # Create body of POST request
  body <- list(
    "corpo:formulario:dataIni" = format(dates[1], "%d/%m/%Y"),
    "corpo:formulario:dataFim" = format(dates[2], "%d/%m/%Y"),
    "corpo:formulario:tipoCaderno" = booklet,
    "corpo:formulario:tribunal" = trt,
    "corpo:formulario:ordenacaoPlc" = "",
    "navDe" = page,
    "detCorrPlc" = "",
    "tabCorrPlc" = "",
    "detCorrPlcPaginado" = "",
    "exibeEdDocPlc" = "",
    "indExcDetPlc" = "",
    "org.apache.myfaces.trinidad.faces.FORM" = "corpo:formulario",
    "_noJavaScript" = "false",
    "javax.faces.ViewState" = state,
    "source" = "corpo:formulario:botaoAcaoPesquisar")

  # Get search results
  r_djt <- httr::POST(
    "http://dejt.jt.jus.br/dejt/f/n/diariocon",
    body = body, encode = 'form')

  # Parse results table and get new state
  djt_table <- parse_table(r_djt)
  state <- get_state(r_djt)

  # Download PDFs
  download_pdfs(trt, path, booklet, dates, page, state, djt_table)

}, tibble::tibble(file = "", result = "error"))

# Download PDFs of a page
download_pdfs <- function(trt, path, booklet, dates, page, state, djt_table) {

  # Sabe table as an RDS
  page_i <- stringr::str_pad((page-1)/30+1, 5, "left", "0")
  readr::write_rds(djt_table, stringr::str_c(path, "/table_page_", page_i, ".rds"))

  # Download each PDF
  djt_table %>%
    dplyr::mutate(id = (1:n()) - 1L) %>%
    dplyr::group_by(id, title, date) %>%
    dplyr::do(download_pdf(
      trt, path, booklet, dates, page, state,
      .$form, .$id, .$date, .$title)) %>%
    dplyr::ungroup()
}

# [Safe] Download a PDF
download_pdf <- purrr::possibly(function(trt, path, booklet, dates, page, state,
                                         form, id, date, title) {

  # Create indexes for file name
  page_i <- stringr::str_pad((page-1)/30+1, 5, "left", "0")
  id_i <- stringr::str_pad(id, 2, "left", "0")
  trt_i <- stringr::str_pad(trt, 2, "left", "0")
  booklet_i <- ifelse(stringr::str_detect(title, "Jud"), "1", "0")

  # Create file name
  file <- stringr::str_c(
    path, "/page_", page_i, "_", id_i, "_", trt_i,
    "_", booklet_i, "_", date, ".pdf")

  # Download file if it doesn't exist
  if (!file.exists(file)) {

    # Create body of POST request
    body <- list(
      "corpo:formulario:dataIni" = format(dates[1], "%d/%m/%Y"),
      "corpo:formulario:dataFim" = format(dates[2], "%d/%m/%Y"),
      "corpo:formulario:tipoCaderno" = booklet,
      "corpo:formulario:tribunal" = trt,
      "corpo:formulario:ordenacaoPlc" = "",
      "navDe" = page,
      "detCorrPlc" = "",
      "tabCorrPlc" = "",
      "detCorrPlcPaginado" = "",
      "exibeEdDocPlc" = "",
      "indExcDetPlc" = "",
      "org.apache.myfaces.trinidad.faces.FORM" = "corpo:formulario",
      "_noJavaScript" = "false",
      "javax.faces.ViewState" = state,
      "source" = sprintf("corpo:formulario:plcLogicaItens:%d:j_id135", id))

    # Download file
    httr::POST(
      "http://dejt.jt.jus.br/dejt/f/n/diariocon",
      body = body, httr::write_disk(file, TRUE))
  }

  return(tibble::tibble(file = file, result = "ok"))

}, tibble::tibble(file = "", result = "error"))
