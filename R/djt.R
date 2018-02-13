
#' Download DJT PDFs based on date, booklet, and trt
#'
#' @param trt Number between 1 and 24 indicating what TRT to download DJT from
#' or `"all"` for all TRTs
#' @param path Path where to save files
#' @param booklet One of `"jud"` (for judicial), `"adm"` (for administrative),
#' or `"all"` (for both)
#' @param date_min Lower bound for dates to be downloaded
#' @param date_max Upper bound for dates to be downloaded
#'
#' @return A `tibble` with identifiers and DJT URLs
#'
#' @export
download_djt <- function(trt, path, booklet = "all", date_min = Sys.Date() - 1,
                         date_max = date_min) {

  # Create directory if necessary
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  # Convert parameters to internal format and check them
  trt <- ifelse(trt == "tst", "0", ifelse(trt == "all", "", as.integer(trt)))
  booklet <- switch (booklet, "adm" = "0", "jud" = "1", "all" = "")
  dates <- lubridate::ymd(date_min, date_max)
  check_parms(trt, booklet, dates)

  # GET page where search will happen
  r_search <- httr::GET("https://dejt.jt.jus.br/dejt/f/n/diariocon")

  # Create body of POST request
  body <- list(
    "corpo:formulario:dataIni" = format(dates[1], "%d/%m/%Y"),
    "corpo:formulario:dataFim" = format(dates[2], "%d/%m/%Y"),
    "corpo:formulario:tipoCaderno" = booklet,
    "corpo:formulario:tribunal" = trt,
    "corpo:formulario:ordenacaoPlc" = "",
    "navDe" = "",
    "detCorrPlc" = "",
    "tabCorrPlc" = "",
    "detCorrPlcPaginado" = "",
    "exibeEdDocPlc" = "",
    "indExcDetPlc" = "",
    "org.apache.myfaces.trinidad.faces.FORM" = "corpo:formulario",
    "_noJavaScript" = "false",
    "javax.faces.ViewState" = get_state(r_search),
    "source" = "corpo:formulario:botaoAcaoPesquisar")

  # Get search results
  r_djt <- httr::POST(
    "https://dejt.jt.jus.br/dejt/f/n/diariocon",
    body = body, encode = 'form')
  n_pages <- n_pages(r_djt)

  # Download PDFs of each page
  tb_error <- tibble::tibble(file = "", result = "error")
  safe_pagination <- purrr::possibly(pagination, otherwise = tb_error)

  out <- purrr::map_dfr(seq_len(n_pages), safe_pagination,
                        trt = trt,
                        path = path,
                        booklet = booklet,
                        dates = dates,
                        state = get_state(r_search),
                        jid = get_jid(r_djt),
                        .id = "page")

  return(out)
}

# Check if all parameters in a call to [download_djt()] are ok
check_parms <- function(trt, booklet, dates) {

  # Check whether each parameter fits the expected format
  if (length(trt) != 1 || !trt %in% c("", 0:24)) {
    stop("TRT must be an number between 1 and 24 or ''")
  }
  if (length(booklet) != 1 || !booklet %in% c("0", "1", "")) {
    stop("Booklet must be either 'jud', 'adm', or 'all'")
  }
  if (!lubridate::is.Date(dates) | any(is.na(dates))) {
    stop("Dates must either have type Date or be in YYYY-MM-DD format")
  }
  if (dates[2] < dates[1]) {
    stop("Minimum date must be smaller than maximum date")
  }
  if (dates[2] > Sys.Date()) {
    stop("Maximum date in the future")
  }

  invisible()
}
