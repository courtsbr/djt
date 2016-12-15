#' Baixar links do djt.
#'
#' Baixar os pdfs do DJT com base em datas, caderno e regiao.
#'
#' @param regiao inteiro entre 1 e 24 identificando o TRT.
#' @param caderno \code{jud} para judicial, \code{adm} para administrativo. Default \code{jud}.
#' @param dt_min,dt_max datas minima e maxima da busca, em formato \code{Date} ou 'YYYY-MM-DD'. Default \code{\link{Sys.Date}}
#' @param dir pasta onde os arquivos serao salvos.
#'
#' @return \code{tibble} com identificadores e links dos DJTs.
#'
#' @export
baixar_djt <- function(regiao = 0L, caderno = '', dt_min = Sys.Date() - 1, dt_max = dt_min, dir = 'data-raw/djt') {
  regiao <- as.integer(regiao)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dts <- suppressWarnings(lubridate::ymd(dt_min, dt_max))
  check_parms(regiao = regiao, caderno = caderno, dts = dts)
  u <- u_djt()
  r <- httr::GET(u)
  state <- pegar_state(r)
  body <- dados_req(state = state, dts = dts, regiao = regiao, caderno = caderno)
  r_djt <- httr::POST(u, body = body, encode = 'form')
  n_pags <- n_pag(r_djt)
  # faz uma req a mais do que precisaria. pregs
  d <- tibble::tibble(pag = 1:n_pags) %>%
    dplyr::group_by(pag) %>%
    dplyr::do(paginar(
      pag = (.$pag - 1) * 30 + 1, state = state, dts = dts,
      regiao = regiao, caderno = caderno, dir = dir
    )) %>%
    dplyr::ungroup()
  d
}

check_parms <- function(...) {
  l <- list(...)
  if (length(l$regiao) != 1 || !l$regiao %in% 0:24) stop('Regiao deve ser um inteiro entre 0 e 24.')
  if (length(l$caderno) != 1 || !l$caderno %in% c('jud', 'adm', '')) stop("Caderno deve ser 'jud', 'adm' ou ''.")
  if (!lubridate::is.Date(l$dts) | any(is.na(l$dts))) stop("Datas devem ser do tipo Date ou no formato YYYY-MM-DD.")
  if (l$dts[2] < l$dts[1]) stop("Data minima maior que data maxima.")
  if (l$dts[2] > Sys.Date()) stop("Data maxima no futuro.")
  invisible()
}

