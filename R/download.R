paginar <- dplyr::failwith(tibble::tibble(result = 'erro'), function(...) {
  body <- dados_req(...)
  r_djt <- httr::POST(u_djt(), body = body, encode = 'form')
  d_tab <- parse_table(r_djt)
  state <- pegar_state(r_djt)
  baixar_pdfs(d_tab = d_tab, state = state, ...)
})

baixar_pdfs <- function(...) {
  l <- list(...)
  saveRDS(l$d_tab, sprintf('%s/tab_pag%05d.rds', l$dir, (l$pag - 1) / 30 + 1))
  l$d_tab %>%
    dplyr::mutate(.id = (1:n()) - 1L) %>%
    dplyr::group_by(.id, titulo, data_disp) %>%
    dplyr::do({
      d_pdf <- tibble::tibble(result = 'erro')
      tentativas <- 0L
      while (d_pdf[['result']] == 'erro' && tentativas < 10) {
        tentativas <- tentativas + 1L
        if (tentativas > 1L) cat(sprintf('pagina %02d...\n', tentativas))
        d_pdf <- baixar_pdf(form = .$form, .id = .$.id,
                            data_disp = .$data_disp, ...)
      }
      d_pdf
    }) %>%
    dplyr::ungroup()
}

baixar_pdf <- dplyr::failwith(tibble::tibble(result = 'erro'), function(...) {
  l <- list(...)
  a <- sprintf('%s/pag%05d_%02d_%02d_%s_%s.pdf',
               l$dir, (l$pag - 1) / 30 + 1, l$.id, l$regiao,
               l$caderno, as.character(l$data_disp))
  wd <- httr::write_disk(a, overwrite = TRUE)
  httr::POST(u_pdf(), body = dados_req_pdf(...), wd)
  tibble::tibble(result = 'OK')
})
