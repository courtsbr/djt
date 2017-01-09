#' @export
pdftohtml <- function(arq, dest_file = NULL){
  dest_file <- check_dest_file(dest_file, arq)
  system(sprintf("pdftohtml -q -s -i -c %s%s", shQuote(arq), dest_file))
}

#' @export
pdftotext <- function(arq, dest_file = NULL){
  dest_file <- check_dest_file(dest_file, arq)
  system(sprintf("pdftotext -raw -q %s%s", shQuote(arq), dest_file))
}

#' @export
check_dest_file <- function(dest_file, arq){
  dest_file <- ifelse(is.null(dest_file),
                      #true
                      '',
                      #false
                      paste0(" ", dirname(arq), "/", dest_file))
  dest_file <- shQuote(dest_file)
}

#' @export
breaks_position <- function(content, milestones){

  stringr::str_locate_all(content, milestones) %>%
  purrr::map2(milestones, function(x,y){
    dplyr::as_data_frame(x) %>%
    dplyr::mutate(tipo = y)}) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(start) %>%
  dplyr::mutate(end = lead(start)-1) %>%
  dplyr::filter(!is.na(end))

}

#' @export
chop_content <- function(content, positions){
  chunks <- stringr::str_sub(content, start = positions$start, end = positions$end)
  names(chunks) = positions$tipo

  return(chunks)
}

#' @export
remove_first_header <- function(content){
  header_regex <- stringr::regex("Caderno Judiciário do Tribunal Regional do Trabalho da [0-9]{1,2}ª Região\nDIÁRIO ELE.*\nPODER JUD.*\nNº[0-9]{4,5}/[0-9]{4}.*\nTribunal .*[0-9]{1,2}ª Região\n")

  stringr::str_replace_all(content, header_regex, "\n")
}

#' @export
remove_headers <- function(content){

 #court_regex <- stringr::regex("\f[0-9]{3,5}/[0-9]{4}.*Região [0-9]{1,2}\n")
 #date_regex <- stringr::regex("\nData da Disponibilização.*\n")

 #mash up the regexes above
 header_regex <- stringr::regex("\f[0-9]{3,5}/[0-9]{4}.*Região [0-9]{1,2}\nData da Disponibilização.*\n")

 stringr::str_replace_all(content, header_regex, "\n")

}

#' @export
remove_footers <- function(content){

  identifier_regex <- stringr::regex("\nCódigo para aferir autenticidade deste caderno.*\n")

  stringr::str_replace_all(content, identifier_regex, "\n")

}

#' @export
pre_process <- function(content){
  content %>%
  remove_first_header %>%
  remove_headers %>%
  remove_footers
}

#' @export
find_html_styles <- function(content){
  content %>%
    xml2::xml_find_all("//style") %>%
    xml2::xml_text()
}

#' @export
build_classes_cf <- function(styles){

  plyr::ldply(styles, function(x){
                       stringr::str_extract_all(x,
                        c("ft[0-9]{1,5}","font-size:[0-9]{1,2}px",
                        "font-family:[:alpha:]{1,15}",
                        "color:#[0-9]{5,8}"), simplify = T) %>%
                        t}) %>%
    setNames(c("class","font_size","font_family","color")) %>%
    dplyr::group_by(font_size, font_family, color)
}

#' @export
build_xpath_query <- function(classes_df){

  classes_df %>%
    dplyr::summarise(xpath =
                       paste(sapply(class, function(x){
                         paste0("' or @class = '",x)
                       }), collapse = '')) %>%
    dplyr::mutate(
      xpath = paste0("./p[", stringr::str_sub(xpath,6),"']"),
      nro_car = nchar(xpath),
      size = stringr::str_extract(font_size, "[0-9]+")) %>%
    dplyr::filter(size >= 13) %>%
    with(xpath)
}
