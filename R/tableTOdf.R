#' Convert a table to tidy data_frame

#' @param table A data.frame or matrix, or table nexted within a list
#' @return A tidy data_frame (aka tibble), with rownames converted a to field named 'key'.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' tableTOdf(mtcars)
#' tableTOdf(NList$c)

tableTOdf <- function(mytable){
  mytable %>% tibble::as_tibble() %>%
    dplyr::mutate(key = rownames(mytable)) %>%
    dplyr::select("key", dplyr::everything())
}

