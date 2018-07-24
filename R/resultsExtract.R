

#' Extract tables in DGE objects (used in limma, edgeR and voom) into tidy data frames (tidyverse).
#' @param object An object of class TopTags
#' @importFrom magrittr %>%
#' @export
#' @return A data_frame (aka tibble).
#' @example
#' resultsExtract(tt)



resultsExtract <- function(object){
  if(class(object)[1]=="TopTags"){
  object$table %>% tibble::as_tibble() %>%
      dplyr::mutate(labels = row.names(object)) %>%
    dplyr::select("labels",everything())}
  else{
  object %>% tibble::as_tibble() %>%
    dplyr::mutate(labels = row.names(object)) %>%
    dplyr::select("labels",everything())
  }
}

