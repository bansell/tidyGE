
#' Extract tables in DGE objects (used in limma, edgeR and voom) into tidy data frames (tidyverse).
#' @param object A DGE object - either a DGEList (edgeR) or an Elist (voom).
#' @param datatype Desired data table to extract - "counts", counts per million ("cpm"), "samples" or "genes"; "E", "weight"s, or "targets".
#' @export
#' @importFrom magrittr %>%
#' @return A data_frame (aka tibble).
#' @examples
#' dgeExtract(dummyDGEList,"counts")
#' dgeExtract(dummyDGEList,"samples")


dgeExtract <- function( object, datatype){
  if(datatype %in% c('counts','E')){
    request <- object[[datatype]]
    if(length(names(object$genes)) > 1){
    message('Multiple gene information columns present, appending all information')
    cbind(tibble::as_tibble(object$genes), tibble::as_tibble(request))
    }
    else{request %>% tibble::as_tibble() %>%
      dplyr::mutate(labels = row.names(request)) %>% dplyr::select("labels",everything())}
  }
  else if (datatype %in% c('cpm')){
    request <- cpm(object)
    if(length(names(object$genes)) > 1){
      message('Multiple gene information columns present, appending all information')
      print(cbind(tibble::as_tibble(object$genes), tibble::as_tibble(request)))
    }
    else{request %>% tibble::as_tibble() %>%
        dplyr::mutate(labels = row.names(request)) %>% dplyr::select(labels,everything())}
  }
  else if(datatype %in% c('samples','targets')){
    request <- object[[datatype]]
    request %>% tibble::as_tibble() %>%
    dplyr::mutate(labels = row.names(request)) %>% dplyr::select(labels,everything())
      }
  else if (datatype == "genes"){
    if(length(names(object$genes)) > 1){
      message('Multiple gene information columns present, appending all information')
      request <- tibble::as_tibble(object$genes)
    }
    else{
      request <- tibble::as_tibble('labels' = object$genes) }
      request
  }
  else if (datatype == "weights"){
    request <- object[[datatype]]
    request <- request %>% tibble::as_tibble() %>%
      dplyr::mutate(labels = row.names(object$E)) %>% dplyr::select(labels,everything())
    names(request) <- c('label',row.names(object$target))
    request
  }
}
