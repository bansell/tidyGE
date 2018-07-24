#Extract GO


#' Extract from a DGE object genes associated with a GO term of interest, e.g. from GSEA testing. Requires GOsource() output.
#' @param DGEobject A TopTags (DGE results) object. GeneID must be entrezgene, symbol or RefSeqID. Later versions will support ensemble_gene IDs and extract associated genes from a DGEList.
#' @param GO_query A GO term e.g. "GO:0002283"
#' @param resultsType Type of output to match. "results" indicates that DGEobject is a TopTags object. Future versions will allow "counts", or "cpm" output.
#' @export
#' @importFrom magrittr %>%
#' @return A data_frame (aka tibble) to the console. EG.GO and entrez_joinTable are exported to R_GlobalEnv.
#' @examples
#' GO2genes(tt,"GO:0002283","results")


#args: DGEobject, GO_query, species, resultsType ('results'/'counts'  = resultsExtract() / dgeExtract())
##dgeExtract() is broken. Focus on resultsExtract #ideally: GO2genes(d, 'Hs', 'GO:0002283', 'counts')

#test:
#GO2genes(tt, 'GO:0000002', 'results')

GO2genes <- function(DGEobject, GO_query, resultsType){

#tryCatch(stopifnot(!exists("entrez_joinTable")),
#         error=stop("entrez and GO match table does not exist. Run GOsource() first."))

if(!exists("entrez_joinTable"))
    stop("Error: entrez-GO match table does not exist. Run GOsource() first.")


#entrez2symbol <- listless::list_to_data.frame(ww[EG.GO[EG.GO$go_id == GO_query, ]$gene_id] ) %>%
#                  distinct() %>% dplyr::rename(entrezgene=names,labels=values)

GO_entrez <- EG.GO[EG.GO$go_id == GO_query, ]$gene_id
entrez2symbol <- entrez_joinTable %>% filter(entrezgene %in% GO_entrez)

if(resultsType=="results"){
  df <- entrez2symbol %>%
  left_join(tibble::as_tibble(resultsExtract(DGEobject)), by="labels") %>% #problematic
  #left_join(tibble::as_tibble(resultsExtract(DGEobject)), by=c('entrezgene'="ENTREZID")) %>%
  na.omit() %>% distinct()
  }
else{df <- entrez2symbol %>%
  left_join(tibble::as_tibble(dgeExtract(DGEobject,"counts")), by="labels") %>% #problematic
  #left_join(tibble::as_tibble(dgeExtract(DGEobject,"counts")), by=c('entrezgene'="ENTREZID")) %>% #problematic
    na.omit() %>% distinct()
}

df %>% mutate(GOID = GO_query) %>%
  left_join(EG.GO[ , c(2,4)] %>% distinct(), by=c('GOID'='go_id')) %>%
  dplyr::select(GOID,Ontology,everything())

}












