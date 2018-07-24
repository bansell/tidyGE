

#' Source a join table that enables linking entrezgene IDs with gene symbols (ALIAS) and accession numbers (ACCNUM).
#' @param species A species identifier ("Hs", or "Mm")
#' @importFrom magrittr %>%
#' @importFrom AnnotationDbi toTable
#' @export
#' @return Creates a join table for the required species, which is called by GO2geneID().



GOsource <- function(species){

if(species =="Mm" & !requireNamespace("org.Mm.eg.db", quietly=TRUE)){
stop("bioconductor package org.Mm.eg.db needed for this function to work. Please install it.",
call. = FALSE)}

if(species=="Hs" & !requireNamespace("org.Hs.eg.db", quietly=TRUE)){
stop("bioconductor package org.Hs.eg.db needed for this function to work. Please install it.",
call. = FALSE)}

if(species=="Mm"){library(org.Mm.eg.db)}
if(species=="Hs"){library(org.Hs.eg.db)}
  #DB <- paste("org", species, "eg", "db", sep = ".")

  #GO2ALLEGS <- paste("org", species, "egGO2ALLEGS", sep = ".")
  EG.GO <- AnnotationDbi::toTable(get(paste("org", species, "egGO2ALLEGS", sep = ".")))
  assign("EG.GO",EG.GO,envir = globalenv())

  #GO2ALLEGS uses entrezgene; need to convert ensembl/refseq ID to entrezgene as step1

  #extract refseq --> entrezgene from ACCNUM

  ACCNUM_DB <- paste("org", species, "egACCNUM", sep = ".")
  ALIAS2EG_DB <- paste("org", species, "egALIAS2EG", sep = ".")

  ww <- as.list(AnnotationDbi::toTable(get(ACCNUM_DB)))
  ww_table <- data_frame('entrezgene' = ww$gene_id, 'labels' = ww$accession) %>% mutate(source='ACCNUM');
  #assign("ww_table", ww_table, envir = globalenv())

  zz <- as.list(AnnotationDbi::toTable(get(ALIAS2EG_DB)))
  zz_table <- data_frame('entrezgene' = zz$gene_id, 'labels' = zz$alias_symbol) %>% mutate(source="ALIAS") %>% distinct();
  #assign("zz_table", zz_table, envir = globalenv())

  EG <- data_frame('entrezgene' = ww$gene_id, 'labels' = ww$gene_id) %>% mutate(source="EG") %>% distinct()

  entrez_joinTable <- rbind(ww_table,zz_table,EG)
  assign('entrez_joinTable',entrez_joinTable, envir=globalenv())
  rm(ww); rm(zz); rm(EG)

select <- dplyr::select #to avoid namespace conflicts
}

