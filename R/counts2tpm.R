#' Convert counts to transcripts per million (TPM).
#'
#' Convert a numeric matrix of features (rows) and conditions (columns) with
#' raw feature counts to transcripts per million.
#'
#'    Lior Pachter. Models for transcript quantification from RNA-Seq.
#'    arXiv:1104.3889v2
#'
#'    Wagner, et al. Measurement of mRNA abundance using RNA-seq data:
#'    RPKM measure is inconsistent among samples. Theory Biosci. 24 July 2012.
#'    doi:10.1007/s12064-012-0162-3
#'
#' @param DGEobject A DGE List that contains raw feature counts (i.e.
#'  n. fragments assigned to each gene), in DGE$counts.
#' @param featureLength A numeric vector with feature lengths.
#' @param meanFragmentLength A numeric vector with mean fragment lengths.
#' @export
#' @return tpm A numeric matrix normalized by library size and feature length.
counts2tpm <- function(DGEobject, featureLength, meanFragmentLength) {

  mycounts <- DGEobject[["counts"]]
  # Ensure valid arguments.
  stopifnot(length(featureLength) == nrow(mycounts))
  stopifnot(length(meanFragmentLength) == ncol(mycounts))

  # Compute effective lengths of features in each library.
  effLen <- do.call(cbind, lapply(1:ncol(mycounts), function(i) {
    featureLength - meanFragmentLength[i] + 1
  }))

  # Exclude genes with length less than the mean fragment length.
  idx <- apply(effLen, 1, function(x) min(x) > 1)
  mycounts <- mycounts[idx,]
  effLen <- effLen[idx,]
  featureLength <- featureLength[idx]

  # Process one column at a time.
  tpm <- do.call(cbind, lapply(1:ncol(mycounts), function(i) {
    rate = log(mycounts[,i]) - log(effLen[,i])
    denom = log(sum(exp(rate)))
    exp(rate - denom + log(1e6))
  }))

  # Copy the row and column names from the original matrix.
  colnames(tpm) <- colnames(mycounts)
  rownames(tpm) <- rownames(mycounts)
  return(tpm)
}
