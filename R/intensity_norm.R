#'intensity_norm (Reference sample based normalization)
#'@description Given the assumption that all olink runs' samples are randomized,
#'intensity_norm does normalization (batch correction) by making analyte-wise intra-plate-median equal among all runs.
#'
#'@param data.ls list of summarizedexperiment objects using read_npx().
#'               normalized assay slot will be added to each of the object in the list.
#'@param between.plate.method method to set the inter plate reference value of the analyte-wise intra-plate-medians.
#'default as 'mean', it is optional to use 'median'.
#'@param from_assay select assay slot which assay is to be normalized.
#'@param save_assay name the assay slot to store the intensity normalized data.
#'@return a list of summarizedexperimet objects with new assay slot stores the intensity normalized data.
#'@export
#'@md
#'


intensity_norm <- function(data.ls, save_assay = "normed" , from_assay = "npx", ref_batch = NULL, between.plate.method = "mean"){

  if(!is_null(ref_batch) & ( !isTRUE(ref_batch %in% names(data.ls)) | (length(ref_batch) > 1))){
    stop("set a single ref_batch from names(data.ls)!")
  }

  if(is.null(names(data.ls))){
    stop("set names using names(data.ls) = data.ls!")
  }
  query <- names(data.ls)
  names(query) <- query

  median.ls <- lapply(query, function(x){
    apply(data.ls[[x]]@assays@data[[from_assay]], 1, "median", na.rm = T)
  })

  if(is_null(ref_batch)){
    all.mean <- median.ls%>%
      do.call(what = "rbind")%>%
      data.frame()%>%
      summarize_all(between.plate.method, na.rm = T)
  }else{
    all.mean <- median.ls[names(median.ls) == ref_batch][[1]]
  }

  adj.ls <- lapply(query, function(x){
    median.ls[[x]] - all.mean
  })

  lapply(query, function(x){
    temp <- data.ls[[x]]
    temp@assays@data[[save_assay]] <- apply(temp@assays@data[[from_assay]], 2, FUN = function(y) y - unlist(adj.ls[[x]]))
    temp@elementMetadata$LOD_int <- as.numeric(temp@elementMetadata$LOD) - unlist(adj.ls[[x]])
    temp
  })
}
