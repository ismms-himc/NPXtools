#'intensity_norm (Reference sample based normalization)
#'@description normalize by adjust samples' value per plate to make that
#'             the between plates median intensity are the same. option to
#'             pick using median, max, mean
#'
#'@param data.ls list of summarizedexperiment objs using read_npx().
#'               normalized assay slot will be added to each of the object in the list.
#'@param between.plate.method method to set the inter plate reference value of bridging samples
#'@param from_assay select assay slot to be normalized
#'@param save_assay name the assay slot for normalized data
#'@export
#'@md
#'


intensity_norm <- function(data.ls, save_assay = "int_normed" , from_assay = "npx", between.plate.method = "mean"){
  query <- names(data.ls)
  names(query) <- query
  
  median.ls <- lapply(query, function(x){
    apply(data.ls[[x]]@assays@data[[from_assay]], 1, "median", na.rm = T)
  })
  
  all.mean <- median.ls%>%
    do.call(what = "rbind")%>%
    data.frame()%>%
    summarize_all(between.plate.method, na.rm = T)
  
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
