#'cmb_npx_se (Merge a list of npx summarizedexperiment to one)
#'@description combine multiple npx summarizedexperiment obj
#'             only common analyt would reimain in the merged object,
#'             the LOD stored in the merged object will be the mean from all
#'             individual npx objects
#'@param se_list list of npx summarizedexperiment obj
#'rowData will be removed, common colData
#'@return combined summarizedexperiment obj
#'@export
#'@md
#'
cmb_npx_se <- function(se_list){
  # get common columns
  com_col <- unlist(lapply(se_list, function(x) {x@colData%>%colnames()}))%>%table()
  com_col <- names(com_col)[com_col == length(se_list)]

  rowdata_list <- sapply(se_list, function(x){
    as.numeric(x@elementMetadata@listData$LOD)
  })

  Uniprot.ID <- as.character(se_list[[1]]@elementMetadata[[1]])

  se_list <- lapply(se_list, function(x){
    x@colData <- x@colData[match(com_col, names(x@colData@listData))]
    x@elementMetadata@listData <- data.frame()
    x
  })
  temp <- se_list[[1]]
  for (i in 2 : length(se_list)) {
    temp <- cbind(temp, se_list[[i]])
  }
  rowData(temp) <- data.frame(Uniprot.ID = Uniprot.ID,
                              Analyt = make.names(rownames(se_list[[1]])),
                              LOD = as.numeric(rowMeans(rowdata_list, na.rm = T)))
  return(temp)
}
