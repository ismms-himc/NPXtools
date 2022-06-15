#'cmb_npx_se (Merge a list of npx summarizedexperiment to one)
#'@description combine a list npx summarizedexperiment objects to a single summarizedexperiment object.
#'             if the multiple object includes different panels, only common analytes would be kept in the combined object.
#'             the listData in elementMetadata slot will be expanded to include all objects combined with surfix of '_Plate.ID',
#'             a additional LOD column stores the mean from all individual npx objects.
#'@param se_list a list of npx summarizedexperiment objects.
#'@return a combined summarizedexperiment object.
#'@export
#'@md
#'
cmb_npx_se <- function(se_list){
  # get common columns
  com_col <- unlist(lapply(se_list, function(x) {x@colData%>%colnames()}))%>%table()
  com_col <- names(com_col)[com_col == length(se_list)]

  com_row <- rownames(se_list[[1]])
  for (i in 2 : length(se_list)) {
    com_row <- intersect(com_row, rownames(se_list[[i]]))
  }

  se_list <- lapply(se_list, function(x){
    x[match(com_row, rownames(x)), ]
  })

  rowdata_list <- lapply(se_list, function(x){
    temp <- data.frame(x@elementMetadata@listData)
    colnames(temp) <- paste(colnames(temp), x$Plate.ID[1], sep = "_")
    temp
  })
  row_data <- rowdata_list[[1]]
  colnames(row_data)[1] <- "Analyte"

  for (i in 2 : length(rowdata_list)) {
    row_data <- cbind(row_data, rowdata_list[[i]][ ,-c(1,2)]) #Analyt Assay
  }

  colnames(row_data) <- make.names(colnames(row_data), unique = T)
  row_data$LOD <- rowMeans(row_data[ , grepl("LOD", colnames(row_data))], na.rm = T)

  se_list <- lapply(se_list, function(x){
    x@colData <- x@colData[match(com_col, names(x@colData@listData))]
    x@elementMetadata@listData <- data.frame()
    x
  })
  temp <- se_list[[1]]
  for (i in 2 : length(se_list)) {
    temp <- cbind(temp, se_list[[i]])
  }
  rowData(temp) <- row_data
  return(temp)
}
