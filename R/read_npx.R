#'read_npx (read onlink npx file to summarizedexperiment)
#'@description  read npx raw file to create a summarizedexperiment
#'              uniportID and LOD in rowfeatures
#'              file name, ctrl and qc warning are in the colData
#'@export
#'@param f a string file path
#'@param lot: optional parameter to record lot information
#'@param startrow 8 for npx, 9 for quant
#'@return a summarizedexperimet object.
#'        rowData is the feature and ctrl deviation,
#'        colData is the unique_id and subject id given by the operator
#'@export
#'@md
#'
#read_npx <- function(f, lot = "default", startrow = 8){
#
#  f_id <- system(paste("md5", f), intern=TRUE)
#  f_id <- substr(f_id, (nchar(f_id)-10), nchar(f_id))
#
#  npx <- readxl::read_xlsx(f, col_names = F)
#  sw_version <- npx[1, 2]
#  npx_panel <- npx[3, 2]
#  ctrl_col_idx <- grep("(Ctrl|Plate ID|QC)", ignore.case = T, npx[4, ])
#
#
#  npx_ctrl <- npx[startrow : nrow(npx), ctrl_col_idx]
#  npx_ctrl <- npx_ctrl[1 : (min(which(is.na(npx_ctrl[ , 1]))) - 1), ]
#
#  colnames(npx_ctrl) <- npx[4, ctrl_col_idx]
#
#  row_feat <- rbind(npx[4 : 5, -ctrl_col_idx])
#
#  npx <- npx[startrow : nrow(npx), -ctrl_col_idx]
#  row_feat <- rbind(row_feat, npx[(min(which(is.na(npx[ , 1]))) + 1): nrow(npx), ])
#
#  npx <- npx[1 : (min(which(is.na(npx[, 1]))) - 1), ]
#  colnames(npx) <- row_feat[1, ]
#  unique_id <- paste(f_id, 1:nrow(npx), sep = "_")
#
#  rowData <- t(row_feat[, -1])%>%
#    data.frame()%>%
#    set_colnames(unlist(row_feat[, 1]))%>%
#    set_rownames(.$Assay)
#  colnames(rowData)[grep("LOD", colnames(rowData))] <- "LOD"
#
#  colData <- cbind(unique_id, Assay = npx$Assay, f_name = toString(f), npx_ctrl)%>%
#    setNames(make.names(names(.), unique = TRUE))%>%
#    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("Ctrl")), .funs = as.numeric)%>%
#    data.frame(row.names = unique_id)
#  npx <- cbind(unique_id, npx)%>%
#    dplyr::mutate_at(.vars = dplyr::vars(!dplyr::matches("(unique_id|Assay)")),
#                     .funs = as.numeric) %>%
#    data.frame(row.names = unique_id) %>%
#    dplyr::select(-unique_id, -Assay)%>%
#    t()
#  re <- SummarizedExperiment(colData = colData,
#                             rowData = rowData,
#                             assays = list(npx = npx),
#                             metadata = list("software_version" = sw_version,
#                                             "panel" = npx_panel,
#                                             "file_name" = toString(f)))
#
#  return(re)
#}
#


read_npx <- function(f, lot = "default", startrow = 8, type = "NPX"){

  f_id <- paste("c", f)
  f_id <- substr(f_id, (nchar(f_id)-10), nchar(f_id))

  if(type != "NPX"){
    npx <- readxl::read_xlsx(f, sheet = 1, col_names = F)
  }
  else{
    npx <- readxl::read_xlsx(f, sheet = which(readxl::excel_sheets(f) == "NPX Data"), col_names = F)
  }

  #---determin cols
  n_col <- length(npx[which(npx[, 1] == "LOD"), ])
  npx <- npx[, 1:n_col]
  sw_version <- npx[1, 2]
  npx_panel <- npx[3, 2]
  ctrl_col_idx <- grep("(Ctrl|Plate ID|QC)", ignore.case = T, npx[4, ])


  npx_ctrl <- npx[startrow : nrow(npx), ctrl_col_idx]

  colnames(npx_ctrl) <- npx[4, ctrl_col_idx]

  row_feat <- rbind(npx[4 : 5, -ctrl_col_idx])

  npx <- npx[startrow : nrow(npx), -ctrl_col_idx]
  row_feat <- rbind(row_feat, npx[(min(which(is.na(npx[ , 1]))) + 1): nrow(npx), ])

  npx <- npx[1 : (min(which(is.na(npx[, 1]))) - 1), ]
  npx_ctrl <- npx_ctrl[1 : nrow(npx), ]

  colnames(npx) <- row_feat[1, ]
  unique_id <- paste(f_id, 1:nrow(npx), sep = "_")

  rowData <- t(row_feat[, -1])%>%
    data.frame()%>%
    set_colnames(unlist(row_feat[, 1]))%>%
    set_rownames(.$Assay)
  colnames(rowData)[grep("LOD", colnames(rowData))] <- "LOD"

  colData <- cbind(unique_id, Assay = npx$Assay, f_name = toString(f), npx_ctrl)%>%
    setNames(make.names(names(.), unique = TRUE))%>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("Ctrl")), .funs = as.numeric)%>%
    data.frame(row.names = unique_id)
  npx <- cbind(unique_id, npx)%>%
    dplyr::mutate_at(.vars = dplyr::vars(!dplyr::matches("(unique_id|Assay)")),
                     .funs = as.numeric) %>%
    data.frame(row.names = unique_id) %>%
    dplyr::select(-unique_id, -Assay)%>%
    t()
  re <- SummarizedExperiment(colData = colData,
                             rowData = rowData,
                             assays = list(npx = npx),
                             metadata = list("software_version" = sw_version,
                                             "panel" = npx_panel,
                                             "file_name" = toString(f)))

  return(re)
}
