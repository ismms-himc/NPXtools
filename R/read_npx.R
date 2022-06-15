#'read_npx (read onlink npx file to summarizedexperiment)
#'@description  Read the NPXManager exported raw file in xlsx format to a summarizedexperiment.
#'              The uniportID, LOD, Missing Data freq., and Normalization method will be stored as listData in elementMetadata.
#'              The Assay(sample name), Plate ID, ctrl, QC.Warning, and QC Deviations will be stored as listData in the colData.
#'              The file path, panel, and software version info will be stored in metadata.
#'              read predicted Quant csv, raw Quant xlsx.
#'@param f a character string naming the file to read.
#'@param lot: optional parameter to record kit lot information.
#'@param startrow use 8 for reading npx data file as default, use 9 for quant data file.
#'@return a summarizedexperimet object.
#'        The uniportID, LOD, Missing Data freq., and Normalization method will be stored as listData in elementMetadata.
#'        The Assay(sample name), Plate ID, ctrl, QC.Warning, and QC Deviations will be stored as listData in the colData.
#'        The file path, panel, and software version info will be stored in metadata.
#'@export
#'@md
#'

read_npx <- function(f, lot = "default", startrow = 8, type = "NPX"){

  if(type != "NPX"){
    if(grepl("xlsx", f)){
      npx <- readxl::read_xlsx(f, sheet = 1, col_names = F)
    }else{
      npx <- read.csv(f, header = F)
    }
    n_col <- length(npx[which(npx[, 1] == "LLOQ"), ])
    npx[npx == ""] <- NA
  }
  else{
    npx <- readxl::read_xlsx(f, sheet = which(readxl::excel_sheets(f) == "NPX Data"), col_names = F)
    n_col <- length(npx[which(npx[, 1] == "LOD"), ])
  }


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
  unique_id <- paste("idx", 1:nrow(npx), sep = "_")

  rowData <- t(row_feat[, -1])%>%
    data.frame()%>%
    set_colnames(unlist(row_feat[, 1]))%>%
    set_rownames(.$Assay)
  colnames(rowData)[grep("LOD", colnames(rowData))] <- "LOD"
  rowData$LOD <- as.numeric(rowData$LOD)

  colData <- cbind(unique_id, Assay = npx$Assay, f_name = toString(f), npx_ctrl)%>%
    setNames(make.names(names(.), unique = TRUE))%>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("Ctrl")), .funs = as.numeric)%>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("QC.Deviation")), .funs = as.numeric)%>%
    data.frame(row.names = unique_id)

  if(type != "NPX"){
    npx <- cbind(unique_id, npx)%>%
      data.frame(row.names = unique_id) %>%
      dplyr::select(-unique_id, -Assay)%>%
      t()
  }else{
    npx <- cbind(unique_id, npx)%>%
      dplyr::mutate_at(.vars = dplyr::vars(!dplyr::matches("(unique_id|Assay)")),
                       .funs = as.numeric) %>%
      data.frame(row.names = unique_id) %>%
      dplyr::select(-unique_id, -Assay)%>%
      t()
  }

  format(npx, digits = 5)
  re <- SummarizedExperiment(colData = colData,
                             rowData = rowData,
                             assays = list(npx = npx),
                             metadata = list("software_version" = sw_version,
                                             "panel" = npx_panel,
                                             "file_name" = toString(f)))

  return(re)
}
