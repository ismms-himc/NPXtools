#' save_npx_csv (same summarizedexpermient to csv)
#'@rdname save_npx_csv
#'@description Save RowData, ColData and user selected assay from the summarizedexpermient to csv files.
#'The RowData exports the listData from elementMetadata to 'rowfeature.csv',
#'The ColData exports the listData from ColData to 'meta.csv',
#'assay exports the assay of choice to 'assay of choice.csv'.
#'@param se npx summarizedexpermient object
#'@param pre_fix a character string that will be sued to concatenate with '***.csv' to name a file to export.
#'@param assay2save vector of strings from the name list of the assay slot of the summarizedexpermient object.
#'@export
#'@md
#'
save_npx_csv <- function(se, pre_fix, assay2save = c("npx")){
  for (i in assay2save) {
    f_name <- paste0(pre_fix, i, ".csv")
    write.csv(se@assays@data[[i]], file = f_name, row.names = T)
  }
  f_name <- paste0(pre_fix, "meta.csv")
  write.csv(data.frame(se@colData@listData), file = f_name, row.names = F)
  f_name <- paste0(pre_fix, "rowfeature.csv")
  write.csv(data.frame(se@elementMetadata@listData), file = f_name, row.names = F)
}
