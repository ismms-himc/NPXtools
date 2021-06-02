#' save_npx_csv (same summarizedexpermient to csv)
#'@rdname save_npx_csv
#'@description save summarizedexpermient to csv files
#'@param se npx summarizedexpermient object
#'@param pre_fix string of the file path
#'@param assay2save vector string of which assay slot to save
#'@export
#'@md
#'
save_npx_csv <- function(se, pre_fix, assay2save = c("npx_default", "npx_inlot_normed")){
  for (i in assay2save) {
    f_name <- paste0(pre_fix, i, ".csv")
    write.csv(se@assays@data[[i]], file = f_name, row.names = T)
  }
  f_name <- paste0(pre_fix, "_meta.csv")
  write.csv(data.frame(se@colData@listData), file = f_name, row.names = F)
  f_name <- paste0(pre_fix, "_rowfwature.csv")
  write.csv(data.frame(se@elementMetadata@listData), file = f_name, row.names = F)
}
