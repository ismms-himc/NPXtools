#'pull_bdg (creat summarizedexperiment of only reference sampls from a list of summarizedexperiments by identifing patterns)
#'@description subset a npx summarizeexperiment object by filter a column in colData.
#'
#'@param f_list a single or a list of summarizeexperiment objects.
#'@param pattern sting pattern used for regex to identify columns to be subset.
#'@param fields a colData name from which is used for subsetting.
#'@return a single or a list of subset summarizeexperiment objects.
#'@export
#'@md
#'
pull_bdg <- function(f_list, pattern = "hd", fields = "Assay"){
  lapply(f_list, function(x){
    x[, grep(pattern, ignore.case = T, x[[fields]])]
  })
}
