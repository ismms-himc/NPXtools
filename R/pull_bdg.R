#'pull_bdg (creat summarizedexperiment of only reference sampls from a list of summarizedexperiments by identifing patterns)
#'@description pull bridging sample from list of summarizeexperiment object by pattern string
#'             get bridging sample only before normalization by plate
#'
#'@param f_list list of summarizeexperiment objects
#'@param pattern sting pattern used for regex to identify samples
#'@param fields colData name from which to identify sample
#'@return list of summarizedexperiment obj
#'@export
#'@md
#'
pull_bdg <- function(f_list, pattern = "hd", fields = "Assay"){
  lapply(f_list, function(x){
    x[, grep(pattern, ignore.case = T, x[[fields]])]
  })
}
