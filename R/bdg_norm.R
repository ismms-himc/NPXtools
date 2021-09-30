#'bdg_norm (Reference sample based normalization)
#'@description normalize by adjust samples' value per plate to make that
#'             the reference sample among all plates are the same. option to
#'             pick using median, max, mean
#'
#'@param bridge.ls list of bridge.summarizedexperiment objs using pull_bdg()
#'@param data.ls list of summarizedexperiment objs using read_npx().
#'               normalized assay slot will be added to each of the object in the list.
#'@param between.plate.method method to set the inter plate reference value of bridging samples
#'       using max can garantee no negative value of the normalized data
#'@param from_assay select assay slot to be normalized
#'@param save_assay name the assay slot for normalized data
#'@export
#'@md
#'
bdg_norm <- function(bridge.ls, data.ls, between.plate.method = "median",
                     from_assay = "npx", save_assay = "inlot_normed"){

  if(is.null(names(data.ls)) | (sum(names(bridge.ls) != names(data.ls)) != 0)){
    stop("list of bridges and data has to be named list!\n")
  }

  bridge <- cmb_npx_se(bridge.ls)

  #  count
  bridge.plate.mean <- cbind.data.frame(f_name = bridge$f_name,
                                        t(bridge@assays@data[[from_assay]]))%>%
    group_by(f_name)%>%
    summarize_all(.fun = mean, na.rm = T)


  bridge.median <- bridge.plate.mean%>%
    dplyr::select(-f_name)%>%
    summarize_all(.funs = between.plate.method, na.rm = T)


  # update adjust factor
  query <- bridge.plate.mean$f_name
  names(query) <- query

  # adjust factor count
  bridge.adj <- lapply(query, function(x){
    bridge.plate.mean[bridge.plate.mean$f_name == x ,-1] - unlist(bridge.median)
  })


  data.ls <- lapply(query, function(x){
    temp <- data.ls[[x]]
    temp@assays@data[[save_assay]] <- (temp@assays@data[[from_assay]] - unlist(bridge.adj[[x]]))%>%round(5)
    temp@elementMetadata$LOD <- as.numeric(temp@elementMetadata$LOD) - unlist(bridge.adj[[x]])
    temp
  })

  data.ls
}

