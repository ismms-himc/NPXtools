#'bdg_norm_multi (Reference sample based normalization)
#'@description normalize by adjust samples' value per plate to make that
#'             the reference sample among all plates are the same. option to
#'             pick using median, max, mean
#'
#'@param bridge.str vector of common strings to identify sets of bridging samples
#'@param data.ls list of summarizedexperiment objs using read_npx().
#'               normalized assay slot will be added to each of the object in the list.
#'@param between.plate.method method to set the inter plate reference value of bridging samples
#'       using max can garantee no negative value of the normalized data
#'@param from_assay select assay slot to be normalized
#'@param save_assay name the assay slot for normalized data
#'@export
#'@md
#'
bdg_norm_multi <- function(bridge.str, data.ls, between.plate.method = "median",
                     from_assay = "npx", save_assay = "normed"){
  if(
    sapply(bridge.str, function(x){
      lapply(data.ls, function(y){
        sum(is.na(grepl(x, y$Assay)))
      })
    })%>%unlist()%>%sum() != 0
  ){
    stop("not all bridging samples exist in each plate!")
  }

  names(bridge.str) <- bridge.str
  adj.ls <- lapply(bridge.str, function(x){
    bridge <- pull_bdg(data.ls, pattern = x, fields = "Assay")%>%
      cmb_npx_se()
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

    return(bridge.adj)
  })

  adj.mean <- adj.ls[[names(adj.ls)[1]]]
  if(length(adj.ls) > 1){
    for (i in names(adj.mean)) {
      for (j in 2 : length(adj.ls)) {
        adj.mean[[i]] <- adj.mean[[i]] + adj.ls[[j]][[i]]
      }
      adj.mean[[i]] <- adj.mean[[i]]/length(adj.ls)
    }
  }

  query <- names(adj.mean)
  names(query) <- query

  data.ls <- lapply(query, function(x){
    temp <- data.ls[[x]]
    temp@assays@data[[save_assay]] <- (temp@assays@data[[from_assay]] - unlist(adj.mean[[x]]))%>%round(5)
    temp@elementMetadata$LOD <- as.numeric(temp@elementMetadata$LOD) - unlist(adj.mean[[x]])
    temp
  })

  data.ls

}
