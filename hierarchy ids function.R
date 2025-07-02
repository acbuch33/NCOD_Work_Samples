ids_list <- function(master_list, hierarchy){
  hierarchy <- hierarchy[`Number Of Employees` %nin% 0]
  
  colnames(hierarchy)[29]="1G"
  test=hierarchy$`10G`
  
  ids_levels=list()
  for(a in 1:length(master_list$groupid)){
    for(i in colnames(hierarchy)[colnames(hierarchy) %in% paste0(12:1,"G")]){
      if(master_list$groupid[a] %in% hierarchy[,get(i)]){
        ids_levels[[a]] = data.table(groupID = master_list$groupid[a], level = i)
      } 
    }
  }
  ids_levels <- rbindlist(ids_levels, use.names = FALSE)
  
  g_cols <- colnames(hierarchy)[colnames(hierarchy) %in% paste0(12:1,"G")]
  hier <- hierarchy[,..g_cols]
  
  # grabbing overall field roll ups ids - VHA, VBA
  field_rollups <- hierarchy[grepl("VISN", `9N`) | grepl("VBA Field Rollup", `10N`)]
  visns_districts <- unique(field_rollups$`9G`)
  admin_rollups <- unique(field_rollups$`10G`)
  ovr_admin <- unique(field_rollups$`11G`)
  
  # grabbing VACO, NCA, and VA ids
  admin <- hierarchy[grepl("Central", `11N`) | grepl("Cemetery", `11N`)]
  admin_ids <- unique(admin$`11G`)
  va <- unique(admin$`12G`)
  
  # combining all ids
  ab_ids <- c(master_list$groupid,visns_districts,admin_rollups,ovr_admin,admin_ids,va)
  
  non_master_list_ids <- c(visns_districts,admin_rollups,ovr_admin,admin_ids,va)
  non_master_list_df <- data.table(groupID = non_master_list_ids, level = NA)
  non_master_list_df$level = ifelse(non_master_list_ids %in% visns_districts, "9G",
                                    ifelse(non_master_list_ids %in% admin_rollups, "10G",
                                           ifelse(non_master_list_ids %in% ovr_admin | non_master_list_ids %in% admin_ids, "11G","12G")))
  
  all_ids <- rbind(ids_levels,non_master_list_df)
  
  # loop that gets all the unique ids for each groupID and puts them in a name list based on groupID
  ids <- list()
  for (i in 1:nrow(all_ids)){
    # i = 1
    
    level <- all_ids$level[i]
    id <- all_ids$groupID[i]
    
    id_col <- which(g_cols %in% level)
    cols <- g_cols[(id_col):length(g_cols)]
    test <- hier[get(level) %in% id, ..cols]
    filtered_ids <- unlist(lapply(test, unique))
    ids[[i]] <- unique(filtered_ids)
  }
  names(ids) <- all_ids$groupID
  return(ids)
}
