#' Generate bootstrap dataset. 
#' When missing data presents, dataset is split into groups. Patients in 
#' the same group have missing on the same covariates. Bootstrap is carried 
#' out in each of the groups
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' 
#' @param data a data frame to be sampled from.
#'
#' @keywords internal
#' @noRd
sampleWithReplacement <- function(data){
  
  uid <- 
    lapply(
      seq_along(data), 
      function(idx){
        df <- data[[idx]]
        df$index <- idx
        df[, c('pid', 'index'), drop = FALSE]
      })
  uid <- do.call(rbind, uid) %>% 
    group_by(.data$pid) %>% 
    summarise(group = paste0(paste0('data-', sort(unique(.data$index))), collapse = ','))
  
  unique_dataset_group <- sort(unique(uid$group))
  selected_uid <- list()
  for(i in seq_along(unique_dataset_group)){
    group <- unique_dataset_group[i]
    ids <- uid$pid[uid$group == group]
    selected_uid[[group]] <- sample(ids, size = length(ids), replace = TRUE)
  }
  
  bdata <- rep(list(NULL), length(data))
  for(group in names(selected_uid)){
    dat_ids <- as.integer(unlist(strsplit(gsub('data-', '', group), ',')))
    for(dat_id in dat_ids){
      if(!any(duplicated(data[[dat_id]]$pid))){
        rownames(data[[dat_id]]) <- data[[dat_id]]$pid
        tmp_data <- data[[dat_id]][selected_uid[[group]], , drop = FALSE]
        tmp_data$pid <- rownames(tmp_data)
        bdata[[dat_id]] <- rbind(bdata[[dat_id]], tmp_data)
        rm(tmp_data)
      }else{
        tmp <- data[[dat_id]][!duplicated(data[[dat_id]]$pid), , drop = FALSE]
        rownames(tmp) <- tmp$pid
        tmp <- tmp[selected_uid[[group]], 'pid', drop = FALSE]
        for(i in seq_along(selected_uid[[group]])){
          sid <- selected_uid[[group]][i]
          new_sid <- rownames(tmp)[i]
          tmp_data <- data[[dat_id]] %>% dplyr::filter(.data$pid %in% sid)
          tmp_data$pid <- new_sid
          bdata[[dat_id]] <- rbind(bdata[[dat_id]], tmp_data)
          rm(tmp_data, sid, new_sid)
        }
      }
    }
  }
  
  bdata
  
}


