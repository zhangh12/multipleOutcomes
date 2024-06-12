
parseGeeConfig <- function(gee_family){
  
  config <- strsplit(gee_family, '[^a-zA-Z\\-_]+', perl = TRUE) %>% unlist()
  
  if(!('gee' %in% config)){ # not a GEE family
    return(NULL)
  }
  
  valid_corstr <- c('independence', 'fixed', 'stat_M_dep', 'non_stat_M_dep', 'exchangeable', 'AR-M', 'unstructured')
  valid_family <- c('gaussian', 'binomial', 'poisson', 'Gamma', 'quasi')
  
  if(!any(valid_corstr %in% config)){
    corstr <- 'independence'
  }else{
    corstr <- intersect(config, valid_corstr)
  }
  
  if(!any(valid_family %in% config)){
    family <- 'gaussian'
  }else{
    family <- intersect(config, valid_family)
  }
  
  id <- setdiff(config, c('gee', corstr, family))
  if(length(id) == 0){
    message(str_glue('id is missing in {gee_family}'))
    return(NULL)
  }
  
  if(length(id) > 1){
    message(str_glue('Cannot identify id from {gee_family}'))
    return(NULL)
  }
  
  list(family_str = str_glue('gee+{id}+{family}+{corstr}'), id = id, family = family, corstr = corstr)
  
}