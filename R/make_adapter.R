
make_adapter <- function(spec, data_list){
  
  stopifnot(inherits(spec, "jc_spec"))
  dat <- data_list[[spec$data_index]]
  
  if(!is.data.frame(dat)){
    stop("data[[data_index]] must be a data.frame")
  }
  
  if(inherits(spec, "jc_spec_glm")){
    return(GLMAdapter$new(spec, dat))
  }
  
  if(inherits(spec, "jc_spec_coxph")){
    return(CoxphAdapter$new(spec, dat))
  }
  
  if(inherits(spec, "jc_spec_logrank")){
    return(LogRankAdapter$new(spec, dat))
  }
  
  if(inherits(spec, "jc_spec_gee")){
    return(GEEAdapter$new(spec, dat))
  }
  
  stop("Unsupported spec type: ", paste(class(spec), collapse = ", "))
}
