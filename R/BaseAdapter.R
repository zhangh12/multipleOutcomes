#' 
BaseAdapter <- R6::R6Class(
  "BaseAdapter",
  public = list(
    
    type = NULL,
    spec = NULL, ## passed through ... in jointCovariance
    data = NULL,
    fit  = NULL,
    km_times = NULL, ## only for engine "km"
    
    # Standardized parameter vector ("beta") per engine
    estimate = NULL,
    score = NULL,
    inv_hess = NULL,
    n = NULL,
    sample_id = NULL,
    
    initialize = function(spec, data) {
      self$spec <- spec
      self$data <- data
    },
    
    fit_model = function(){
      stop("fit_model() not implemented.")
    },
    
    get_engine = function(){
      self$spec$engine
    },
    
    get_n = function(){
      if(is.null(self$n)){
        stop('sample size is not calculated. Debug it. ')
      }
      self$n
    },
    
    get_km_times = function(){
      self$km_times
    },
    
    get_sample_id = function(){
      if(is.null(self$sample_id)){
        stop('sample ID is missing. Debug it. ')
      }
      self$sample_id
    },
    
    get_coef = function(){
      if(is.null(self$estimate)) {
        stop("estimate is not found. Did you call $fit_model()?")
      }
      self$estimate
    },
    
    get_dim = function(){
      length(self$estimate)
    },
    
    get_coef_name = function(){
      names(self$estimate)
    },
    
    get_data_index = function(){
      self$spec$data_index
    },
    
    # Bootstrap refit: MUST return named vector aligned to coef_names
    refit = function(bdata){
      stop("refit() not implemented.")
    },
    
    # ---- stubs you will implement ----
    get_score = function(){
      
      if(self$type != 'logrank'){
        score_epsilon <- 1e-6
        stopifnot((colSums(self$score) / nrow(self$score)) %>% abs %>% max < score_epsilon)
      }
      self$score
    },
    
    get_bread = function(){
      self$inv_hess
    },
    
    get_type = function(){
      self$type
    },
    
    print = function(){
      print(self$type)
      print(self$estimate)
      print(head(self$score))
      print(self$inv_hess)
      invisible(self)
    }
  )
)


