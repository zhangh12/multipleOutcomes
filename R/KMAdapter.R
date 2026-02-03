
KMAdapter <- R6::R6Class(
  "KMAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'km'
      
      self$fit <- fitKMCurve(self$spec$formula, self$data, self$spec$conf_type)
      
      ## estimate
      self$n <- nrow(self$data)
      self$sample_id <- self$data[[self$spec$id_col]]
      self$km_times <- extractKaplanMeierTimes(self$fit)
      attributes(self$fit) <- NULL
      self$estimate <- self$fit
      
      
      invisible(self)
    },
    
    refit = function(bdata){
      
      bfit <- fitKMCurve(self$spec$formula, bdata, self$spec$conf_type, 
                         times = self$get_km_times())
      bfit
    }
    
  )
)

