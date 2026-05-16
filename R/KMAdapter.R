
KMAdapter <- R6::R6Class(
  "KMAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'km'
      
      self$fit <- fitKMCurve(self$spec$formula, self$data, self$spec$conf_type, self$spec$times)
      
      ## estimate
      self$n <- nrow(self$data)
      self$sample_id <- self$data[[self$spec$id_col]]
      self$km_times <- extractKaplanMeierTimes(self$fit)
      # build a plain named numeric so downstream c()/cov() see only the names
      # we care about and not the conf_type attribute carried on self$fit.
      est <- as.numeric(self$fit)
      names(est) <- names(self$fit)
      self$estimate <- est

      invisible(self)
    },
    
    refit = function(bdata){
      
      bfit <- fitKMCurve(self$spec$formula, bdata, self$spec$conf_type, 
                         times = self$get_km_times())
      
      bfit
    }
    
  )
)

