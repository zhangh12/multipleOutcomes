
GEEAdapter <- R6::R6Class(
  "GEEAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'gee'
      
      id <- as.factor(self$data[[self$spec$id_col]])
      dat <- self$data
      dat$id <- id
      
      suppressMessages(
        capture.output(
          self$fit <- gee(formula = self$spec$formula, 
                          id = id, 
                          data = dat, 
                          family = self$spec$family, 
                          corstr = self$spec$corstr,
                          R = self$spec$R, 
                          b = self$spec$b, 
                          Mv = self$spec$Mv), 
          file = NULL
        )
      )
      
      ## estimate
      self$estimate <- stats::coef(self$fit)
      
      ## score
      self$score <- self$fit$score
      
      rownames(self$score) <- self$fit$score_id
      self$sample_id <- rownames(self$score)
      self$n <- nrow(self$score)
      
      ## inv_hess
      self$inv_hess <- solve(-self$fit$hess / self$n)
      
      invisible(self)
    },
    
    refit = function(bdata){
      
      ## multipleOutcomes::gee() is modified from gee::gee(), where id 
      ## is very tricky. It actually want it be numeric or factor, but has never 
      ## been mentioned in its manual. To see this, search code line: 
      ## id <- as.double(id)
      ## in R/gee.R in this package or R/ugee.R in the gee package. 
      ## That line of code could be in trouble if id is a character vector. 
      id <- as.factor(bdata[[self$spec$id_col]])
      bdata$id <- id
      
      suppressMessages(
        capture.output(
          bfit <- gee(formula = self$spec$formula, 
                      id = id, 
                      data = bdata, 
                      family = self$spec$family, 
                      corstr = self$spec$corstr,
                      R = self$spec$R, 
                      b = self$spec$b, 
                      Mv = self$spec$Mv), 
          file = NULL
        )
      )
      
      stats::coef(bfit)
    }
    
  )
)

