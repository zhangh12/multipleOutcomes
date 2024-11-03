
#' g(S(t)), a transformed estimate of survival probability.
#' See https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_lifetest_details08.htm
gFunction <- function(conf.type = c('log','log-log','plain', 'logit')){
  
  conf.type = match.arg(conf.type)
  
  gLog <- function(x){
    log(x)
  }
  
  gLogLog <- function(x){
    log(-log(x))
  }
  
  gPlain <- function(x){
    x
  }
  
  gLogit <- function(x){
    log(x / (1 - x))
  }
  
  gArcsin <- function(x){
    asin(sqrt(x))
  }
  
  if(conf.type %in% 'log'){
    gLog
  }else{
    if(conf.type %in% 'log-log'){
      gLogLog
    }else{
      if(conf.type %in% 'plain'){
        gPlain
      }else{
        glogit
      }
    }
  }
}

