
# Inversed function of g(S(t)).
# See https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_lifetest_details08.htm
gInverseFunction <- function(conf.type = c('log','log-log','plain', 'logit')){
  
  conf.type = match.arg(conf.type)
  
  gLog <- function(x){
    exp(x)
  }
  
  gLogLog <- function(x){
    exp(-exp(x))
  }
  
  gPlain <- function(x){
    x
  }
  
  gLogit <- function(x){
    exp(x) / (1 + exp(x))
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
        gLogit
      }
    }
  }
}

