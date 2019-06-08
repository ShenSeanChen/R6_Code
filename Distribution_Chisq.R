library(distr6)
library(R6)

Chisquared <- R6::R6Class("Chisquared", inherit = Distribution, lock_objects = F)
Chisquared$set("public","name","Chisquared")
Chisquared$set("public","short_name","Chisq")
Chisquared$set("public","traits",list(type = PosReals$new(zero = T),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))

Chisquared$set("public","properties",list(support = PosReals$new(zero = T),
                                           distrDomain = PosReals$new(zero = T),
                                           symmetry  = "asymmetric"))

Chisquared$set("private",".pdf",function(x1, ncp = 0, log = FALSE){
  dchisq(x1, self$getParameterValue("df"), ncp, log)
})

Chisquared$set("private",".cdf",function(x1, ncp = 0, lower.tail = TRUE, log.p = FALSE){
  pchisq(x1, self$getParameterValue("df"), ncp, lower.tail, log.p)
})

Chisquared$set("private",".quantile",function(p, ncp = 0, lower.tail = TRUE, log.p = FALSE){
  qchisq(p, self$getParameterValue("df"), ncp, lower.tail, log.p)
})

Chisquared$set("private",".rand",function(n, ncp = 0){
  rchisq(n, self$getParameterValue("df"), ncp)
})

Chisquared$set("public","expectation",function(){
  self$getParameterValue("df")
})

Chisquared$set("public","var",function(){
  (self$getParameterValue("df"))*2
})

Chisquared$set("public","skewness",function() return(sqrt(8/self$getParameterValue("df"))))

Chisquared$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(12/self$getParameterValue("df"))
  else
    return(3 + 12/self$getParameterValue("df"))
})

Chisquared$set("public","entropy",function(base = exp(1)){
  k <- self$getParameterValue("df")
  log(2*gamma(k/2), base) - (1-k/2)*digamma(k/2) + k/2
})

Chisquared$set("public", "mgf", function(t){
  k <- self$getParameterValue("df")
  if(t < 1/2)
    return((1-2*t)^(-k/2))
  else
    return(0)
})

Chisquared$set("public", "cf", function(t){
  k <- self$getParameterValue("df")
  return((1-2i*t)^(-k/2))
})

Chisquared$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1, lower.tail = FALSE, log.p)
})

Chisquared$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

Chisquared$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})

Chisquared$set("public","mode",function() {
  k <- self$getParameterValue("df")
  return(max(k-2, 0))})

Chisquared$set("private",".parameters", NULL)

Chisquared$set("public","initialize",function(df = NULL, decorators = NULL,...){

  df.bool = FALSE

  if(is.null(df)){
    message("df is missing. df = 1 parameterisation used.")
    df = 1
  } else {
    message("df is provided. df parameterisation used.")
    df.bool = TRUE
    df = df
  }


  private$.parameters <- ParameterSet$new(id = list("df"), value = list(1),
                                          lower = list(0), upper = list(Inf),
                                          class = list("numeric"),
                                          settable = list(df.bool),
                                          description = list("degree of freedom"))

  if(!is.null(df)) self$setParameterValue(list(df = df))

  super$initialize(decorators = decorators,...)
  invisible(self)
})
