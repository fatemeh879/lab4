

linreg <- setRefClass("linreg", fields = list( coefficients="matrix",
                                               fittedvalues="matrix",
                                               residuals="matrix",
                                               df="numeric",
                                               residualvariance="numeric",
                                               varianceregcoe="matrix",
                                               tvalues="matrix",
                                               pvalue="matrix",
                                               formula="character",
                                               data="character"
                                               )
                      
                               
)


linreg$methods(initialize = function( formula,data){
  X<<- model.matrix(formula,data)
  y<<- all.vars(formula)[1]
  Y<<- data[,y]
  n=nrow(data)
  p=ncol(data)
  .self$formula <- deparse(formula)
  .self$data <- deparse(substitute(data))
  
  .self$coefficients <<- solve(t(X) %*% X) %*% t(X) %*% Y
  .self$fittedvalues <- X %*% coefficients
  .self$residuals <- Y - fittedvalues
  .self$df <- n-p
  .self$ residualvariance<- as.numeric(t(residuals)%*% residuals/df)
   varmat<- residualvariance * solve(t(X) %*% X)
   
  .self$varianceregcoe <- matrix(diag(varmat), nrow=nrow(coefficients))
  rownames(varianceregcoe)<<- rownames(varmat)
  .self$tvalues<- coefficients/sqrt(varianceregcoe)
  .self$pvalue<- pt(coefficients,df)
  
},
#show= function(){
  #cat("call:\n")
  #cat("linreg(formula= ", formula, ", data =" ,data,")  \n  \n")
  #cat(colnames(matrix(coefficients)),"\n",matrix(coefficients),"\n"  )
  #cat(coefficients,"\n")
show = function(){
  cat("call:\n")
  cat("linreg(formula =", formula, ", data = ", data, ")\n\n")
  cat(rownames(coefficients), "\n")
  cat(as.vector(coefficients), "\n")
  
},
residual= function(){
  return(as.vector(residuals))
},
pred= function(){
  return(fittedvalues)
},
coef= function(){
  coef<- as.vector(coefficients)
  
  return(coef)
}

 
)
data("iris")
 test <- linreg(Petal.Length~Species, iris)
 ttt <- linreg(Sepal.Length~Species, iris)
 print(test)
 test$residual()
 test$fittedvalues
 test$pred()
 test$coef()
 test$coefficients
 
 
 