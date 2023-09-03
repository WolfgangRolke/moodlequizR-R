regression.bestmodel <- function(whichstory=1) {
    require(mvtnorm)
          
    if(whichstory==1)
      category <- "Regression / Best Model / quadratic" 
    if(whichstory==2)
      category <- "Regression / Best Model / log(y) vs x"
    quizname <- "problem - " 
    g <- function() {
      if(whichstory==1) {
        outQ <- slr(y, x, polydeg=2, show.tests=TRUE, return.result=TRUE)
        outC <- slr(y, x, polydeg=3, show.tests=TRUE, return.result=TRUE)      
        qtxt <- paste0("<p>The best model for predicting the ", varnames[2], 
        " is<p>
        {:MC:~Linear~y vs log(x)~log(y) vs x~log(y) vs log(x)~=Quadratic~Cubic}")
        atxt <- paste0("Linear model and all log transform models have bad residual vs fits plts.
              <p>p value of quadratic term in quadratic model= ", outQ[7], " < 0.05
              <p>p value of cubic term in cubic model= ", outC[9], " > 0.05")
      }
      if(whichstory==2) {
        qtxt <- paste0("<p>The best model for predicting the ", varnames[2], 
        " is<p>
        {:MC:~Linear~y vs log(x)~=log(y) vs x~log(y) vs log(x)~Quadratic~Cubic}")
        atxt <- "The only model with a good residual vs. fits plot is log(y) vs. x"
        
      }        
      list(qtxt=qtxt, atxt=atxt)                     
    }                    
    n <- sample( 50:80, 1)
    if(whichstory == 1)  {
        x <- 1:n
        y <- round((x + x^2/5+rnorm(n,0,10))/10)
        y[y<1] <- 1
    }
    if(whichstory == 2)  {
        n <- sample(40:50, 1)
        x <- 20:n
        y <- round(exp(x/5+rnorm(length(x))))
        y[y<1] <- 1
     }  
    if(whichstory == 1) {
         varnames <- c("Day", "Faults")
         data <- data.frame(Day=x, Faults=y)
         qtxt <- paste0("A company has a machine that makes widgets. For ", n, " consecutive days
            they count the number of faulty widgets.")
    }  
    if(whichstory == 2) { 
         varnames <- c("Sugar","Microbes")
         data <- data.frame(Time=x, Microbes=y)
         qtxt <- paste0("A biologist wants to study the relationship between
            the number of microbes that grow in a petry dish and the amount of time
            (in hours). ")
    }
   
   g1 <- g() 
   list(qtxt = paste0("<h5>", qtxt, g1$qtxt,"</h5><hr>", moodle.table(data)), 
        atxt = paste0("<h5>", g1$atxt,"</h5>"), 
        category = category, quizname = quizname)     
}   
