quiz30.hp.power <- function(which) {
   which <- sample(c("n", "mu", "sigma", "alpha"), 1)
   category <- paste0("Hypothesis Testing /  Power / change  ", which)
   quizname <- "problem - " 
   n0 <- sample(30:70, 1) 
   n1 <- n0 + sample(c(-1,1), 1)* round(n0/4)
   mu0 <- round(runif(1, 10, 100))
   sig0 <- round(runif(1, mu0/4, mu0/3), 1)
   sig1 <- round(ifelse(runif(1)<0.05, runif(1, sig0/3, sig0/1.4), runif(1, 1.5*sig0, 2*sig0)), 1)
   alpha1 <- sample(c(1, 10), 1)
   mu01 <- round(mu0 + runif(1, 1.5, 4)*sig0/sqrt(n0))   
   mu1 <- mu01 + sample(c(-1,1), 1)* round(runif(1, 2, 5), 1)
 
   pow <- t.ps(n=n0, diff=mu0-mu01, sigma=sig0, return.result=TRUE)
   if(which=="n")
      pow1 <- t.ps(n=n1, diff=mu0-mu01, sigma=sig0, return.result=TRUE) 
   if(which=="mu")
      pow1 <- t.ps(n=n0, diff=mu0-mu1, sigma=sig0, return.result=TRUE) 
   if(which=="sigma")
      pow1 <- t.ps(n=n0, diff=mu0-mu01, sigma=sig1, return.result=TRUE) 
   if(which=="alpha")
      pow1 <- t.ps(n=n0, diff=mu0-mu01, sigma=sig0, alpha=alpha1/100, return.result=TRUE) 
   if(pow>pow1) {
      qmc <- "{:MC:~=lower~higher~can't tell which}"
      atxt <- paste0("<h5>Power now = ", pow1,"%, so it is lower</h5>")
   }   
   else {
      qmc <- "{:MC:~lower~=higher~can't tell which}"
      atxt <- paste0("<h5>Power now = ", pow1,"%, so it is higher</h5>")
   }   
   qtxt <- paste0("<h5>A researcher is doing some experiment. He collects ", n0, " observations with 
          a standard deviation of ", sig0," . He then tests at the 5% level whether<p>
          H<sub>0</sub>: &mu; = ", mu0, " vs H<sub>a</sub>: &mu; &ne; ", mu0, "<p> Now if it turned out that the
          true population mean is ", mu01, " this test has a power of ", pow, "%.<p>")
   if(which=="n") 
          qtxt <- paste0(qtxt, "If instead he had done the experiment with a sample size
          of ", n1)            
   if(which=="mu") qtxt <- paste0(qtxt, "If instead the true mean had been ", mu1)   
   if(which=="sigma") qtxt <- paste0(qtxt, "If instead the standard deviation had been ", sig1)   
   if(which=="alpha") qtxt <- paste0(qtxt, "If instead the test had been done at the ", alpha1, "% level")    
      
   qtxt <- paste0(qtxt, ", the power of the test would have been ", qmc,"</h5>")
        
   list(qtxt = qtxt, 
        atxt = atxt, 
        category = category, quizname = quizname) 
} 
