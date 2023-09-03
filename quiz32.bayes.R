quiz32.bayes <- function() {
   category <- "Bayesian Statistics"
   quizname <- "problem - "
   
   bayes <- function (x, n, loc, rg) {
     prior <- function(x) dnorm(x, loc, rg/4)/
                 diff(pnorm(c(0,100), loc, rg/4))              
     mx <- 1
     posterior <- function(p) {dbinom( x, n, p/100)*prior(p)/mx}
     mx <- integrate(posterior, 0, 100)$value     
     ef <- function(x) {x*posterior(x)}
     round(integrate(ef, 0, 100)$value, 1)
   }
   
   n <- sample(20:30, 1)
   p <- sample(15:25, 1)
   x <- sample(0:4, 1)
   rg <- sample(5:10, 1)
   est <- bayes(x, n, p, 2*rg)
   qtxt <- paste0("<h5>A company has just received a shipment of electronic parts. 
      Generally the number of faulty parts is ", p,"% but it can be as low as ", 
      p-rg, "% and as high as ", p+rg, "%.<p>&nbsp;<p>
      They randomly select ", n, " parts and find ", x, " that are faulty. 
      The Bayesian estimate of the percentage of faulty parts is ",
      nm(est, eps=0.999), "%</h5>")
      
   atxt <- paste0("<h5>run command ibayesprop()<p>&nbsp;<p>
      Use Location and Range, set most likely value to ", p, " and Range to ", 2*rg,
       " (from ", p-rg, " to ", p+rg, 
       ").<p>Sample Size: ", n, " Number of Successes: ", x, "<p>
      The Bayesian estimate of the percentage of faulty parts is ", est, "%</h5>")
   
   list(qtxt = qtxt,  atxt = atxt, 
        category = category, quizname = quizname) }
