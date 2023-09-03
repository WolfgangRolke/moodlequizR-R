quiz36.samplesize.mean <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample(1:2, 1) 
   Test <- TRUE
   if(whichstory %in% 3:5) Test <- FALSE
   category <- paste("Inference for Mean / Sample size / Story = ", whichstory, 
   ifelse(Test, " (Test)", " (Interval) "))
   quizname <- "problem - "


   alternatives <- c("equal","less","greater")   
   alpha <- sample( c(1, 5, 10), 1)
   power <- sample( c(90, 95, 99), 1)
   if(whichstory == 1) {
         digits <- 2
         mu <- runif(1, 2, 3)
         par0 <- mu + sample(c(-1,1), 1) * runif(1, 0.2, 0.4)
         sigma <- runif(1, 1, 2)
   }   
   if(whichstory == 2) {
         digits <- 1 
         par0 <- runif(1,15,60)
         mu <- par0 - runif(1, 1, 5)
         sigma <- runif(1, 10, 20)
   }   
    sigma <- round(sigma, digits)   
    if(Test) {
        mu <- round(mu, digits)
        par0 <- round(par0, digits)    
        greaterlessorequal <- 1  
        if(runif(1)<0.66) greaterlessorequal <- ifelse(par0 > mu, 2, 3) 
        alt1 <- c("", ", alternative = \"less\"",", alternative = \"greater\"")[greaterlessorequal]
        alt2 <- c("different from ","less than ","more than ")[greaterlessorequal]
        alt3 <- c("equal","less","greater")[greaterlessorequal]    
        if(whichstory==2) {
          alt1 <- ", alternative = \"less\""
          alt3 <- "less"
        }    
        alttxt <- ifelse( alt3 == "equal", "", paste0(", alternative = \"", alt3, "\"")) 
        powtxt <- paste0(", power = ", power)
        alphatxt <-  ifelse( alpha == 5, "", paste0(", alpha = ", alpha/100))      
        command <- paste0("<p>&nbsp;<p>R command: t.ps(diff = ", par0, " - ", mu, ", sigma = ", sigma, 
          powtxt, alphatxt, alttxt, ")")
        n <- t.ps(diff = par0 - mu, sigma = sigma, power = power, 
            alpha = alpha/100, alternative = alt3, return.result = TRUE)        
        n[2] <- t.ps(diff = par0 - mu, sigma = sigma, power = power, 
            alternative = alt3, return.result = TRUE)    
        n[3] <- t.ps(diff = par0 - mu, sigma = sigma, power = power, 
            alpha = alpha/100, return.result = TRUE)    
        n[4] <- t.ps(diff = par0 - mu, sigma = sigma, power = power, return.result = TRUE)                        
        w <- c(100,  75, 75, 50)
    }        
    else {   
        cl <-  ifelse( alpha == 5, "", paste0(", conf.level = ", 100-alpha))
        command <- paste0("<p>&nbsp;<p>R command: t.ps( E = ", err,  ", sigma = ", sigma, cl, ")")
        n <- t.ps( E = err,  sigma =  sigma, conf.level = 100-alpha, 
            return.result = TRUE)
        n[2]<- t.ps( E = err,  sigma =  sigma, return.result = TRUE)
        w <- c(100, 50)
    }
    nm <- nm(n, w)               
    
    if(whichstory == 1) {
        qtxt <- paste0("A company has a machine that makes widgets. They have 
          a contract with a customer that specifies a mean length of the widgets of ", 
          par0," inches with a standard deviation of ", sigma, ". They will randomly 
          select some widgets and measure their lengths (in inches). Then they will 
          test at the ", alpha, "% level whether the length is ", alt2, par0, " inches.
           What sample size is needed for this test if the true length is ", mu,
            " and if they want the test to have a power of ", power, "%?")              
    }  
    if(whichstory == 2) {
         qtxt <- paste0("Last month a call center studied the time their representatives 
         spend on the phone with their customers and found that the mean length of the 
         calls was ", par0, " with a standard deviation of ", sigma, " minutes. 
         They have since made some changes to their customer service and want to know 
         if it has shortened the lengths of the calls. They are planning to do a
          hypothesis test at the ", alpha, "% level whether the length is less than ", 
          par0, " minutes. What sample size is needed for this test if the true 
          length is ", mu, " and if they want the test to have a power of ", power, "%?") 
   }
      
   qtxt <- paste0("<h5>", qtxt, "<p>n = ", nm, "</h5>") 
   atxt <- paste0("<h5>Required sample size is: n = ", n[1], 
     command, "</h5>")     
 
   list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)     
   
   
}   
