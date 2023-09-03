quiz35.power.mean <- function(whichstory) {
    if(missing(whichstory)) whichstory <- sample(1:2, 1) 
    category <- paste("Inference for Mean / Power / Story = ", whichstory)
    quizname <- "problem - "
 
    alpha <- sample(c(0.01, 0.05, 0.1),1)
    n <- 10*sample(5:50, 1)     
    
    if(whichstory == 1) {
        muNull <- round(runif(1, 2, 3), 2)
        muAlt <- round(muNull + runif(1, 0.2, 0.4), 2)
        sigma <- round(runif(1, 1, 2), 2)
        alt.com <- ", alternative=\"greater\""
        alt.f <- "greater"

        qtxt <- paste0("A company has a machine that makes widgets. They have 
          a contract with a customer that specifies a mean length of the widgets of ", 
          muNull," inches with a standard deviation of ", sigma, " inches. They will randomly 
          select ", n, " widgets and measure their lengths. Then they will 
          test at the ", 100*alpha, "% level whether the length is actually higher.
          If in fact the mean length is ", muAlt, " inches, what is the power of of the test?")              
    }  
    if(whichstory == 2) {
         muNull <- round(runif(1, 15, 60), 1)
         muAlt <- round(muNull - runif(1, 1, 5), 1)
         sigma <- round(runif(1, 10, 20), 1)
         alt.com <- ", alternative=\"less\""
         alt.f <- "less"

         qtxt <- paste0("Last month a call center studied the time their representatives 
         spend on the phone with their customers and found that the mean length of the 
         calls was ", muNull, " minutes with a standard deviation of ", sigma, " minutes. 
         They have since made some changes to their customer service and want to know 
         if it has shortened the lengths of the calls. They are planning to randomly select ",
         n, " calls and then do a hypothesis test at the ", 100*alpha, "% level whether the 
         calls are now shorter. If in fact the mean length is now ", muAlt, " minutes, 
         what is the power of of the test?")              
   }
      
  command <- paste0("<p>&nbsp;<p>R command: t.ps(
              n = ", n, ", diff = ", muAlt, "-", muNull, 
             ", sigma = ", sigma,  
             ifelse(alpha==0.05, "", paste(", alpha = ", alpha)), 
             alt.com, ")")
   pow <- t.ps(n=n, diff=muAlt-muNull, sigma=sigma, alpha=alpha, 
        alternative=alt.f, return.result  = TRUE)
   pow[2] <- t.ps(n=n, diff=muAlt-muNull, sigma=sigma, alpha=alpha, return.result  = TRUE)     
   w <- c(100, 75)
   
   qtxt <- paste0("<h5>", qtxt,  "<p>&nbsp;<p>The power of the test is ", nm(pow, w), "%</h5>")                              
   atxt <- paste0("<h5>The power of the test is ", pow[1], "%", command, "</h5>")    
   list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)            
   
}   
