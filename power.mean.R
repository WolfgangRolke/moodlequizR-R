power.mean <- function(whichstory) {
    if(missing(whichstory)) whichstory <- sample(1:5, 1) 
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
   if(whichstory == 3) {
         muNull <- round(runif(1, 25, 35), 2)
         muAlt <- round(muNull + runif(1, 3, 5), 2)
         sigma <- round(runif(1, 30, 50), 2)
         alt.com <- ", alternative=\"greater\""
         alt.f <- "greater"

         qtxt <- paste0("A store has just done a major add compaign on TV, and they 
            want to know what effect it has had. Before the compaign the mean sales was ",
            muNull, "$ with a standard deviation of ", sigma, "$.They will randomly collect ", n, " 
            sales receipts. Then they will do a hypothesis test at the ", 100*alpha, 
            "% level whether the sales amounts are now higher. If in fact the mean 
            amount is now ", muAlt, "$, what is the power of of the test?") 
                    
   }
   if(whichstory == 4) {
         n <- n+50 
         muNull <- round(runif(1, 3000, 6000), -2)
         muAlt <- round(muNull - runif(1, 500, 1500), -2)
         sigma <- round(runif(1, 3000, 5000), -2)
         alt.com <- ", alternative=\"less\""
         alt.f <- "less"
         qtxt <- paste0("10 years ago the mean payout on insurance claims by an 
            insurance company was ", muNull, "$ with a standard deviation of ", sigma, 
            "$. They want to know if the payout amounts are lower today, so they are 
            planning on randomly selecting ", n, " claims.
            Then they will do a hypothesis test at the ", 100*alpha, 
            "% level whether the claim amounts are now lower. If in fact the mean 
            amount is now ", muAlt, "$, what is the power of of the test?") 
   }                            
   if(whichstory == 5) {
         n <- 5*sample(10:20, 1)
         muNull <- round(runif(1, 2.6, 2.9), 1)
         muAlt <- round(muNull - runif(1, 0.1, 0.2), 1)
         sigma <- round(runif(1, 0.7, 0.9), 1)
         alt.com <- ", alternative=\"less\""
         alt.f <- "less"
         qtxt <- paste0("The chancellor of some university has read in an article that the
         mean GPA at universities like hers it ", muNull, " with a standard deviation of ", 
         sigma, ". She suspects that the mean GPA at his school is lower. So she randomly selects ",
         n, " GPAs. Then she will do a hypothesis test at the ", 100*alpha, 
         "% level whether the GPAs at her school are now lower. If in fact the mean 
         GPA is ", muAlt, ", what is the power of of the test?") 
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
