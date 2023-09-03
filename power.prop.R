power.prop <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample(1:5, 1) 
   category <- paste("Inference for Proportion / Power / Story = ", whichstory)
   quizname <- "problem - "

   alpha <- sample(c(0.01, 0.05, 0.1),1)
   n <- 10*sample(5:50, 1)     

   if(whichstory == 1) {
        pi.null <- 50
        phat <- sample(55:65, 1)
        alt.com <- ", alternative=\"greater\""
        alt.f <- "equal"       
        qtxt <- paste0("A company has a machine that makes widgets. Half of the widgets 
        are supposed to be red and the other half blue. They want to see whether this is true 
        and are planning an experiment as follows: They will randomly collect ", n, " widgets and the 
        then test at the ", 100*alpha, "% level whether the percentage is equal to 50%. 
        If in reality ", phat, "% of the widgets are red, what will be the power of this test?")
   }                   
   if(whichstory == 2) {
        pi.null <- sample( 30:45, 1)
        phat <- pi.null - sample(5:10, 1)
        alt.com <- ", alternative=\"less\""
        alt.f <- "less"       
        qtxt <- paste0("Mr. Miller is running for major. In a poll some time ago he got ", 
          pi.null, "% of the votes. He has reason to believe he would currently get fewer votes.
          He is planning to do a phone survey to see if this is true. If he randomly selects ", 
          n, " voters and asks them their opinion, what would be the power of the test at the ",
           100*alpha, "% level whether his current percentage is as low as ", phat, "%?")              
   }                         
   if(whichstory == 3) {
        pi.null <- sample( 25:35, 1)
        phat <- pi.null + sample( 5:10, 1)
        alt.com <- ", alternative=\"greater\""
        alt.f <- "greater"       
        qtxt <- paste0("In the past a certain flavour of ice cream of the Tasty Ice Cream 
            company was chosen by ", pi.null, "% of their customers. They  have done
            an advertisement campaign. They are going to do a survey of ", n, " customers and 
            test if the percentage is now higher. If the test is done at the ", 100*alpha, 
            "% level and if the percentage is actually now ", phat, "%, what
            would be the power of the test?")
   }                         
   if(whichstory == 4) {
        pi.null <- sample( 20:30, 1)
        phat <- pi.null + sample( 5:10, 1)  
        alt.com <- ", alternative=\"greater\""
        alt.f <- "greater"               
        qtxt <- paste0("An insurance company wants to know what percentage of their 
            policies are for cars. In the past about ", pi.null, "% of the policies were for cars. 
            To find out what is now the company is planning 
            to randomly select ", n, " of their policies. Then they will do a hypothesis 
            test to see if the true percentage is as high as ", phat, "%. 
            They will do the test at the ", 100*alpha, "% level. What would be the power of 
            the test?") 
   }                         
   if(whichstory == 5) {
        pi.null <- sample( c(60:80)/2, 1)
        phat <- pi.null - sample( c(10:20)/2, 1)   
        alt.com <- ", alternative=\"less\""
        alt.f <- "less"   
        qtxt <- paste0("At some university historically ", pi.null, "% of the students 
           fail a certain course. To lower the the failure rate the university has 
           created a tutoring center. They want to know whether this has improved the
           situation, and for that they will randomly select ", n, " students. 
           Then they will do a hypothesis test at the ", 100*alpha, "% level. 
           If the failure rate is now ", phat, "%, what would be the power of the test?")  
  } 
  command <- paste0("<p>&nbsp;<p>R command: prop.ps(
              n = ", n, ", pi.null=", pi.null/100, 
             ", phat = ", phat/100,  
             ifelse(alpha==0.05, "", paste(", alpha = ", alpha)), 
             alt.com, ")")
   pow <- prop.ps(n=n, pi.null=pi.null/100, phat=phat/100, alpha=alpha, 
        alternative=alt.f, return.result  = TRUE)
   pow[2] <- prop.ps(n=n, pi.null=pi.null/100, phat=phat/100, alpha=alpha, return.result  = TRUE)     
   w <- c(100, 75)

   
   qtxt <- paste0("<h5>", qtxt,  "<p>&nbsp;<p>The power of the test is ", nm(pow, w), "%</h5>")                              
   atxt <- paste0("<h5>The power of the test is ", pow[1], "%", command, "</h5>")    
   list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)         
} 
