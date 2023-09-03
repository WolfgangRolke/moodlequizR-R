samplesize.prop <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample( 1:6, 1) 
   Test <- FALSE
   if(whichstory %in% 3:5) Test <- TRUE
   category <- paste("Inference for Proportion / Sample size / Story = ", whichstory, 
   ifelse(Test, " (Test)", " (Interval) "))
   quizname <- "problem - "
 
   alpha <- sample(c(90,95,99),1)
   power <- sample(c(90,95,99),1)
   error <- sample( c(0.01, 0.02, 0.05, 0.1), 1)                                 
       
   if(whichstory == 1) {
        command <- paste0("<p>&nbsp;<p>R command: prop.ps(E = ", error, ", conf.level = ", alpha, ")")
        n <- prop.ps(E = error, conf.level = alpha, return.result  = TRUE)
        n[2] <- prop.ps(E = error, return.result  = TRUE)
        w <- c(100, 75)
        qtxt <- paste0("A company has a machine that makes widgets. Each widget is either red or blue. 
            They want to randomly choose a number of widgets and then find a ", alpha,"% confidence interval 
            for the true percentage of red widgets with an error of ", error, ".")
   }                   
   if(whichstory == 2) {
        phat <- sample( 30:45, 1)
        command <- paste0("<p>&nbsp;<p>R command: prop.ps( phat = ", phat/100,  ", E = ", error, ", conf.level = ", alpha, ")")
        n <- prop.ps( phat = phat/100, E =  error, conf.level =  alpha, return.result  = TRUE)
        n[2] <- prop.ps(E =  error, conf.level =  alpha, return.result  = TRUE)
        n[3] <- prop.ps(E =  error, phat = phat/100, return.result  = TRUE)        
        n[4] <- prop.ps(E = error, return.result  = TRUE)
        w <- c(100, 75, 75, 50)
        qtxt <- paste0("Mr. Miller is running for major. In a poll some time ago he got ", phat, "% of the votes. He wants to know 
            how he is doing now, so he is planning to do a phone survey. He then wants to find a ", alpha,"% confidence interval for the true percentage of 
            people who will vote for him with an error of ", error, ".")              
   }                         
   if(whichstory == 3) {
        pi.null <- sample( 25:35, 1)
        phat <- pi.null + sample( 5:10, 1)
        command <- paste0("<p>&nbsp;<p>R command: prop.ps( phat = ", phat/100, ", pi.null = ",  pi.null/100, 
            ", power = ", power, ", alpha = ", 1-alpha/100, ", alternative = \"greater\")")
        n <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alpha =  1-alpha/100,  
            alternative = "greater", power =  power, return.result  = TRUE) 
        n[2] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alpha =  1-alpha/100,  power =  power, return.result  = TRUE)    
        n[3] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alternative = "greater",  power =  power, return.result  = TRUE)    
        n[4] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  power =  power, return.result  = TRUE)            
        w <- c(100, 75, 75, 50)
        qtxt <- paste0("In the past a certain flavour of ice cream of the Tasty Ice Cream company was chosen by only ", 
            pi.null, "% of their customers. They  are considering to change the flavor a bit, and they will do so if in the
            future at least ", phat, "% of the customers choose this new flavour. To find out they are planning 
            to do a taste test. Afterwards they will do a hypothesis test to see if the true percentage is now over ",
            phat, "%. They will do the test at the ", 100-alpha, "% level and they want the test to have a power of ", 
            power, "%.")
   }                         
   if(whichstory == 4) {
        pi.null <- sample( 20:30, 1)
        phat <- pi.null + sample(c(-1,1), 1)*sample( 5:10, 1)   
        command <- paste0("<p>&nbsp;<p>R command: prop.ps( phat = ", phat/100, ", pi.null = ",  pi.null/100, 
            ", power = ", power, ", alpha = ", 1-alpha/100, ")")
        n <- prop.ps( phat = phat/100, pi.null =  pi.null/100, alpha = 1-alpha/100,  power =  power, return.result  = TRUE)            
        n[2] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  power =  power, return.result  = TRUE)            
        w <- c(100, 75)
        qtxt <- paste0("An insurance company wants to know what percentage of their policies are for cars.
            In general about ", pi.null, "% of the policies are for cars. To find out what is is for their company planning 
            to randomly select n of their policies. Then they will do a hypothesis test to see if the true percentage is 
            different from ", phat, "%. They will do the test at the ", 100-alpha, "% level and they want the test to have a power of ", 
            power, "%.") 
   }                         
   if(whichstory == 5) {
        pi.null <- sample( c(60:80)/2, 1)
        phat <- pi.null - sample( c(10:20)/2, 1)   
        command <- paste0("<p>&nbsp;<p>R command: prop.ps( phat = ", phat/100, ", pi.null = ",  pi.null/100, ", power = ", power, 
              ", alpha = ", 1-alpha/100, ", alternative = \"less\")")
        n <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alpha =  1-alpha/100,  
            alternative = "less", power =  power, return.result  = TRUE)    
        n[2] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alpha =  1-alpha/100,  power =  power, return.result  = TRUE)    
        n[3] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  alternative = "less",  power =  power, return.result  = TRUE)    
        n[4] <- prop.ps( phat = phat/100, pi.null =  pi.null/100,  power =  power, return.result  = TRUE)            
        w <- c(100, 75, 75, 50)
   
        qtxt <- paste0("At some university historically ", pi.null, "% of the students fail a certain course. To lower the
            the failure rate the university has created a tutoring center. They want to know whether this has improved the
            situation, and for that they will randomly select n students. Then they will do a hypothesis test 
            at the ", 100-alpha, "% level. If the failure rate is now ", phat, "% they want 
            the test to have a power of ", power, "%.")  
  } 
   if(whichstory == 6) {
        phat <- sample( 20:30, 1)
        command <- paste0("<p>&nbsp;<p>R command: prop.ps( phat = ", phat/100,  ", E = ", error, ", conf.level = ", alpha, ")")
        n <- prop.ps(phat = phat/100, E =  error, conf.level =  alpha, return.result  = TRUE)
        n[2] <- prop.ps(E =  error, conf.level =  alpha, return.result  = TRUE)
        n[3] <- prop.ps(E =  error, phat = phat/100, return.result  = TRUE)        
        n[4] <- prop.ps(E = error, return.result  = TRUE)
        w <- c(100, 75, 75, 50)
        qtxt <- paste0("A biologist wants to estimate the percentage of young among an animal population. It is known that
          in general for this animal ", phat, "% are young.
          He will visit n locations and find the percentage of young animals at each of them. Then he will 
          find a ", alpha,"% confidence interval for the true percentage of young animals with 
          an error of ", error, ".")    
   }
   qtxt <- paste0("<h5>", qtxt,  "<p>&nbsp;<p>Required sample size n = ", nm(n, w), "</h5>") 
   htxt <- ""                             
   atxt <- paste0("<h5>Required sample size is n = ", n[1], command, "</h5>")    
   list(qtxt = qtxt, htxt=htxt, atxt = atxt, category = category, quizname = quizname)         
} 
