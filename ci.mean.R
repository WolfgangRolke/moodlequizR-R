ci.mean <- function(type, whichstory, doGraph=FALSE, doAssumptions=FALSE) {
   if(missing(type)) type <- sample(c("Data", "Summary"), 1)
   if(missing(whichstory)) whichstory <- sample( 1:5, 1) 
   category <- paste0("Inference for Mean / Confidence interval / ", 
      type, " / Story = ", whichstory, 
      ifelse(doAssumptions, " (with Assumptions)", " (without Assumptions)"))
   quizname <- "problem - "

   alpha <- sample(c(90,95,99), 1)
   al <- ifelse(alpha == 95, "", paste0(", conf.level = ", alpha) )
   n <- sample(30:70,1)
   if(whichstory == 1) {
        mu <- runif(1,10,100)
        digits <- 1
        sigma <- runif(1,mu/10,mu/4)
        x <- round(rnorm(n,mu,sigma), digits)
        est <- round(c(mean(x), sd(x)), digits+1)
   }   
   if(whichstory == 2) {
        mu <- runif(1,5,20)                
        digits <- 1  
        x <- round(abs(rnorm(n,mu,10)),digits)
        est <- round(c(mean(x), sd(x)), digits+1)        
  }   
  if(whichstory == 3) {
        mu <- runif(1,20,200)                
        digits <- 1        
        x <- round(abs(rnorm(n,mu,mu)),digits)
        est <- round(c(mean(x), sd(x)), digits+1)        
  }   
  if(whichstory == 4) {
        mu <- runif(1,2000,15000)             
        digits <- (-2)                            
        x <- round(abs(rnorm(n,mu,mu)),digits)
        est <- round(c(mean(x), sd(x)), digits+1)        
  }   
  if(whichstory == 5) {
        mu <- runif(1, 0, 0.3)             
        digits <- 1    
        x <- round(abs(runif(n, 2, 3.7) + mu),digits)
        est <- round(c(mean(x), sd(x)), digits+1)
  }
  plt64 <- ""
  if(type == "Data") {
      if(doGraph) 
        plt64 <- png64(one.sample.t(x, conf.level = alpha, return.graph = TRUE))
      int1 <- one.sample.t(x, conf.level = alpha, ndigit=digits+1, return.result = TRUE)
      int2 <- one.sample.t(x, ndigit=digits+1, return.result = TRUE)
      command <- paste0("<p>&nbsp;<p>R command: one.sample.t(x", al, 
              ", ndigit=", digits+1,  ")")      
  }
  else {  
      int1 <- one.sample.t(est[1], shat = est[2], n = n, 
              ndigit=digits+1, conf.level = alpha, return.result = TRUE)
      int2 <- one.sample.t(est[1], shat = est[2], n = n, 
              ndigit=digits+1, return.result = TRUE)
      command <- paste0("<p>&nbsp;<p>R command: one.sample.t( y = ", 
              est[1], ", shat = ", est[2], ", n = ", n, al, 
               ", ndigit=", digits+1, ")") 
  } 
  low.nm <- nm(c(int1[1], int1[1], int2[1], int2[1]), 
               w=c(100, 80, 75, 50), 
               eps=c(0, 0.01*int1[1], 0, 0.01*int2[1]))
  high.nm <- nm(c(int1[2], int1[2], int2[2], int2[2]), 
               w=c(100, 80, 75, 50), 
               eps=c(0, 0.01*int1[2], 0, 0.01*int2[2]))
  qassumptions <- ""
  if(doAssumptions) {
      if(type == "Data")
        qassumptions <- "<p>Assumptions: {1:MC:%100%ok~%0%not ok~%0%none~%0%can't tell~%0%don't know}"
      else 
        qassumptions <- "<p>Assumptions: {1:MC:%0%ok~%0%not ok~%0%none~%100%can't tell~%0%don't know}"
  } 
  aassumptions <- ""
  if(doAssumptions) {
      if(type == "Data")
        aassumptions <- "<p>The normal plot looks ok, so the assumptions are ok}"
      else 
        aassumptions <- "<p>No data, so assumptions can not be checked"
  }  
  if(whichstory == 1) {
      varname <- "lengths of widgets"
      if(type == "Data") {    
          qtxt <- paste0("A company has a machine that makes widgets. 
            They randomly select some widgets and measure their lengths (in inches). 
            The data is shown below. ")
      }
      else {                
          qtxt <- paste0("A company has a machine that makes widgets. 
            They randomly select ", n, " widgets and measure their lengths (in inches). 
            They find a mean length of ", est[1], " with a standard deviation of ", 
            est[2], ".")
      }
   }   
   if(whichstory == 2) {
      varname <- "length of calls"
      if(type == "Data") {    
          qtxt <- paste0("A call center wants to study the time their representatives 
                spend on the phone with their customers. They randomly time a number of 
                phone calls. The data is below.")
      }
      else {                    
          qtxt <- paste0("A call center wants to study the time their representatives 
                spend on the phone with their customers. They randomly time ", n, 
                " phone calls. They find a mean length of calls of ", est[1], 
                " minutes with a standard deviation of ", est[2])
      }
  }   
  if(whichstory == 3) {
      varname <- "sales amount"
      if(type == "Data") {
           qtxt <- paste0("A store wants to find out how much money people spend there. 
              They randomly collect some sales receipts. The data is below.")   
      }
      else {                                   
            qtxt <- paste0("A store wants to find out how much money people spend there. 
              They randomly collect ", n, " sales receipts and find a mean of ", 
              est[1], " dollars with a standard deviation of ", est[2])                
      }
  }   
  if(whichstory == 4) {
      varname <- "amount of payouts"
        if(type == "Data") {
             qtxt <- paste0("An insurance company is interested in the amount of money 
               they pay on average on insurance claims. They randomly select a number of 
               policies and find the payoutamounts. The data is below.") 
        }
        else {                
             qtxt <- paste0("An insurance company is interested in the amount of money 
                they pay on average on insurance claims. They randomly select ", n ,
                " policies and find a mean payouts of $", est[1], " with a standard 
                deviation of ", est[2])               
        }
  }   
  if(whichstory == 5) {
        varname <- "GPA"
        if( type == "Data") {
             qtxt <- paste0("A University wants to know  the GPAs of their students.        
                        They randomly select the GPA of ", n," students. The data is below.") 
        }
        else {                              
            qtxt <- paste0("A University wants to know  the GPAs of their students.        
                        They randomly select the GPA of ",n," students and find a 
                        mean GPA of ", est[1], " with a standard deviation of ", est[2]) 
        }                
  } 
  
  qtxt <- paste0(qtxt, "<p>&nbsp;<p>A ", alpha, "% confidence interval for the true ", 
    varname, " is given by (", low.nm, ", ", high.nm, ")", qassumptions)
  
  htxt <- "Did you check what confidence level is required?<p>Did you round correctly?" 
   
  atxt <- paste0("<p>&nbsp;<p>A ", alpha, "% confidence interval for the true ", 
    varname, " is given by (", int1[1], ", ", int1[2], ")", aassumptions)  
  
  qdata <- ""
  if(type=="Data") qdata <- paste0("<hr>", moodle.table(x))
  list(qtxt = paste0("<h5>", qtxt , "</h5>", qdata), 
        htxt = paste0("<h5>", htxt, "</h5>"),
        atxt = paste0("<h5>", atxt, command, "</h5>"), 
        category = category, quizname = quizname) }
