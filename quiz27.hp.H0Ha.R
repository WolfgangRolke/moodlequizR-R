quiz27.hp.H0Ha <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample(1:4, 1)
   category <- paste0(" Hypothesis Testing  / H0-Ha / Story = ", whichstory)
   quizname <- "problem - " 
   
   hle <- c("equal to", "not equal to", "higher than", "lower than")
   sgns <- c("=", "&ne;", ">", "<")
   params <- c("mean", "median", "percentage", "proportion", "probability"
                  , "standard deviation", "correlation", "association", "other")                  
   w <- cbind(c(100, 0, 0, 0), c(0, 100, 0, 0), c(0, 0, 100, 0), c(0, 0, 0, 100))
   
   if(whichstory==1) {
      mu0 <- 10*sample(5:15, 1)
      mu <- mu0 + sample(c(-1, 1), 1)*sample(50:100, 1)/10
      
   }
   if(whichstory==2) {
      mu0 <- sample(5:15, 1) + sample(0:10, 1)/10
      mu <- round(mu0 + sample(c(-1, 1), 1)*sample(50:100, 1)/30, 1)
   }
   if(whichstory==3) {
      mu0 <- sample(10:20, 1) + sample(0:10, 1)/10
      mu <- round(mu0 + sample(c(-1, 1), 1)*sample(50:100, 1)/30, 1)
   }
   if(whichstory==4) {
      mu0 <- sample(5:10, 1) + sample(0:10, 1)/10
      mu <- round(mu0 + sample(c(-1, 1), 1)*sample(50:100, 1)/50, 1)
   }   
      
   if(mu<mu0) {
      alt <- sample(hle[c(2, 4)], 1)
      mmc <- mc(sgns, w = w[, ifelse(alt==hle[2], 2, 4)])
      correct.sgn <- ifelse(alt==hle[2], "&ne;", "<")            
   }    
   else {
      alt <- sample(hle[c(2, 3)], 1)       
      mmc <- mc(sgns, w = w[, ifelse(alt==hle[2], 2, 3)])
      correct.sgn <- ifelse(alt==hle[2], "&ne;", ">")
   }      
   
   if(whichstory==1)   
      qtxt <- paste0("A psychologist has given an IQ test to a group of ", sample(50:70), 
          "  people. They got a mean score of ", mu, " with a standard deviation of ", 
          sample(500:1580, 1)/100, ". This test is desiged to yield a mean score of ", 
          mu0, ". The psychologist wants to know whether the mean IQ in her group is ",
          alt, " ", mu0, ". So she tests")
 
   if(whichstory==2)       
      qtxt <- paste0("A biologist is counting the number of trees in a forrest. He checks ",
          sample(50:70), " randomly chosen areas of a standard size and finds a mean 
          number of trees of ", mu, " with a standard deviation of ", 
          sample(50:158, 1)/10, ". In this type of forrest one would expect to see ", mu0, 
          " trees per area. The biologist wants to know whether the mean number of trees 
          in this forrest is ", alt, " ", mu0, ". So he tests")
          
   if(whichstory==3)       
      qtxt <- paste0("An engineer wants to know the number of faulty parts made by a machine
          per day. He checks the parts on ", sample(20:30), " randomly chosen days and finds 
          a mean  ", mu, " faulty parts with a standard deviation of ", 
          sample(50:150, 1)/10, ". Generally the machine should make ", mu0, 
          " faulty parts per day. He wants to know whether the mean number of  
          faulty parts is ", alt, " ", mu0, ". So he tests")

   if(whichstory==4)       
      qtxt <- paste0("A teacher wants to know the number of spelling errors made by her pupils.         
          She checks ", sample(40:50), " randomly chosen essays and finds a mean  ", mu, 
          " with a standard deviation of ", sample(50:158, 1)/10, " errors. Generally in 
          this type of essay the mean number of errors is ", mu0, 
          ". She wants to know whether the mean number of errors this time is ", 
          alt, " ", mu0, ". So she tests")
          
   qtxt <- paste0(qtxt, "<p>&nbsp;<p>H<sub>0</sub>: Mean&nbsp;", mc(sgns, c(100, 0, 0, 0)), 
          "&nbsp;", nm(mu0), "&nbsp;vs H<sub>a</sub>: Mean&nbsp;", mmc, "&nbsp;", nm(mu0)) 
   atxt <- paste0("H<sub>0</sub>: Mean = ", mu0, 
          " vs H<sub>a</sub>: Mean&nbsp;", correct.sgn, "&nbsp;", mu0)
          
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>"),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
