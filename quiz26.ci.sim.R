quiz26.ci.sim <- function() {
   category <- "Confidence Intervals / Simulation"
   quizname <- "problem - " 
   B <- 10
   Low <- rep(0, B)
   High <- rep(0, B)
   good <- rep(TRUE, B)
   makebad <- sample(1:B, 1)
   err <- 1.645*20/sqrt(50) + runif(10, -0.5, 0.5)
   for(i in 1:B) {
   mu <- 100 
      if(i==makebad) {
          if(runif(1)<0.5) {
            Low[i] <- mu - 2*err[i] - runif(1, 0, 3)
            High[i] <- mu - runif(1, 0, 3)
          }
          else {
            Low[i] <- mu + runif(1, 0, 3)
            High[i] <- mu + 2*err[i] + runif(1, 0, 3)
          }
      }
      else {
          Low[i] <- mu - err[i]/1.6 - runif(1, 0, 2)
          High[i] <- mu + err[i]/1.6 + runif(1, 0, 2)
      }
   }
   qtxt <- "Below are 10 90% confidence intervals from 10 samples, 
      each of them has 50 observations from 
      a normal distribution with mean 100 and standard deviation 20.<p>    
      For each interval decide whether it is a good one or a bad one.<hr>"
      for(i in 1:10) {
      qtxt <- paste0(qtxt, "(", round(Low[i], 1), ", ", round(High[i], 1), 
           ")&nbsp;&nbsp;&nbsp;", ifelse(i==makebad, 
              "{11:MC:~Good~=Bad}", 
              "{1:MC:~=Good~Bad}"),"<br>")
   }      
   htxt <- "What is the value of the parameter that is supposed to be in the interval?"
   atxt <- "<h5>"
   for(i in 1:10) {
      atxt <- paste0(atxt, "(", round(Low[i], 1), ", ", round(High[i], 1), 
           ")&nbsp;&nbsp;&nbsp;",ifelse(i!=makebad, "Good", "Bad"),"<br>")
   }
        
   list(qtxt = paste("<h5>", qtxt, "</h5>"), 
        htxt = paste("<h5>", htxt, "</h5>"),
        atxt = paste("<h5>", atxt, "</h5>"),
        category = category, quizname = quizname) 
} 
