quiz16.zscore.find <- function() {
   category <- paste0(" Summary Statistics / z score / Find z score" )
   quizname <- "problem - "
    
   mn <- round(runif(1, 70, 75), 1)
   s <- round(runif(1, 6, 8), 1)   
   x <- rnorm(1, mn, s)
   y <- round(runif(1, mn-2.5*s, mn+2.5*s), 1)
   z <- round((y-mn)/s, 1) 
   qtxt <- paste0("Your score in some exam was ", y, " points. Overall the class had a 
   mean score of ", mn, " with a standard deviation of ", s, ". So your z score in this exam 
   was ", nm(z, eps=0.1)) 
   
   htxt <- "What is the formula of the z score?"
   atxt <- paste0("round((",y,"-",mn,")/",s,",1) = ", z)
      
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"), 
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
