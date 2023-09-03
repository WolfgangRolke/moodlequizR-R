quiz19.percentile <- function() {
   category <- paste0(" Summary Statistics / Percentiles" )
   quizname <- "problem - " 
   
   n <- sample(101:149, 1)
   k <- sort(sample(5*2:18, 2))
   x <- round(rnorm(n, 50, 10), 1)
   p <- round(quantile(x, k/100), 2)
   
   qtxt <- paste0("For the data set below the 
      <p> ", k[1], "<sup>th</sup> percentile is ", nm(c(p[1],p[1]), w=c(100, 80), eps=c(0.1, 1)), 
      "<p> ", k[2], "<sup>th</sup> percentile is ", nm(c(p[2],p[2]), w=c(100, 80), eps=c(0.1, 1))) 
   
   htxt <- ""
   atxt <- paste0("round(quantile(x, c(", k[1]/100, ", ", k[2]/100, ")), 2) = ", p)
      
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>", 
        ifelse(is.null(x), "", paste("<hr>", moodle.table(x)))),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
