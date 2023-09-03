quiz18.emprule <- function(isworking) {
   if(missing(isworking)) isworking <- sample(c(TRUE, FALSE), 1) 
   category <- paste0(" Summary Statistics / Empirical Rule / ", 
      ifelse(isworking, " is working", "is not working"))
   quizname <- "problem - " 
   
   n <- sample(200:250, 1)
   x <- round(rnorm(n, 20, 5),1 )
   mmc <- mc(c("is", "is not"), c(100, 0))
   if(!isworking) {
      l <- mean(x)+2*sd(x)
      I <- sample(1:n, size=25)
      x[I] <- round(runif(length(I), l+15, l+30), 1)
      mmc <- mc(c("is", "is not"), c(0, 100))
   }   
   mu <- round(mean(x), 2)
   sig <- round(sd(x), 2)
   y <- x[x>=mu-2*sig]
   m <- length(y[y<=mu+2*sig])
   qtxt <- paste0("Consider the data set below. According to the <nolink>empirical rule</nolink> ",
   nm(round(0.95*n),eps=0.99), " of the observations should be in the interval (", 
   nm(mu-2*sig, eps=0.1), " , ", nm(mu+2*sig, eps=0.1), ") 
   <p>Actually ", nm(m), " of the observations are in the interval, so the 
   empirical rule ", mmc$qmc, " working here.") 
   htxt <- ""
   atxt <- paste0("95% of ", n, " is ", round(0.95*n, 1),   
        "<p>Mean = ", mu, ", std = ", sig, " so the <nolink>empirical rule</nolink> interval is 
        (", mu-2*sig, ", ", mu+2*sig, ").
        <p>", m, " observations are in the interval, so the rule is ", 
        ifelse(isworking, "", " not "), " working here.")

   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(x)),
        htxt <- "",
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
