simulation <- function(which) {
   if(missing(which)) which <- sample(1:3, 1)
   category <- paste0(" Simulations  / Which = ", which)
   quizname <- "problem - " 
   
   if(which==1) {
      mu <- sample(20:30, 1)
      sig <- sample(1:5, 1)
      t <- round(runif(1, mu+0.5*sig, mu+2*sig), 1)
      y <- round(1-pnorm(t, mu, sig), 4)
      qtxt <- paste0("Using <nolink>simulation</nolink> we find the probability that an observation
         from a normal distribution with  mean ", mu, " and standard deviation ", sig, 
         " is greater than ", t, " is ", nm(y, eps=3*sqrt(y*(1-y)/B)))
      atxt <- paste0("B <- 10000<br>x <- rnorm(B, ", mu, ", ", sig, ")<br>
          length(x[x>", t, "])/B")   
   }
   if(which==2) {
      n <- c(sample(500:600, 1), sample(700:800, 1))
      if(runif(1)>0.5) n <- n[2:1]
      y <- round(dhyper(0, n[1], n[2], 3), 4)
      qtxt <- paste0("A box contains ", n[1], " white balls and ", n[2], " black balls.
        We pick 3 balls simultaneously. Using <nolink>simulation</nolink> we find the probability that none
        of them is white to be ", nm(y, eps=3*sqrt(y*(1-y)/B))) 
   
      atxt <-  paste0("B <- 10000<br>y <- 0<br>for(i in 1:B) {<br>
         &nbsp;&nbsp;x <- sample(c(\"White\", \"Black\"), size=3, 
           replace=TRUE, prob=c(", n[1], ", ", n[2], "))<br>
         &nbsp;&nbsp;if(length(x[x==\"White\"])==0) y <- y+1<br>
        }<br>
        y <- y/B")
   }
   
   if(which==3) {
      k <- round(runif(1, 10, 20), 1)
      m <- round(runif(1, k/2, 1.5*k))
      y <- round( (1-k/100)^m, 4)
      qtxt <- paste0("It is known that in some population ", k, "% have a 
          certain genetic disorder. Say we randomly select people from this
          population until we find the first one with the disorder. We want to
          know the probability that it will take more then ", m, " tests. Using
          <nolink>simulation</nolink> we find it to be ", nm(y, eps=3*sqrt(y*(1-y)/B)),
          "<p>You can generate data for this distribution with rgeom(B, ", k, "/100)+1")
      atxt <- paste("B <- 10000<p>x <- rgeom(B, ", k, "/100)+1<p>
              length(x[x>", m, "])/B")     
   
   }
        
   list(qtxt = paste0("<h5>", qtxt, "<p>Use B=10000 for the <nolink>simulation</nolink>
         .</h5><hr>"),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
