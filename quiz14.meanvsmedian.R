quiz14.meanvsmedian <- function(whichstory, doMean) {
   if(missing(whichstory))
      whichstory <- sample(1:2, 1)
   if(missing(doMean))
      doMean <- sample(c(T, F), 1)      
   category <- paste0("Summary Statistics / Mean vs Median 
   / Story ", whichstory, " / ", ifelse(doMean, "use mean", "use median"))
   quizname <- "problem - " 

   n <- sample(50:70, 1) 
   m <- round(sample(1000:2000, 1), -2)

   if(whichstory == 1) {
      x <- sort(round(10*rchisq(n, 1), 2))
      x[x<1] <- x[x<1]+0.5
      x[n] <- 5*x[n]
      if(doMean) {
        qtxt <- paste0("A company wants to estimate the sales taxes they will have to pay
          for a month. They randomly select a number of sales receipts. The sale amounts
          are shown below. If they know 
          that during the month they will have a total of ", m, " sales, the 
          estimated tax (assuming a sales tax rate of 11%) is ", 
          nm(round(mean(x)*m*0.11), eps=1),"$.")
        atxt <- paste0("mean(x)*", m, "*0.11 = ", round(mean(x)*m*0.11),
          "<p> Use the mean because large sales pay a lot of taxes!")
      }
      else {    
        qtxt <- paste0("A company wants to estimate the sales taxes they will have to pay
          on a typical sale. They randomly select a number of sales receipts. The sale amounts
          are shown below. If the sales tax is 11%, based on theses sale 
          the tax on a typical sale is ", 
          nm(round(median(x)*0.11), eps=1),"$.")
        atxt <- paste0("median(x)*0.11 = ", round(mean(x)*0.11),
          "<p> Use the median because large sales are not typical!")
      }          
   }
   
   if(whichstory == 2) {
      x <- sort(round(10*rchisq(n, 1)))
      x[n] <- 5*x[n]
      if(doMean) {
        qtxt <- paste0("A forrest biologist wants to estimate the total number
          of trees in a forrest. On a map he devides the forrest into ", m, " grids. 
          Then he randomly selects ", n , " of these grids, goes into the forrest and 
          counts the trees in each. The numbers are shown below. Based on these numbers
          he estimates the total number of trees to be ", 
          nm(round(mean(x)*m), eps=1),".")
        atxt <- paste0("mean(x)*", m, " = ", round(mean(x)*m),
          "<p> Use the mean because grids with lots of trees have lots of trees!")
      }
      else {    
        qtxt <- paste0("A forrest biologist wants to estimate the number
          of trees in a typical region of the forrest. On a map he devides the 
          forrest into ", m, " grids. 
          Then he randomly selects ", n , " of these grids, goes into the forrest and 
          counts the trees in each. The numbers are shown below. Based on these numbers
          he estimates that a typical grid has ", 
          nm(round(median(x)), eps=1)," trees.")
        atxt <- paste0("mean(x)*", m, " = ", round(median(x)),
          "<p> Use the median because most grids don't have that many trees!")
      }          
   }      
   if(whichstory == 3) { # FOR EXAM 1
      n=sample(20:30, 1)  
      x <- 100000 + sort(round(100000*rchisq(n, 1), -3))
      x[n] <- x[n]+1000000
      p=round(c(mean(x),mean(x),median(x),median(x)),-2)
      eps=c(0,1000,0, 1000)
      points=c(50, 30, 100, 80)
      qtxt <- paste0("A real estate agent wants to get an idea of the price of an average house in some
          neighborhood. She randomly selects  ", n, " recently sold houses. Their sales prices are shown below. 
          Based on these prices she estimates that a typical house costs ", 
          nm(p, points, eps=eps),"$.")
      atxt <- paste0("round(median(x),-2)  = ", p[3], 
          "<p> Use the median because there is one exceptionally expensive house, round to two digits.")          
   }   
   htxt <- "Which method for calculating an average is more appropriate for this problem?"     
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(x)),
         htxt = paste0("<h5>", htxt, "</h5>"),   
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
