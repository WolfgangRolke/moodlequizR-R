quiz20.five.number <- function() {
   category <- paste0(" Summary Statistics / Five Number Summary" )
   quizname <- "problem - " 
   
   ndigits <- sample(0:3, 1)
   n <- sample(100:120, 1)
   x <- round(rnorm(n, 100, 30), ndigits)
   y1 <- fivenumber(x, return.result=TRUE, ndigit=ndigits+1) 
   y2 <- fivenumber(x, return.result=TRUE) 
   w <- c(100, 75)
   eps <- c(0, 1)
   qtxt <- paste0("Find the five number summary of the data below
          <p><table><tr><th>Minimum</th><td>", nm(c(y1[1], y2[1]), w=w, eps=eps), "</td></tr>
          <tr><th>1<sup>st</sup> Quartile</th><td>", nm(c(y1[2], y2[2]), w=w, eps=eps),"</td></tr>
          <tr><th>Median</th><td>", nm(c(y1[3], y2[3]), w=w, eps=eps),"</td></tr>
          <tr><th>3<sup>rd</sup> Quartile</th><td>", nm(c(y1[4], y2[4]), w=w, eps=eps),"</td></tr>
          <tr><th>Maximum</th><td>", nm(c(y1[5], y2[5]), w=w, eps=eps),"</td></tr></table>")
   htxt <- "What command does the five-number-summary?<br>Don't forget the rounding"
   if(ndigits==0) 
      atxt <- paste0("fivenumber(x)<p>data has 0 digits behind decimal, so answers should have 1 digit")
   else
     atxt <- paste0("fivenumber(x, ndigits=", ndigits+1,")<p>data has ", ndigits, " digits behind decimal,
      so answers should have ", ndigits+1, " digits")     
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(x)),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
