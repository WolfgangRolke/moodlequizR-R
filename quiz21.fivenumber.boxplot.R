quiz21.fivenumber.boxplot <- function() {
   require(base64)
   category <- paste0(" Summary Statistics / Five Number Summary From Boxplot" )
   quizname <- "problem - " 
   
   n <- 100
   x <- rnorm(n, runif(1, 50, 70), runif(1, 10, 20))
   plt64 <- png64(bplot(x, return.graph=TRUE, label_x="", label_y=""))
   y <- fivenumber(x, return.result = TRUE)
   
   qtxt <- paste0("Find the <nolink>five number summary of the boxplot</nolink> shown below.
          <p><table><tr><th>Minimum</th><td>", nm(y[1], eps=3), "</td></tr>
          <tr><th>1<sup>st</sup> Quartile</th><td>", nm(y[2], eps=3),"</td></tr>
          <tr><th>Median</th><td>", nm(y[3], eps=3),"</td></tr>
          <tr><th>3<sup>rd</sup> Quartile</th><td>", nm(y[4], eps=3),"</td></tr>
          <tr><th>Maximum</th><td>", nm(y[5], eps=3),"</td></tr></table>")
   
   atxt <- paste(r.tbls(y), collapse="<br>")
#   paste0(colnames(y)[1:5]," = ", y[1:5], "<p>", collapse="")
   htxt <- "What are the numbers shown in the graph? Read up on the boxplot!
           <p>the answers do not have to be exactly right"     
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", plt64),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        htxt = paste0("<h5>", htxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
