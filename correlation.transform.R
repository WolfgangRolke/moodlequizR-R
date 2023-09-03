correlation.transform <- function() {
    require(mvtnorm)
    category <- "Correlation / Transform"
    quizname <- "problem -" 
    g <- function() {
      plt64 <- png64(mplot( x, y, return.graph = TRUE))
      if(tr.xy == "x") {
              plt64a <- png64(mplot( log(x), y, return.graph = TRUE))
              est <- round(cor(log(x), y ), 3)
              atxt <-  paste0("There is a problem with the x variable:<br>", plt64, 
                    "<br>a log transform of x fixes those problems:<br>", plt64a, "<br>")
              command <- "cor(log(x), y )"       
      }        
      if(tr.xy == "y") {
              plt64a <- png64(mplot( x, log(y), return.graph = TRUE))
              est <- round(cor(x, log(y) ), 3)
              atxt <-  paste0("There is a problem with the y variable:<br>", plt64, 
                    "<br>a log transform of y fixes those problems:<br>", plt64a, "<br>")
              command <- "cor(x, log(y) )"       
      }          
      if(tr.xy == "xy") {
              plt64a <- png64(mplot( log(x), log(y), return.graph = TRUE))      
              atxt <-  paste0("There are problems with both x and y variables:<br>", plt64, 
                     "<br> log transforms of both x and y fix those problems:<br>", plt64a, "<br>")      
              est <- round(cor(log(x), log(y)), 3) 
              command <- "cor(log(x), log(y) )" 
      }     
      qtxt <- paste0( "<p>Pearson's correlation coefficient is : 
                          {:NM:%100%", est, "~%100%", round(est, 3), 
                          "~%80%", est, ":0.01, ~%80%", est, "}") 
      atxt <- paste0(atxt, "<p>Pearson's correlation coefficient is : ", est)
      command <- paste(command, "<br>round to 3 digits")
      atxt <- paste0( atxt, "<p>", "<p>R command: ", command, collapse = "")                    
      list(qtxt = qtxt, atxt = atxt)  
    } 
    n <- sample( 20:50, 1)
    mu <- sample(0:3, 2, T)
    sigma <- sample(1:5, 2, T)
    rho <- runif(1, -1, 1)
    xy <- rmvnorm(n, mean = mu, sigma = matrix(c(sigma[1], rho, rho, sigma[2]),2,2))
    x <- round(xy[,1], 1)
    y <- round(xy[,2], 2)
    tr.xy <- sample( c("x", "y", "xy"), 1)
    if(tr.xy %in% c("x", "xy")) x <- exp(x)
    if(tr.xy %in% c("y", "xy")) y <- exp(y)
    data <- data.frame(x=x, y=y)
    qtxt <- "<p>How strong is the relationship of x and y?"
    g1 <- g() 
#print( c(qtxt, g1$qtxt, g1$atxt) ) 
   list(qtxt = paste0("<h5>", qtxt, g1$qtxt,"</h5><hr>", moodle.table(data)), 
        atxt = paste0("<h5>", g1$atxt,"</h5>"), 
        category = category, quizname = quizname)     
}   
