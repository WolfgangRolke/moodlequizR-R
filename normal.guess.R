normal.guess <- function() {
    require(base64)
    category <- "Probability / Normal Distribution / Guess Mean&Std"
    quizname <- " problem -"
    mu <- round( runif( 1, 0, 100) )
    sigma <- round( runif( 1, 0, 30) )
    x <- seq( mu-3*sigma, mu+3*sigma, length = 250)
    y <- dnorm( x , mu, sigma)
    dta <- data.frame( x = x, y = y )
    plt <- ggplot( aes( x, y), data = dta) +
           geom_line( aes( x, y ) , colour = "blue", size = 1.1, data = dta)
    plt64 <- png64(plt)
    qtxt <- paste0("<h5>What are the mean and the standard deviation of this normal curve
                  <p>Hint: use the inormal routine and play around with the mean and standard 
                  deviation until they match this one! Use whole numbers only<p>", 
                  plt64, 
                 "<p>Mean: {:NM:%100%", mu, "~%90%", mu, ":1~%80%", mu, ":2~%50%", mu, ":5}  
                  <p>Standard Deviation: {:NM:%100%", sigma, "~%90%", sigma, 
                        ":1~%80%", sigma, ":2~%50%", sigma, ":5}
                 </h5>")
    atxt <- paste0("<h5>Mean: ", mu, 
                   "<p>Standard Deviation: ", sigma, "</h5>")
                    
    list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)    
}    
