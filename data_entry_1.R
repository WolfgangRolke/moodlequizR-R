data_entry_1 <- function() {
    category <- "Introduction to R/Data Entry 1"
    quizname <- " data entry 1 -"
    n <- sample( 50:75, 1)
    x <- round( runif( n, 10, 100) )
    n <- sample( 150:200, 1)
    y <- sample( LETTERS[1:6], size = n, replace = T, prob = c(1,4,4,1,1,1))
    n <- sample( 50:75, 1)
    z <- round( runif( n, 10, 100), 1 )
    z0 <- paste0(z[1], paste0(", ", z[-1]), collapse="")
    n <- 10
    tmp <- sort( round(runif( 10, 20, 50)) )
    u <- data.frame( x=tmp, y=round(2*tmp+rnorm(10, 0, 5))) 
    out <- round( mean(x), 1)
    out[2] <- length( y[y=="A"] )
    out[3] <- sum(z)
    out[4] <- round( cor(u[,1], u[,2]), 3)
    out[5] <- round( cor(u[,1], u[,2]), 4)
    qtxt <- paste0("<h5>
            1) Find the mean of these numbers: <p>", moodle.table(x, 15), 
            "<p>{:NM:=", out[1], "~%75%", out[1], ":1}<hr>
            2) Find the number of As: <p>", moodle.table(y, 20), 
            "<p>{:NM:=", out[2], "}<hr>
            3) Find the sum of these numbers: <p>", z0, 
            "<p>{:NM:=", out[3], "}<hr>
            4) Find the correlation of x and y (R command cor(x) ): <p>", moodle.table(u), 
            "<p>{:NM:=", out[4], "~%100%", out[5], "~%75%", out[4], ":0.001}
                    </h5>")
    atxt <- paste0("<h5>In all problems use the mouse to highlight the data, go to R and enter commands<p>
                      Correct answers:<p>
                      1) ", out[1], " (rounded to 1 digit)<br>R commands: <br>
                            x <-scan(\"clipboard\")<br>
                            mean(x)<p>          
                      2) ", out[2], " <br>R commands: <br>
                            x <-scan(\"clipboard\", what = \"char\")<br>
                            table(x)<p>
                      3) ", out[3], " <br>R commands: <br>
                            x <-scan(\"clipboard\", sep=",")<br>
                            sum(x)<p>          
                      4) ", out[4], " (rounded to 3 or 4 digits)<br>R commands: <br>
                            x <- idataio()<br> (use Copy from Clipboard option)
                            cor(x)         
                 </h5>")
    list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)              
}    
