quiz2.basic.R.2 <- function(B = 1) {
    category <- "Introduction to R / Basic R Part 2"
    quizname <- "problem - "
    n <- sample(100:120, 1)
    x <- round(runif(n, 1, n))
    z <- sample(c(0,1), size = n, replace = T, prob = c(1,2))
    y <- round(x + 5*z + rnorm(n, 0, 3), 1)
    datatblx <- moodle.table(x, 20)
    datatblz <- moodle.table(z, 20)
    datatbly <- moodle.table(y, 20)
    out <- mean(x)
    out[2] <- mean(y)
    out[3] <- sum(z)
    out[4] <- round(sum(z)/length(z)*100, 1)
    q1 <- round(quantile(x, 0.25), 1)
    out[5] <- mean(x[x>q1])
    qtxt <- paste0("Below is some data. Transfer it to R <b>one variable at a time</b> with the get.moodle.data command. Remember to save each data set before copy-pasting the next one.
                <hr><strong>x variable</strong><p>", datatblx, "<hr>
                <strong>y variable</strong><p>", datatbly, "<hr>              
                <strong>z variable</strong><p>", datatblz, "<hr>            
                Answer the following questions:<p>
                the mean of the x variable is ", nm(out[1],  eps=1), 
               "<p>the mean of the y variable is ", nm(out[2],  eps=1), 
               "<p>the number of 1's in the z variable is ", nm(out[3]), 
               "<p>the <b>percentage</b> of 1's in the z variable rounded to one
               digit behind the decimal is ", nm(out[4]), 
               "<p>the mean of those x's that are greater then ", q1, " is ", nm(out[5], eps=1))
                
    htxt <- "useful R commands: sum, mean, table"
                      
    atxt <- paste0("Correct answers:
                  <p>Enter data into R as described in class
                  <p>Mean of x = ", out[1], " R: mean(x)
                  <p>Mean of y = ", out[2], " R: mean(y)
                  <p>the number of 1's in the z variable = ", out[3], "  R: sum(z)
                  <p>the percentage of 1's in the z variable rounded to one
               digit behind the decimal = ", out[4], 
                  "  R: round(sum(z)/length(z)*100, 1)
                  <p>R: mean(x[x>",q1, "])")   
               
    list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname)
}    
