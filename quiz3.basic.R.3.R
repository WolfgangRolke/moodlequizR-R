quiz3.basic.R.3 <- function() {
   category <- "Introduction to R / Basic R Part 3"
   quizname <- "problem - " 
   x <- matrix(0, 100, 4)
   colnames(x) <- c("Age", "GPA", "Score", "Gender") 
   x <- data.frame(x)
   x[, 1] <- round(runif(100, 18, 25))
   x[sample(10:90, 1), 1] <- 26
   x[, 2] <- round(runif(100, 2.0, 4), 1)
   x[, 3] <- round(runif(100, 2, 10), 2)
   x[, 4] <- sample(c("Male", "Female"), size=100, replace=TRUE, prob=c(0.4, 0.6))
   
   qtxt <- paste0("Below is the data from a survey college students. 
      <p>&nbsp;<p>The number in row 31, column 3 is ", nm(x[31, 3]))
               
   atxt <-paste0("x[31, 3] = ", x[31, 3])
      
   qtxt <- paste0(qtxt, "<p>The sum of the GPA's in rows 10 to 20 is ", nm(sum(x[10:20, 2])))      
   atxt <- paste0(atxt, "<p>sum(x[10:20, 2]) = ", sum(x[10:20, 2]))
   qtxt <- paste0(qtxt, "<p>The highest score in rows 60 to 100 is ", nm(max(x[60:100, 3])))      
   atxt <- paste0(atxt, "<p>max(x[60:100, 3]) = ", max(x[60:100, 3]))  
   
   mx <- round(mean(x[, 1]), 1)
   qtxt <- paste0(qtxt, "<p>The mean age is ", nm(mx,eps=1))      
   atxt <- paste0(atxt, "<p>mean(x[, 1]) = ", mx)
   
   if(x[x[ ,1]==max(x[, 1]), 4] == "Male") w <- c(100, 0)
   else w <- c(0, 100)   
   qtxt <- paste0(qtxt, "<p>the oldest student ", mc(c("Male", "Female"), w)$qmc)      
   atxt <- paste0(atxt, "<p>x[x[ ,1]==max(x[, 1]), 4], Answer:  ", x[x[ ,1]==max(x[, 1]), 4])
   
   htxt <- "Copy the <b>whole</b> table (including the column titles), switch to
      R and use the get.moodle.data() command.
      <p>Read the section on the bracket [, ] notation.
      <p>useful R commands: x[1, 1], x[, 1], sum, max"   
        
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(x)),
        htxt = paste0("<h5>", htxt, "</h5>"),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
