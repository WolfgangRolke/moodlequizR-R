quiz4.basic.R.4 <- function() {
   category <- "Introduction to R   / Basic R, Part 4"
   quizname <- "problem - " 
   nyears <- sample(5:15, 1)
   x <- length(wrinccensus$Years[wrinccensus$Years>=nyears])
   qtxt <- paste0("In this quiz we will use the data set <b>wrinccensus</b>, 
        already part of Resma3.
        <p>1) There are ", nm(x), " employees with ", nyears, " or more years of service.")
   atxt <- paste0("attach(wrinccensus)
                  <p>isubset conditions: 
                  <br>1) Years more or equal to ", nyears)

   lvl <- sample(2:4, 1)
   gndr <- sample(1:2, 1)
   x <- nrow(subset( wrinccensus , Gender == c("Male", "Female")[gndr] & Job.Level <= lvl))
   qtxt <- paste(qtxt, "      
        <p>2) There are ", nm(x), c(" male ", " female ")[gndr], " employees with a
           job level of ", lvl, " or lower")
   atxt <- paste(atxt, "      
        <p>2) Gender equal to ", c("Male", "Female")[gndr], " AND Job.Level <= ", lvl )   
        
   lvl <- sample(1:5, 1) 
   gndr <- sample(1:2, 1)
   a <- subset( wrinccensus , Gender == c("Male", "Female")[gndr] & Satisfaction == lvl)
   x <- mean(a[, "Income"])
   qtxt <- paste(qtxt, "      
        <p>3) The mean income of ", c(" male ", " female ")[gndr], " employees 
           with a satisfaction rating of ", lvl, " is ", nm(x, eps=10))
   atxt <- paste(atxt, "      
        <p>3) Gender equal to ", c("Male", "Female")[gndr], " AND Satisfaction == ", lvl )   
    
   lvls <- c(sample(1:7, 1), sample(1:5, 1) , sample(5:10, 1))  
   
   a <- subset( wrinccensus , ( Job.Level == lvls[1] & Years < lvls[3] ) & Satisfaction == lvls[2] )
   x <- nrow(a)
   qtxt <- paste(qtxt, "      
        <p>4) There are ", nm(x), " employees who have a job level of ", lvls[1], 
         ", a satisfaction rating of ", lvls[2], " and less than ", lvls[3], " years of service")
   
   atxt <- paste(atxt, "      
        <p>4) ( Job.Level equal to ", lvls[1], 
           " AND Satisfaction equal to ", lvls[2], " ) AND  Years less then ", lvls[3] )   
   htxt <- "Use either the isubset command or the bracket notation to get the data required 
            for each problem" 
        
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>"),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
