quiz5.R.practice <- function(problem) {
   category <- paste0("Introducrion to R/ R Practice  / Problem = ", problem)
   quizname <- "problem - " 
   htxt <- ""
   atxt <- ""
   x <- NULL
   if(problem==1) {
      n <- sample(200:250, 1)
      k <- sample(101:130, 1)
      j <- c(sample(40:60, 1), sample(140:160, 1))
      mu <- sample(10:20, 1)*3
      x <- round(rnorm(n, mu, mu/5), 1)
      x[j[1]] <- max(x[j[1]:j[2]]) - 3
      x[j[2]] <- max(x[j[1]:j[2]]) - 2.5
      qtxt <- paste0("Find the  following:
          <p>&nbsp;<p>The ", k, "<sup>th</sup> number is ", nm(x[k]), 
         "<p>&nbsp;<p>The sum of all the numbers is  ", nm(sum(x)), 
         "<p>&nbsp;<p>The largest number between the ", j[1], "<sup>th</sup> and 
                ", j[2], "<sup>th</sup> is ", nm(max(x[j[1]:j[2]]))) 
      htxt <- paste0("copy numbers, go to R and run command get.moodle.data().
         <p>&nbsp;<p>x[", k, "] = ", x[k],   
         "<p>&nbsp;<p>sum(x) = ", sum(x),
         "<p>&nbsp;<p>max(x[", j[1], ":", j[2], "]) = ", max(x[j[1]:j[2]]))
   }
 
    if(problem==2) {
       n <- sample(200:300, 1) 
       m <- sample(10:15, 1)
       z <- round(sum(sqrt(rep(1:4, m))), 1)
       qtxt <- paste0("The sum of the <b>even</b> numbers from 2 to ", 2*n, 
            " is ", nm(sum(2*c(1:n))),
            "<p>&nbsp;<p>Consider the sequence 1, 2, 3, 4, repeated ", m, " times. 
             The sum of the square roots of these numbers is ", nm(z, eps=0.001)) 
            
            
       htxt <- paste0("sum(2*c(1:", n, ")) = ", sum(2*c(1:n)),
            "<p>&nbsp;<p>round(sum(sqrt(rep(1:4, ", m, "))), 1) = ", z)    
   }  

   if(problem==3) {
      attach(world.mortality.2017)
      z1 <- round(mean(LifeExpectancy.Both), 2)
      z2 <- LifeExpectancy.Both[Country=="Puerto Rico"]
      z3 <- Country[LifeExpectancy.Both==max(LifeExpectancy.Both)]      
      z4 <- rep(0, length(Country))
      z4[Country==z3] <- 100
                      
      qtxt <- paste0("In this problem we will use the Resma3 data set 
          <b>world.mortality.2017</b>.
          <p>&nbsp;<p>There are ", nm(dim(world.mortality.2017)[1]), " countries
          in this data set.
          <p>&nbsp;<p>The mean life expectancy is ", nm(z1, eps=1), 
         "<p>&nbsp;<p>The life expectancy in Puerto Rico is ", nm(z2), 
         "<p>&nbsp;<p>The country with the highest life expectancy is ", 
              mc(Country, z4)$qmc
          ) 
   
      htxt <- paste0("dim(world.mortality.2017) or ncol(world.mortality.2017)
            <p>&nbsp;<p>attach(world.mortality.2017)
            <p>round(mean(LifeExpectancy.Both), 2)
            <p>LifeExpectancy.Both[Country==\"Puerto Rico\"]
            <p>Country[LifeExpectancy.Both==max(LifeExpectancy.Both)]
            (Answer: ", mc(Country, z4)$amc, ")")
   }  
   
   if(problem==4) {
        n <- sample(50:70, 1)
        Gender <- sample(c("Male", "Female"), size=n, replace=T)
        Age <- sample(18:23, size=n, replace=T)
        GPA <- sample(seq(2, 4, 0.1), size=n, replace=T)
        Year <- sample(c("F", "J", "So", "Se"), size=n, replace=T, prob=c(4, 3, 2,1))
        x <- data.frame(Gender, Year, Age, GPA)
        z <- length(Gender[Gender=="Female" & Year=="F"])
        qtxt <- paste0("Below is some data on students in a course.
          <p>&nbsp;<p>There are ", nm(n), " students in this class.
          <p>&nbsp;<p>There are ", nm(length(Gender[Gender=="Female"])), 
            " female students.
          <p>&nbsp;<p>The percentage of seniors is ", nm(round(length(Year[Year=="Se"])/n*100, 1), eps=0.51),
          "<p>&nbsp;<p>The mean GPA of the male students is ", 
                nm(mean(GPA[Gender=="Male"]), eps=0.1),         
         "<p>&nbsp;<p>There are ",  nm(z), " female freshman in this class")
                 
        htxt <- paste0("copy table with numbers, go to R and run command get.moodle.data().
          <p>&nbsp;<p>dim(x)
          <p>&nbsp;<p>table(Gender)
          <p>&nbsp;<p>length(Year[Year==\"Se\"])/length(Year)*100
          <p>&nbsp;<p>mean(GPA[Gender==\"Male\"])
          <p>&nbsp;<p>subset(x, Gender==\"Female\" & Year==\"F\"), or use the isubset() command
          ")
   }

   if(problem==5) {
        n <- sample(150:200, 1)
        Time <- round(runif(n, 10, 30), 1)
        Score <- round(3*Time+10+rnorm(n, 0, 5), 1)
        x <- data.frame(Time=Time, Score=Score)
        
        tm <- round(quantile(Time, c(0.25, 0.75)), 1)
        mtm1 <- round(mean(Score[Time<tm[2]]), 2)
        mtm2 <- round(mean(Score[Time>tm[1] & Time<tm[2]]), 2)
        
        z <- Time[Score==max(Score)]
        
        qtxt <- paste0("Below is some data on the time (in minutes) that 
            people taking a test needed to finish and their scores.
            <p>&nbsp;<p>", nm(n), " people took this test.
            <p>&nbsp;<p>The mean score is ", nm(round(mean(Score), 2), eps=0.51), 
           "<p>&nbsp;<p>The mean score of people who finished in under ", tm[2], "
             minutes is ", nm(mtm1, eps=0.051),
           "<p>&nbsp;<p>The mean score of people who finished in needed between ", tm[1], "
             and ", tm[2], " minutes is ", nm(mtm2, eps=0.051),   
           "<p>&nbsp;<p>The person with the highest score needed ", nm(z),
           " minutes")           
           
        htxt <- paste0("copy table with numbers, go to R and run command get.moodle.data().
            <p>&nbsp;<p>dim(x)
            <p>&nbsp;<p>mean(Score)  
            <p>&nbsp;<p>mean(Score[Time<", tm, "])
            <p>&nbsp;<p>use the isubset() command to get the Scores wih these times
            <p>&nbsp;<p>Time[Score==max(Score)]")
   }   
   
   if(problem==6) {
        n <- 10
        x <- round(runif(n, 500, 1000))
        qtxt <- paste0("For the data below find <b> without using R</b> 
        <p>&nbsp;<p>x[3] = ", nm(x[3]),
       "<p>&nbsp;<p>sum(x[c(3, 7)]) = ", nm(sum(x[c(3, 7)])),
       "<p>&nbsp;<p>sum(x[1:3]) = ", nm(sum(x[1:3])),
       "<p>&nbsp;<p>sum(x[-c(1:7)]) = ", nm(sum(x[-c(1:7)])))
       
       htxt <- paste0("x[3] = ", x[3],
       "<p>&nbsp;<p>sum(x[c(3, 7)]) = ", sum(x[c(3, 7)]),
       "<p>&nbsp;<p>sum(x[1:3]) = ", sum(x[1:3]),
       "<p>&nbsp;<p>sum(x[-c(1:7)]) = ", sum(x[-c(1:7)]))
   }
        
   if(problem==7) {
        n <- 10
        A <- round(runif(n, 1, 10))
        B <- round(runif(n, 50, 100))
        x <- data.frame(A, B)
        qtxt <- paste0("For the data below find <b> without using R</b> 
        <p>&nbsp;<p>x[1, 1] = ", nm(x[1, 1]),
       "<p>&nbsp;<p>x[3, 2] = ", nm(x[3, 2]),
       "<p>&nbsp;<p>sum(x[2, 1:2]) = ", nm(sum(x[2, 1:2])),
       "<p>&nbsp;<p>sum(x[c(1:2, 8:9), 2]) = ", nm(sum(x[c(1:2, 8:9), 2])),
       "<p>&nbsp;<p>sum(x[5, ]) = ", nm(sum(x[5, ])),
       "<p>&nbsp;<p>sum(x[, 2]) = ", nm(sum(x[, 2])),
       "<p>&nbsp;<p>sum(x[-c(1:8), ]) = ", nm(sum(x[-c(1:8), ])),
       "<p>&nbsp;<p>sum(x[7:9, 1:2]) = ", nm(sum(x[7:9, 1:2])))
       
       htxt <- paste0("x[1, 1] = ", x[1, 1],
       "<p>&nbsp;<p>x[3, 2] = ", x[3, 2],
       "<p>&nbsp;<p>sum(x[2, 1:2]) = ", sum(x[2, 1:2]),
       "<p>&nbsp;<p>sum(x[c(1:2, 8:9), 2]) = ", sum(x[c(1:2, 8:9), 2]),
       "<p>&nbsp;<p>sum(x[5, ]) = ", sum(x[5, ]),
       "<p>&nbsp;<p>sum(x[, 2]) = ", sum(x[, 2]),
       "<p>&nbsp;<p>sum(x[-c(1:8), ]) = ", sum(x[-c(1:8), ]),
       "<p>&nbsp;<p>sum(x[7:9, 1:2]) = ", sum(x[7:9, 1:2]))
   }        
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>", 
        ifelse(is.null(x), "", paste("<hr>", moodle.table(x)))),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
