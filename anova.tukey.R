anova.tukey <- function() {
   category <- "ANOVA / Tukey"
   quizname <- "problem - "
   g <- function() {
     ngroup <- length(table(x))
     out <- tukey(y, x, show.all=TRUE,return.result = TRUE)
     command <- "tukey(y, x)"
     m <- nrow(out)
     pairs <- gsub("-", " and ", out[,1])
     qtxt <- ""
     for(i in 1:m) {
        qtxt <- paste0(qtxt, "<p>Is ", strsplit(out[i, 1], "-")[[1]][1], 
          " statistically significantly different from ", strsplit(out[i, 1], "-")[[1]][2],     
          "? ", ifelse(out[i, 2]<0.05, "{1:MC:~=Yes~No}",  "{1:MC:~Yes~=No}"))
     }
     atxt <- "Pairs that are statistically significantly different (p-value<0.05):"
     for(i in 1:m) {
        if(out[i, 2] < 0.05) atxt <- paste0(atxt, "<p>", pairs[i])
     }
     atxt <- paste0( atxt, "<p>", "<p>R command: ", command)       
     list(qtxt = qtxt, atxt = atxt)  
   } 
   whichstory <- sample( 1:5, 1) 
   alpha <- sample( c(0.01, 0.05, 0.1), 1)
   repeat {
    if(whichstory == 1) {
        ngroup <- 5
        n <- sample( 10:20, size = ngroup, replace = T)
        z <- c("", paste0("Machine_", 1:ngroup))
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup))) 
        x <- rep( z[-1] , n)
        y <- round(10 + ifelse(x==k, 1, 0) + rnorm(sum(n), 0, 0.5), 1)
    }   
    if(whichstory == 2) {
        ngroup <- 4
        n <- sample( 10:20, size = ngroup, replace = T)
        z <- c("", LETTERS[1:ngroup])         
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup))) 
        x <- rep( z[-1] , n)        
        y <- round(20 + ifelse(x==k, 6, 0) + rnorm(sum(n), 0, 2), 1)
    }   
    if(whichstory == 3) {
        ngroup <- 3
        n <- sample( 10:20, size = ngroup, replace = T)      
        z <- c("", paste0("Mall_", 1:ngroup))
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1],  n)
        y <- round(50 + ifelse(x==k, 10, 0) + rnorm(sum(n), 0, 10), 1)
    }   
    if(whichstory == 4) {
        ngroup <- 6
        n <- sample( 10:20, size = ngroup, replace = T)         
        z <- c("", paste0("City_", LETTERS[1:ngroup]))        
        k <- sample( z , size=3, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1] , n)
        y <- 10000  + rnorm(sum(n), 0, 3000)
        for(j in k) y[x==j] <- y[x==j] + + sample(c(-1,1),1)*sample( 1000+50*1:40, 1)
        y <- round(y, -1)
    }   
    if(whichstory == 5) {
        ngroup <- 4
        z <- c("", "Arts&Sciences", "Law", "Medical_Sciences", "Engineering")
        n <- sample( 10:20, size = ngroup, replace = T)         
        k <- sample( z , size=2, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1], n)
        y <- 2.7 + rnorm(sum(n), 0, 0.5)
        for(j in k) y[x==j] <- y[x==j] + sample(c(-1,1),1)*sample( c(0.1, 0.2, 0.3), 1)
        y <- round(y, 1)
    }
    if(whichstory == 6) {
        ngroup <- sample( 5:8, 1)
        z <- c("", paste0("Type_", 1:ngroup))
        n <- sample( 10:15, size = ngroup, replace = T)         
        k <- sample( z , size=2, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1], n)
        y <- 10 + rnorm(sum(n), 0, 1)
        for(j in k) y[x==j] <- y[x==j] + sample(c(-1,1),1)*runif(length(x[x==j]), 2, 5)
        y <- round(y, 2)
    }
    if(oneway(y, x, return.result=TRUE)[1]<0.05) break
   }
    if(whichstory == 1) {
       data <- data.frame(Length = y, Machine = x)    
       qtxt <- paste0("A company has a 5 machines that make widgets. They want to test that all machines make
           widgets of the same length, so they randomly select some widgets made by each machine and measure their 
          lengths (in inches). ")
    }  
    if(whichstory == 2) {
       data <- data.frame(Length = y, Employee = x)
       qtxt <- paste0("A call center has 4 people answering the phones. They want to see whether there is a difference 
          in the lengths of the calls answered by each employee, so they time a number of their calls. ")
   }
   if(whichstory == 3) {
         data <- data.frame(Amount = y, Outlet = x)
         qtxt <- paste0("A store has outlets in three different malls. They have just done a major add compaign on TV, 
            and they want to know if the effect is different for the three outlets, so they
             randomly choose a number of sales receipts from each store.")
   }
   if(whichstory == 4) {
        data <- data.frame(Amount = y, City = x)
        qtxt <- paste0(" An insurance company wants to know whether there differences between the sizes of the
        insurance claims in six cities. They randomly choose a number claims in each of the cities.")
   }                            
   if(whichstory == 5) {
        data <- data.frame(GPA = y, School = x)   
        qtxt <- paste0("The chancellor of some university wants to know whether there are differences between 
        students from different schools and their GPAs. He randomly chooses a number of students
        from each school.")             
   }
   if(whichstory == 6) {
        data <- data.frame(Weight = y, Bird = x)   
        qtxt <- paste0("A biologist is studying a certain species of birds. These birds have ", ngroup, " subspecies.
        He wants to find out whether the birds in different subspecies have different weights (in gram). He randomly collects birds
        from each subspecies.")             
   }
    list(qtxt = paste0("<h5>",qtxt, g()$qtxt,  "</h5><hr>", moodle.table(data), "</hr>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)     
        
   
}   
