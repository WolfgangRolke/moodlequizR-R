anova.nonpar <- function() {
   category <- "ANOVA / Nonparametric"
   quizname <- "problem -"
   
   g <- function() {
     ngroup <- length(table(x))
     pval <- kruskalwallis(y, x, return.result = TRUE)
     pval.anova <- oneway(y, x, return.result = TRUE)[1]
     pval.log <- oneway(log(y), x, return.result = TRUE)[1]
     command <- "kruskalwallis(y, x)"
     if(pval < alpha) 
         qdecision <- "<p>Decision: {1:MC:~=Reject~Fail to reject} null hypothesis"
     else    
         qdecision <- "<p>Decision: {1:MC:~Reject~=Fail to reject} null hypothesis"     
     command <- "kruskalwallis(y, x)"
     qtxt <- paste0("
         <p>p-value = {:NM:%100%", pval, ":0.01~%50%", pval.anova, ":0.01~%75%", pval.log, ":0.01}
         <p>", qdecision)
     
      atxt <- paste0("The boxplot shows outliers, a log transform does not work.
          <p>Therefore we need to use the Kruskall-Wallis test 
          <p>p-value of test: ", pval, "
          <p>R command: ", command)       
      list(qtxt = qtxt, atxt = atxt)  
   } 
   whichstory <- sample( 1:5, 1) 
   alpha <- sample( c(0.01, 0.05, 0.1), 1)
   if(whichstory == 1) {
        ngroup <- 5
        n <- sample( 10:20, size = ngroup, replace = T)
        z <- c("", paste0("Machine-", 1:ngroup))
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
        z <- c("", paste0("Mall-", 1:ngroup))
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1],  n)
        y <- round(50 + ifelse(x==k, 10, 0) + rnorm(sum(n), 0, 10), 1)
   }   
   if(whichstory == 4) {
        ngroup <- 6
        n <- sample( 10:20, size = ngroup, replace = T)         
        z <- c("", paste0("City-", LETTERS[1:ngroup]))        
        k <- sample( z , size=3, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1] , n)
        y <- 10000  + rnorm(sum(n), 0, 3000)
        for(j in k) y[x==j] <- y[x==j] + + sample(c(-1,1),1)*sample( 1000+50*1:40, 1)
        y <- round(y, -1)
    }   
    if(whichstory == 5) {
        ngroup <- 4
        z <- c("", "Arts&Sciences", "Law", "MedicalSciences", "Engineering")
        n <- sample( 10:20, size = ngroup, replace = T)         
        k <- sample( z , size=2, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1], n)
        y <- 2.7 + rnorm(sum(n), 0, 0.5)
        for(j in k) y[x==j] <- y[x==j] + sample(c(-1,1),1)*sample( c(0.1, 0.2, 0.3), 1)
        y <- round(y, 1)
    }
    if(whichstory == 6) {
        ngroup <- sample( 5:8, 1)
        z <- c("", paste0("Type-", 1:ngroup))
        n <- sample( 10:15, size = ngroup, replace = T)         
        k <- sample( z , size=2, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1], n)
        y <- 10 + rnorm(sum(n), 0, 1)
        for(j in k) y[x==j] <- y[x==j] + sample(c(-1,1),1)*runif(length(x[x==j]), 2, 5)
        y <- round(y, 2)
    }
    medians <- tapply(y, x, median)
    iqrs <- tapply(y, x, IQR)
    z <- z[z!=""]
    repeat {
      whichadd <- sample(0:1, size=ngroup, replace=TRUE)
      if(sum(whichadd)>0) break
    }  
    for(i in 1:ngroup)
      if(whichadd[i]==1) 
          y[x==z[i]][1] <- medians[i] + runif(1, 3, 5)*iqrs[i]
    if(whichstory == 1) {
       data <- data.frame(Length = y, Machine = x)    
       qtxt <- paste0("A company has a 5 machines that make widgets. They want to test that all machines make
           widgets of the same length, so they randomly select some widgets made by each machine and measure their 
          lengths (in inches). Test at the ", 100*alpha,"% level whether the  true mean lengths of the widgets are the same.")               
    }  
    if(whichstory == 2) {
       data <- data.frame(Length = y, Employee = x)
       qtxt <- paste0("A call center has 4 people answering the phones. They want to see whether there is a difference 
          in the lengths of the calls answered by each employee, so they time a number of their calls. 
          Test at the ", 100*alpha,"% level whether the mean lengths of the calls are the same.")
   }
   if(whichstory == 3) {
         data <- data.frame(Amount = y, Outlet = x)
         qtxt <- paste0("A store has outlets in three different malls. They have just done a major add compaign on TV, 
            and they want to know if the effect is different for the three outlets, so they randomly choose a number of sales receipts from each store.
            Test at the ", 100*alpha,"% level whether the mean amounts of sales are the same.")
   }
   if(whichstory == 4) {
        data <- data.frame(Amount = y, City = x)
        qtxt <- paste0(" An insurance company wants to know whether there differences between the sizes of the
        insurance claims in six cities. They randomly choose a number claims in each of the cities.
        Test at the ", 100*alpha,"% level whether the mean amounts of sales are the same.")
   }                            
   if(whichstory == 5) {
        data <- data.frame(GPA = y, School = x)   
        qtxt <- paste0("The chancellor of some university wants to know whether there are differences between 
        students from different schools and their GPAs. He randomly chooses a number of students
        from each school.
        Test at the ", 100*alpha,"% level whether the mean GPAs are the same.")             
   }
   if(whichstory == 6) {
        data <- data.frame(Weight = y, Bird = x)   
        qtxt <- paste0("A biologist is studying a certain species of birds. These birds have ", ngroup, " subspecies.
        He wants to find out whether the birds in different subspecies have different weights (in gram). He randomly collects birds
        from each subspecies.
        Test at the ", 100*alpha,"% level whether the mean weights are the same.")             
   }
    list(qtxt = paste0("<h5>",qtxt, g()$qtxt,  "</h5><hr>", moodle.table(data), "</hr>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)     
        
   
}   
