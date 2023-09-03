anova.transform <- function() {
   category <- "ANOVA / Transformation"
   quizname <- "problem -"
   g <- function() {
     plt64a <- png64(bplot(y, x, return.graph = TRUE))
     plt64b <- png64(bplot(log(y), x, return.graph = TRUE))
     plt64c <- png64(oneway(log(y), x, return.graph = TRUE))     
     out <- oneway(log(y), x, return.result = TRUE)
     pval <- out[1]
     if(pval<0.0001) pval <- 0
     if(pval < alpha) 
         qdecision <- "<p>Decision: {1:MC:~=Reject~Fail to reject} null hypothesis"
     else    
         qdecision <- "<p>Decision: {1:MC:~Reject~=Fail to reject} null hypothesis"     
     command <- "oneway(log(y), x)"
     qtxt <- paste0("
         <p>p-value = {:NM:%100%", pval, ":0.01}
         <p>", qdecision)
       
     atxt <- paste0("The boxplot shows outliers, a log transform fixes the problem.
          <p>p-value of test: ", ifelse(pval==0, "0.000", pval), "
          <p>R command: ", command, " 
          <p>Graphs<p>", plt64a, "<p>", plt64b, "<p>", plt64c)       
      list(qtxt = qtxt, atxt = atxt)  
   } 
   whichstory <- 1 
   alpha <- sample( c(0.01, 0.05, 0.1), 1)
   repeat { 
      if(whichstory == 1) {
        ngroup <- sample(3:5, 1)
        n <- sample(15:20, 1)
        z <- paste0("Operator-", 1:ngroup)
        x <- rep(z, each=n)
        y <- round(rlnorm(n*ngroup)+0.1, 1)
        if(runif(1)<0.5) {
            which_op <- sample(z, 1)
            repeat {
                y[x==which_op] <- y[x==which_op]+1
                if(oneway(log(y), x, return.result=TRUE)[1]<alpha) break
            }            
        }
        data <- data.frame(Length = y, Machine = x)    
        qtxt <- paste0("The call center of a company has a ", ngroup, " phone operators. 
        They want to see whether the average length of calls of the operators is the same. 
        They randomly select ", n, " calls from each operator and time their 
        lengths (in minutes). <p>Test at the ", 100*alpha,"% level whether the  
        true mean lengths of the calls are the same.")               
      }  
      out <- oneway(y, x, return.result=TRUE)
      if(3*out[4]<out[5]) next
      out <- oneway(log(y), x, return.result=TRUE)
      if(3*out[4]>out[5]) break
      
    }
    list(qtxt = paste0("<h5>",qtxt, g()$qtxt,  "</h5><hr>", moodle.table(data), "</hr>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)     
        
   
}   
