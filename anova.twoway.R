anova.twoway <- function(with.interaction=TRUE, reps=TRUE) {
   category <- paste0("ANOVA/twoway", 
      ifelse(with.interaction, "/with interaction", "/without interaction"),  
      ifelse(reps, "/with repeated measurements", "/without repeated measurements"))        
   quizname <- "problem - "
  
   whichstory <- 1#sample( 1:5, 1) 
   if(whichstory==1) ngroups <- c(3, 3)
   if(reps) n <- sample(2:5, size = 1, prob=c(1,1,1,1))
   else n <- 1
   x1 <- rep(1:ngroups[1]-1, n*ngroups[2])
   x2 <- rep(1:ngroups[2]-1, each=n*ngroups[1])   
   x12 <- x1*x2
   if(runif(1)<0.5) x1a <- 0*x1
   else x1a <- x1
   if(runif(1)<0.5) x2a <- 0*x2        
   else x2a <- x2
   if(!with.interaction) x12a <- 0*x12   
   else x12a <- x12 
   y <- round(10 + x1a +x2a + 2*x12a + rnorm(ngroups[1]*ngroups[2]*n),  2)
   if(n>1) pvals <- twoway(y, factor(x1), factor(x2), return.result=TRUE)
   else pvals <- c(0, 0, 0)
      
   pvals1 <- twoway(y, factor(x1), factor(x2), with.interaction=FALSE, return.result=TRUE)
   if(n==1) {
      plt <- iplot(y, x1, x2, return.graph = TRUE)
      plt64 <- png64(plt)
   } 
   else plt64 <-""
   if(!with.interaction | pvals[3]>0.05) {   
      q1 <- paste0("{:NM:%100%", pvals1[1], ":0.001~%50%", pvals[1], "0.001}")
      a1 <- pvals1[1]
      q2 <- ifelse(pvals1[1]<0.05, "{:MC:~=is~is not}", "{:MC:=is~=is not}")
      a2 <- ifelse(pvals1[1]<0.05, " is ", "is not ")
   }      
   else {
      q1 <- paste0("{:NM:%100%", pvals[1], ":0.001~%50%", pvals1[1], "0.001}")
      a1 <- pvals[1]
      q2 <- ifelse(pvals[1]<0.05, "{:MC:~=is~is not}", "{:MC:=is~=is not}")
      a2 <- ifelse(pvals[1]<0.05, " is ", "is not ")
   }  
   rps <- ifelse(n==1, " no ", "")
   cnrps <- ifelse(n>1, "", " not ")
   int <- ifelse(with.interaction, " is some ", "is no ")   
   if(whichstory == 1) {
      Machine <- paste0("Machine-", rep(0:2, n*ngroups[2]))
      Shift <- rep(c("Morning", "Afternoon", "Night"), each=n*ngroups[1])
      data <- data.frame(Length = y, Machine = Machine, Shift=Shift)  
      command <- "twoway(Length, Machine, Shift)"
      if(!with.interaction| pvals[3]>0.05)   
        command <- "twoway(Length, Machine, Shift, with.interaction=FALSE)"      
      qtxt <- paste0("A company has 3 machines that make widgets. They are used
          during 3 shifts. They want to test that all machines make
           widgets of the same length, so they randomly select some widgets made by 
           each machine and measure their lengths (in inches). Test at the 5% level 
           whether the  true mean lengths of the widgets are different for different machines.
           <p>Your analysis should include the information on the shifts.
           <p>The p value of the test for the machines is ", q1, "and so we conclude that 
           the factor machines ", q2, " statistically significant")
            
       atxt <- paste0("We have two factors (machine and shift), so this is a twoway problem.
          <p>Because we have ", rps, " repeated measurements we can ", cnrps, " test for 
          interaction.")
       if(n==1) atxt <- paste0(atxt, " The interaction plot indicates that there ", int, "
           interaction.") 
       if(n>1) atxt <- paste0(atxt, " We find a p value of ", pvals[3], ", so there ", int,
           "interaction.")
       if(int=="is no ") atxt <- paste0(atxt, " We will therefore fit an additive model.")
       atxt <- paste0(atxt, "<p>The p value of the test for the machines is ", a1, " and so we conclude that 
           the factor machine ", a2, " statistically significant.")
            
    }
    htxt <- ""  
    atx <- paste0(atxt, "<p>R command:", command, plt64)
    
    list(qtxt = paste0("<h5>",qtxt, "</h5><hr>", moodle.table(data), "</hr>"), 
        htxt = paste0("<h5>", htxt,"</h5>"),
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname)     
        
   
}   
