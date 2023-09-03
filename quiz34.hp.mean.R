quiz34.hp.mean <- function(type, whichstory, doGraph=FALSE, doAssumptions=FALSE) {
   if(missing(type)) type <- sample(c("Data", "Summary"), 1)
   if(missing(whichstory)) whichstory <- sample( 1:2, 1)
   category <- paste0("Inference for Mean / Hypothesis test / ", 
      type, " / Story = ", whichstory, 
      ifelse(doAssumptions, " (with Assumptions)", " (without Assumptions)"))
   quizname <- "problem - "

   whichstory <- sample( 1:5, 1) 
   alpha <- sample( c(0.01, 0.05, 0.1), 1)
   n <- sample(30:70,1)    
   if(whichstory == 1) {
         mu <- runif(1,10,100)
         par0 <- round(runif(1,mu-2,mu+2), 1)
         sigma <- runif(1,mu/10,mu/4)
         x <- round(rnorm(n,mu,sigma), 1)
         x[x<0] <- 0
         est <- round(c(mean(x), sd(x)), 2)
   }   
   if(whichstory == 2) {
         mu <- runif(1,5,20)                
         par0 <- round(runif(1,mu-1,mu+1), 1)                    
         x <- round(abs(rnorm(n,mu,10)), 1)
         x[x<0] <- 0
         est <- round(c(mean(x), sd(x)), 2)
   }   
    mu.x <- mean(x)
    sd.x <- sd(x)
    xtbl <- moodle.table(x) 

    which.alt <- 1  
    if(runif(1)<0.66) which.alt <- ifelse(par0 > mean(x), 2, 3) 
    alt.txt <- c("different from","less than","greater than")[which.alt]
    alt.com <- c("equal", "less", "greater")[which.alt]
    alt.sgn <- c("&ne;", "<", ">")[which.alt]
    if(which.alt==1) alt.mc <- mc(c("&ne;", "<", ">"), c(100, 0, 0))[[1]]
    if(which.alt==2) alt.mc <- mc(c("&ne;", "<", ">"), c(0, 100, 0))[[1]]
    if(which.alt==3) alt.mc <- mc(c("&ne;", "<", ">"), c(0, 0, 100))[[1]]
           
    pval1 <- one.sample.t(y=mu.x, n=n, shat=sd.x, mu.null = par0, return.result = TRUE)
    if(which.alt==1) pval <- pval1
    else {
      if(which.alt==2) pval <- one.sample.t(y=mu.x, n=n, 
                shat=sd.x, alternative="less", mu.null = par0, return.result = TRUE)
      else pval <- one.sample.t(y=mu.x, n=n, shat=sd.x, 
               alternative="greater", mu.null = par0, return.result = TRUE)
    }
    if(pval1< alpha) pval.mc <- mc(c("reject", "fail to reject"), c(100, 0))[[1]]
    else pval.mc <- mc(c("reject", "fail to reject"), c(0, 100))[[1]]
    
    if(type=="Data") command <-  paste0("<p>&nbsp;<p>one.sample.t(x, mu.null = ", 
              par0, ifelse(which.alt==1, "",  paste0(", alternative = \"", alt.com, "\"")), ")")   
    else command <-  paste0("<p>&nbsp;<p>one.sample.t(y = ", est[1], ", n = ", n, 
             ", shat = ", est[2], ",   mu.null = ", par0, ifelse(which.alt==1, "", 
        paste0(", alternative = \"", alt.com, "\"")), ")")   
    
    
    if(whichstory == 1) {
       if(type == "Data")  
         qtxt <- paste0("A company has a machine that makes widgets. They have a 
            contract with a customer that specifies a mean length of the widgets of ",par0," inches. 
            They randomly select some widgets and measure their lengths (in inches). The data is below. 
            Test at the ", 100*alpha,"% level whether the  true mean length of the widgets is
            ", alt.txt, " ", par0," inches")               
       else                    
         qtxt <- paste0("A company has a machine that makes widgets. They have a 
            contract with a customer that specifies a mean length of the widgets of ",par0,
            " inches. They randomly select ", n, " widgets and measure their lengths 
            (in inches). They find a mean length of ", est[1], " with a standard deviation of ",
             est[2], ". Test at the ", 100*alpha,"% level whether the  true mean length of the 
             widgets is ", alt.txt, " ", par0," inches")                               
    }  
    if(whichstory == 2) {
      if(type == "Data") 
         qtxt <- paste0("Last month a call center studied the time their representatives 
            spend on the phone with their customers and found that the mean length of the 
            calls was ",par0," minutes. They have since made some changes to their 
            customer service and want to know how it effected the lengths of the calls. 
            They randomly select ", n, " calls and time their lengths. The data is below. 
            Test at the ", 100*alpha,"% level whether the  true mean length of the calls is
                             ", alt.txt, " ", par0," minutes")               
      else
         qtxt <- paste0("Last month a call center studied the time their representatives 
         spend on the phone with their customers and found that the mean length of the 
         calls was ",par0," minutes. They have since made some changes to their 
         customer service and want to know how it effected the lengths of the calls. 
         They randomly select ", n, " calls and time their lengths. They find a mean number 
         of minutes of ", est[1], " with a standard deviation of ", est[2], ". Test at the ", 
         100*alpha,"% level whether the  true mean length of the calls is
         ", alt.txt, " ", par0," minutes")               
   }
   parlist <- c("&mu;", "&pi;", "&sigma;", "&lambda;", "&rho;", "other")
   mc1 <- mc(parlist, c(100, 0, 0, 0, 0, 0))[[1]]

   qtxt <- paste(qtxt, "<p>&nbsp;<p>H<sub>0</sub>: ", mc1, 
      " = ", nm(par0), " vs. H<sub>a</sub>: ", mc1, 
      alt.mc , nm(par0))
   atxt <- paste("<p>H<sub>0</sub>: &mu; = ", par0, " vs. H<sub>a</sub>: &mu; ", alt.sgn, par0)
   qtxt <- paste(qtxt, "<p>p value = ", nm(c(pval, pval1), c(100, 75), eps=0.01))   
   atxt <- paste(atxt, "<p>p value = ", ifelse(pval<0.001, "0.000", pval))
   qtxt <- paste(qtxt, "<p>we ", pval.mc, " the null hypothesis")   
   if(pval1<alpha)  atxt <- paste(atxt, "<p><nolink>p-value</nolink> < alpha, so we 
      reject the null hypothesis")
   else  atxt <- paste(atxt, "<p>p value > alpha, so we fail to reject the null hypothesis")
  
   htxt <- "Do you have the correct alternative hypothesis?
            <p>Do you have the correct parameter?"

   list(qtxt = paste0("<h5>",qtxt, "</h5>", ifelse(type=="Data", xtbl, "")), 
        htxt = paste0("<h5>", htxt, "</h5>"),
        atxt = paste0("<h5>", atxt, command, "</h5>"), 
        category = category, quizname = quizname)     
      
}   
