hp.prop <- function(whichstory) {
    if(missing(whichstory)) whichstory <- sample(1:6, 1)
    category <- paste("Inference for Proportion / Hypothesis Test / Story =  ", whichstory)
    quizname <- "problem - " 
    
    alpha <- sample( c(0.01, 0.05, 0.1), 1)
    n <- sample(100:1000,1)
    if(whichstory == 1)  p <- sample(c(4:16)*0.05, 1)
    if(whichstory == 2)  p <- sample(c(8:12)*0.05, 1)
    if(whichstory == 3)  p <- sample(c(4:8)*0.05, 1)                 
    if(whichstory == 4)  p <- sample(c(8:16)*0.025, 1)                 
    if(whichstory == 5)  {n <- sample(200:400,1);p <- sample(c(25:35)*0.01, 1)}                 
    if(whichstory == 6)  {n <- sample(50:100, 1);p <- sample( c(0.15,0.155,0.16,0.165,0.17,0.175), 1)}                    
    x <- rbinom(1,n,p)
    par0 <- round(runif(1,p-0.1,p+0.1),2)
    if(whichstory == 6) {
        par0 <- sample(0.2 + c(0:10)/100, 1)
        if(x > n*par0) x <- x - sample(2:5, 1)
    }
    
    which.alt <- 1  
    if(runif(1)<0.66) which.alt <- ifelse(par0 > mean(x), 2, 3) 
    alt.txt <- c("different from","less than","greater than")[which.alt]
    alt.com <- c("equal", "less", "greater")[which.alt]
    alt.sgn <- c("&ne;", "<", ">")[which.alt]
    if(which.alt==1) alt.mc <- mc(c("&ne;", "<", ">"), c(100, 0, 0))
    if(which.alt==2) alt.mc <- mc(c("&ne;", "<", ">"), c(0, 100, 0))
    if(which.alt==3) alt.mc <- mc(c("&ne;", "<", ">"), c(0, 0, 100))
    
    pval1 <- one.sample.prop(x, n, pi.null = par0, return.result = TRUE)
    if(which.alt==1) pval <- pval1
    else {
      if(which.alt==2) pval <- one.sample.prop(x, n, alternative="less", pi.null = par0, return.result = TRUE)
      else pval <- one.sample.prop(x, n, alternative="greater", pi.null = par0, return.result = TRUE)
    }
    if(pval1< alpha) pval.mc <- mc(c("reject", "fail to reject"), c(100, 0))
    else pval.mc <- mc(c("reject", "fail to reject"), c(0, 100))
    
    command <-  paste0("<p>&nbsp;<p>one.sample.prop( x = ", x, ", n = ", n, ", 
        pi.null = ", par0, ifelse(which.alt==1, "", 
        paste("alternative = \"", alt.com, "\"")), ")")      

    if(whichstory == 1) {
         varname <- "widgets" 
         qtxt <- paste0("A company has a machine that makes widgets. Each widget is either red or blue. 
                           According to their contract ", 100*par0,"% of the widgets are supposed to be red.   
                            They randomly select ",n," widgets, of which ",x," are red. Test at the ", 100*alpha,"% 
                            level whether the  true percentage of red widgets is now ", alt.txt, " ", 100*par0,"% ")               
    }  
    if(whichstory == 2) {
         varname <- "votes"
         qtxt <- paste0("Mr. Miller is running for major. In a survey last month he received ",100*par0,"% 
                                      of the votes. He decides to do another poll, and this time of the ",n," people 
                                      interviewed ",x," said they would vote for him. Test at the ", 100*alpha,"% level 
                                      whether his percentage of the votes is now ", alt.txt , " ",  100*par0,"% ") 
   }
   if(whichstory == 3) {
         varname <- "buyers"
         qtxt <- paste0("The Tasty Ice Cream company is considering to introduce a new flavor of 
                                  ice cream. Last month they did a survey and found that ",100*par0,"% of the tasters said 
                                  they would like the flavour. Since then they have made some changes, so they decide to 
                                  repeat the taste test. Of the ",n," people participating in the new test ",x," said they 
                                  would buy it. Test at the ", 100*alpha,"% level whether the percentage of
                                  people who will buy the ice cream is now ", alt.txt, " ", 100*par0,"% ") 
   }
   if(whichstory == 4) {
         varname <- "car insurance policies"
         qtxt <- paste0("Some years ago an insurance company did a study of their policies and found 
                              that ",100*par0,"% of them where for cars. They randomly select ",n," of their current policies and 
                              find that ",x," of them are car insurances.  Test at the ", 100*alpha,"% level whether the percentage of
                                   car insurance policies is now ", alt.txt ," ", 100*par0,"%")      
   }                            
   if(whichstory == 5) {
         varname <- "failures"
         qtxt <- paste0("Some years ago a certain course at some University had a failure rate of ",100*par0,"%. 
                               To see whether there has been any change the University randomly selects ",n," students at the beginnig of the semester. 
                                At the end of the semester ",x," of them failed the course. Test at the ", 100*alpha,"% level whether the percentage of
                                 failures is now ", alt.txt ," ", 100*par0,"%")
   }
   if(whichstory == 6) {
        varname <- "injuries"
         qtxt <- paste0("In a study done 10 years ago ", 100*par0,"% of the manatees 
         in a certain area showed signs of injuries from boat accidents. 
         A law was put in place to restrict access of boats to the area. In a recent survey ", 
         x, " of ", n, " manatees showed signs of injuries.  
         Test at the ", 100*alpha,"% level whether the  true percentage of injuries is now ", 
         alt.txt, " ", 100*par0,"% ")   
   }  
   
   parlist <- c("&mu;", "&pi;", "&sigma;", "&lambda;", "&rho;", "other")
   qtxt <- paste(qtxt, "<p>&nbsp;<p>H<sub>0</sub>: ", mc(parlist, c(0, 100, 0, 0, 0, 0)), 
      " = ", nm(par0), " vs. H<sub>a</sub>: ", mc(parlist, c(0, 100, 0, 0, 0, 0)), 
      alt.mc , nm(par0))
   atxt <- paste("<p>H<sub>0</sub>: &pi; = ", par0, " vs. H<sub>a</sub>: &pi; ", alt.sgn, par0)
   qtxt <- paste(qtxt, "<p>p value = ", nm(c(pval, pval1), c(100, 75), eps=0.01))   
   atxt <- paste(atxt, "<p>p value = ", pval1)
   qtxt <- paste(qtxt, "<p>we ", pval.mc, " the null hypothesis")   
   if(pval1<alpha)  atxt <- paste(atxt, "<p><nolink>p-value</nolink> < alpha, so we reject the null hypothesis")
   else  atxt <- paste(atxt, "<p>p value > alpha, so we fail to reject the null hypothesis")
   
   list(qtxt = paste0("<h5>", qtxt,"</h5>"), 
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname)     
}   
