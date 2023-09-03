hp_cor <- function() {
    category <- "Inference for proportion/Hypothesis test"
    quizname <- "hyptest - prop - " 
    g <- function() {
      pval <- one.sample.prop(x, n, piNull = par0, alternative = alt3, return.result = TRUE)
      pval1 <- one.sample.prop(x, n, piNull = par0, return.result = TRUE)
      command <-  paste0("one.sample.prop( x = ", x, ", n = ", n, ", piNull = ", par0, alt1, ")")      
      txts <- qatxts( par = "percentage", assumption = "none", 
                  alpha = alpha, h0num = par0,
                  alt = alt3, pval = c(pval, pval1))
      qtxt <- paste0(  txts$q[1:6] , collapse = "")
      atxt <- paste0( txts$a[1:6] , collapse = "")       
      atxt <- paste0( atxt, "<p>", "<p>R command: ", command, collapse = "")       
      list(qtxt = qtxt, atxt = atxt)  
    } 
    
    alternatives <- c("equal","less","greater")
    whichstory <- sample( 1:5, 1) 
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
    greaterlessorequal <- 1  
    if(runif(1)<0.66) greaterlessorequal <- ifelse(par0 > mean(x),2,3) 
    alt1 <- c("", ", alternative = \"less\"",", alternative = \"greater\"")[greaterlessorequal]
    alt2 <- c(" different from "," less than ", " more than ")[greaterlessorequal]
    alt3 <- c("equal","less","greater")[greaterlessorequal]   
    if(whichstory == 6) {
        varname <- "injuries"
         txt <- paste0("In a study done 10 years ago ", 100*par0,"% of the manatees in a certain area showed signs
         of injuries from boat accidents. A law was put in place to restrict access of boats to the area. In a recent survey ", 
         x, " of ", n, " manatees showed signs of injuries.  Test at the ", 100*alpha,"% 
                  level whether the  true percentage of injuries is now ", alt2, 100*par0,"% ")   
    }   
    if(whichstory == 1) {
         varname <- "widgets" 
         qtxt <- paste0("A company has a machine that makes widgets. Each widget is either red or blue. 
                           According to their contract ", 100*par0,"% of the widgets are supposed to be red.   
                            They randomly select ",n," widgets, of which ",x," are red. Test at the ", 100*alpha,"% 
                            level whether the  true percentage of red widgets is now ", alt2, 100*par0,"% ")               
    }  
    if(whichstory == 2) {
         varname <- "votes"
         qtxt <- paste0("Mr. Miller is running for major. In a survey last month he received ",100*par0,"% 
                                      of the votes. He decides to do another poll, and this time of the ",n," people 
                                      interviewed ",x," said they would vote for him. Test at the ", 100*alpha,"% level 
                                      whether his percentage of the votes is now ", alt2 , 100*par0,"% ") 
   }
   if(whichstory == 3) {
         varname <- "buyers"
         qtxt <- paste0("The Tasty Ice Cream company is considering to introduce a new flavor of 
                                  ice cream. Last month they did a survey and found that ",100*par0,"% of the tasters said 
                                  they would like the flavour. Since then they have made some changes, so they decide to 
                                  repeat the taste test. Of the ",n," people participating in the new test ",x," said they 
                                  would buy it. Test at the ", 100*alpha,"% level whether the percentage of
                                  people who will buy the ice cream is now ", alt2, 100*par0,"% ") 
   }
   if(whichstory == 4) {
         varname <- "car insurance policies"
         qtxt <- paste0("Some years ago an insurance company did a study of their policies and found 
                              that ",100*par0,"% of them where for cars. They randomly select ",n," of their current policies and 
                              find that ",x," of them are car insurances.  Test at the ", 100*alpha,"% level whether the percentage of
                                   car insurance policies is now ", alt2 ,100*par0,"%")      
   }                            
   if(whichstory == 5) {
         varname <- "failures"
         qtxt <- paste0("Some years ago a certain course at some University had a failure rate of ",100*par0,"%. 
                               To see whether there has been any change the University randomly selects ",n," students at the beginnig of the semester. 
                                At the end of the semester ",x," of them failed the course. Test at the ", 100*alpha,"% level whether the percentage of
                                 failures is now ", alt2 ,100*par0,"%")
   }
   list(qtxt = paste0("<h5>", qtxt, g()$qtxt,"</h5>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)     
}   
