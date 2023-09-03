quiz28.pvalue <- function() {
   Less <- sample(c(TRUE, FALSE), 1) 
   category <- paste0("Hypothesis Testing   / p value / Less=", Less)
   quizname <- "problem - " 
 
   repeat {   
      n <- 5*sample(5:10, 1)
      mu1 <- sample(c(8, 8.25, 8.5, 8.75, 9, 11, 11.25, 11.5, 11.75, 12), 1) 
      sig <- 2*sample(0:3, 1)+1
      alpha <- sample(c(1, 5, 10), 1)/100
      which <- sample(1:3, 1)
      alt <- c("less", "two.sided", "greater")[which]
      alt1 <- c("less then", "different from", "greater then")[which]
      alt.sgn <- c("<", "<>", ">")[which]
      pval <- one.sample.t(y=mu1, n=n, shat=sig, mu.null=10, 
         alternative=alt, return.result=TRUE)
      if(Less & pval<alpha) break
      if(!Less & pval>alpha) break
   }    
   if(Less) mc1 <- mc(c("reject", "fail to reject"), w=c(100, 0))
   else mc1 <- mc(c("reject", "fail to reject"), w=c(0, 100))
   if(Less) mc2 <- mc(c("is", "is not"), w=c(100, 0))
   else mc2 <- mc(c("is", "is not"), w=c(0, 100))   
   qtxt <- paste0("We have done an experiment and then a hypothesis test of 
      <p>H<sub>0</sub>: &mu;=10 vs H<sub>a</sub>: &mu; ", alt.sgn, " 10.
      <p>We found a p value of ", pval, ". 
      <p>Therefore if we test at the ", 100*alpha, "% level we ", mc1[[1]], " the null hypothesis.
      <p>The true mean ", mc2[[1]], " statistically significantly ", 
      alt1, " 10")
         
   htxt <- "when do we reject a null hypothesis?"
   atxt <- paste0("p value=", pval, ifelse(Less, "<", ">"), alpha, " = &alpha;
          <p>Therefore we ", mc1[[2]], " the null hypothesis")
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        htxt = paste0("<h5>", htxt, "</h5>"),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
