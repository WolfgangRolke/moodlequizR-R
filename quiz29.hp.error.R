quiz29.hp.error <- function() {
   quizname <- "problem - " 
   n <- sample(5*10:20, 1) 
   mu1 <- round(runif(1, 50, 100), 1)
   mu2 <- round(ifelse(runif(1)<0.5, mu1, mu1+sample(c(-1,1), 1)*runif(1,1,3)), 1)
   sig <- round(runif(1, 5, 15), 1)   
   x <- round(rnorm(n, mu1, sig), 1)
   pval <- one.sample.t(x, mu.null=mu2, return.result=TRUE)
   if(pval<0.001) pval <- "0.000"
   if(mu1==mu2 & pval<0.1) {
      qcordec <- "{4:MC:~Made the correct decision~=committed the type I error~comitted the type II error}"
      atxt <- paste0("True mean is ", mu1, ", so null hypothesis is true. We falsely rejected it, so
      we committed the type I error")
      category <- "Hypothesis Testing / What Error? / Type I"
   }   
   if(mu1==mu2 & pval>0.1) {
      qcordec <- "{4:MC:~=Made the correct decision~committed the type I error~comitted the type II error}"
      atxt <- paste0("True mean is ", mu1, ", so null hypothesis is true. We failed to reject it, so
      we made the correct decision")
            category <- "Hypothesis Testing / What Error? / No Error"
   }   
   if(mu1!=mu2 & pval>0.1) {
      qcordec <- "{4:MC:~Made the correct decision~committed the type I error~=comitted the type II error}"
      atxt <- paste0("True mean is ", mu1, ", not ", mu2, ", so null hypothesis is false. We falsely 
      failed to reject it, so we committed the type II error")
            category <- "Hypothesis Testing / What Error? /  Type II"
   }   
   if(mu1!=mu2 & pval<0.1) {
      qcordec <- "{4:MC:~=Made the correct decision~committed the type I error~comitted the type II error}"
      atxt <- paste0("True mean is ", mu1, ", not ", mu2, ", so null hypothesis is false. We rejected it, so
      we made the correct decision")
            category <- "Hypothesis Testing / No Error"
   }   
   qtxt <- paste0("<h5>Here are ", n ," observations from a normal distribution with
      mean ", mu1, " and standard deviation ", sig, "<hr>",  paste(x, collapse=" "),  "<hr> 
      The hypothesis test H<sub>0</sub>: &mu; = ", mu2, " vs H<sub>a</sub>: &mu; &ne; ", mu2, "
      has a p-value = ", pval, "<p>
      What is the decision on the test at the 10% level? ",
      ifelse(pval<0.1, "{1:MC:~=Reject~Fail to reject}", "{1:MC:=Reject~=Fail to reject}"), "<p>
      In this test we ", qcordec, "</h5>")
   
   htxt <- "<h5>When does one commit an error?</h5>"
   atxt <-paste("<h5>", atxt,"</h5>")   
        
   list(qtxt = qtxt, htxt = htxt, 
        atxt = atxt, 
        category = category, quizname = quizname) 
} 
