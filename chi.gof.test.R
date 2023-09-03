chi.gof.test <- function() {
   category <- "Categorical Data / GoF Test"
   quizname <- "problem - "
   
   p <- c(sample(10:20, 1), sample(20:30, 1), sample(30:40, 1), sample(20:30, 1), sample(5:10, 1))
   p <- round(100*p/sum(p))
   print(p)
   n <- sample(50:70, 1)
   x <- table(c(1:5, sample(1:5, size=n, replace=TRUE, prob=p)))-1 
   out <- chi.gof.test(x, p/100, return.result=TRUE)
   if(out[2]==0) {
       repeat {
          p[1] <- p[1]+1
          p[5] <- p[5]-1
          out <- chi.gof.test(x, p/100, return.result = TRUE)
          print(c(p,out))   
          if(out[2]==1) break
       } 
   }
   out1 <- chi.gof.test(c(x[1:3], x[4]+x[5]), c(p[1:3], p[4]+p[5])/100, return.result=TRUE)
   if(out1[1] < 0.05) {
         qdecision <- "<p>Decision: {1:MC:~=Reject~Fail to reject} null hypothesis"
         adecision <- " reject "
   }      
   else {   
         qdecision <- "<p>Decision: {1:MC:~Reject~=Fail to reject} null hypothesis"   
         adecision <- " fail to reject "
   }      
   names(x) <- c("Calm", "Somewhat Agitated", "Agitated", "Upset", "Very Upset")
   p1 <- paste0(p[1], "%, ", p[2], "%, ", p[3], "%, ", p[4], "% and ", p[5], "%. ")
   
   qtxt <- paste0("<h5>A psychologist has developped a new rating system for the
   behaviour of people in stressful situations. They are either Calm, Somewhat Agitated,
   Agitated, Upset or Very Upset. His theory predicts that in a certain population the
   respective percentages are ", p1, " He randomy selects ", n, " people from this population who are
   in some stressful situation and rates them. He finds: <p>", moodle.table(x), "<p>
   Test at the 5% level whether the data agrees with his theory.
   <hr>Assumptions: {1:MC:ok~=not ok~none~can't tell~don't know}
   <p>p-value = {2:NM:%100%", out1[1], "~75%", out[1], "} 
   <p>", qdecision, "</h5>")
   
   atxt <-paste("<h5>Data is categorical (Calm, Upset, ...), so we use Pearsons'
   chisquare goodness of fit test. It says there is a problem with the expected 
   counts < 5, so we need to combine the categories Upset and Very Upset. The
   pvalue is ", out1[1], ", so we ", adecision, "the null hypothesis<p>
   R command: chi.gof.test(x, p)</h5>")   
        
   list(qtxt = qtxt, 
        atxt = atxt, 
        category = category, quizname = quizname) 
} 
