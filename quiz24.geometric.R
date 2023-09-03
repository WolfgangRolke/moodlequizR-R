quiz24.geometric <- function() {
   category <- paste0("Probability / geometric")
   quizname <- "problem - " 
   x <- NULL
   
   k=sample(2:6, 1)
   j=sample(1:6, 1)
   p=1/6*(5/6)^(k-1)
   qtxt <- paste0("If you roll a fair die over and over again, the probability that the first ", j, " 
              appears on the ",  k, "<sup>th</sup> trial is ", nm(p, 100, 0.01))       
   htxt <- ""
   atxt <- paste0("Prob(1/6*(5/6)^(",k,"-1))"=",round(p, 4)")

        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
