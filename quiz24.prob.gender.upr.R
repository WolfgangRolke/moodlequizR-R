quiz24.prob.gender.upr <- function() {
   category <- paste0("Probability / gender-upr")
   quizname <- "problem - " 
   x <- NULL
   
   year=as.character(sample(2003:2013, 1))
   gender=sample(c("F", "M"), 1)
   x=table(upr$Gender, upr$Year)
   p=round(x[gender, year]/sum(x[,year]), 4)    
   qtxt <- paste0("If we randomly select a student from among those that applied to UPR in ",
     year, ", the probability that the student is ", ifelse(gender=="F", "female","male"), " is ", nm(p, 100, 0.01))       
   htxt <- ""
   atxt <- paste0("x=table(upr$Gender, upr$Year)
            <p>round(x[\"", gender, "\",\"", year,"\"]/sum(x[,\"", year,"\"]), 4)")
   
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
