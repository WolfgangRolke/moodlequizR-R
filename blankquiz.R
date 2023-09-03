 <- function() {
   category <- paste0("")
   quizname <- "problem - " 
   x <- NULL
   
   qtxt <- ""
   htxt <- ""
   atxt <- ""


   dta.tbl <- ""     
   if(!is.null(x)) 
      dta.tbl <-  paste("<hr>", moodle.table(x), collapse="")    
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        dta.tbl,
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
