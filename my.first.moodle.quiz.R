my.first.moodle.quiz <- function() {
   category <- paste0("First Moodle Quiz")
   quizname <- "problem - " 
   x <- NULL
   
   n <- sample(10:20, size=2)
   qtxt <- paste0("Problem 1: ",n[1], " + ", n[2], " = ", nm(n[1]+n[2], eps=0), collapse="") 
   atxt <- paste0("Problem 1: ",n[1], " + ", n[2], " = ", n[1]+n[2], collapse="")

   n <- sample(101:199, size=1)
   m <- sample(round((n/2-20):(n/2+20)), size=1)
   qtxt <- paste(qtxt, "<p>",
      "Problem 2: In a survey of ", n, " people ", m, " said that 
         they prefer Coke over Pepsi. So the percentage of people who prefer Coke over Pepsi
         is ", nm(round(m/n*100, 1), eps=0.1),"% <br>(round answer to one digit behind decimal)", collapse="")

   htxt <- paste("Problem 2:  to find percentage devide count by total, multiply by 100. 
            <br>Don't forget to round!", collapse="")     
   atxt <- paste0(atxt, "<p>",
        "Problem 2: ", m, "/", n, "*100 = ", m/n*100, " rounded = ", round(m/n*100, 1), collapse="")
   qtxt <- paste(qtxt, "<p>",
        "Problem 3: This percentage is ", mc(c("higher", "the same", "lower"), 
         w=c(100,0,0))[[1]], " than 30%", collapse="")

   atxt <- paste(atxt, "<p>", "Problem 3: This percentage is higher than 30%", collapse="") 
            
   dta.tbl <- ""     
   if(!is.null(x)) 
      dta.tbl <-  paste("<hr>", moodle.table(x), collapse="")    
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>",
        dta.tbl),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
