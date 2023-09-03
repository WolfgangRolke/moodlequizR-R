hp.cat.ind <- function(type) {
   category <- "Categorical Data / Test for independence"
   quizname <- "problem - "
   g <- function() {
      if(type == 2) {
         res <- chi.ind.test(x, return.result = TRUE)
         pval <- res[1]
         command <- "chi.ind.test( xtbl )"
      }
      else {
         res <- chi.ind.test( table( x[,1], x[,2] ) , return.result = TRUE)
         pval <- res[1] 
         command <- "chi.ind.test( table( xtbl) )"
      }
                  
      txts <- qatxts( par = "association", assumption = ifelse( res[2] == 1, "ok", "not ok"), 
                  alpha = alpha, pval = pval)
      qtxt <- paste0(  txts$q[ c(1, 2, 5, 6) ] , collapse = "")
      atxt <- paste0( txts$a[ c(1, 2) ],
                      ifelse( res[2]==1, "OK (all expected cell counts > 5)",
                                         "Not OK (some expected cell counts < 5)"),   
                      txts$a[ c(5, 6) ] , collapse = "")       
      atxt <- paste0( atxt, "<p>", "<p>R command: ", command, "<p>", collapse = "")       
      list(qtxt = qtxt, atxt = atxt)  
      
   } 
   if(type == 0 ) type <- sample(1:2, 1)
   whichstory <- sample( 1:5, 1) 
   alpha <- sample( c(0.01, 0.05, 0.1), 1)
   n <- sample(30:70,1)
   rho <- runif(1, 0, 0.5)    
   if(whichstory == 1) {
         xnames <- c( "Male", "Female" )
         ynames <- c( "Yes", "No" )
         x <- gen_cont_table_data(n, A=xnames, B=ynames, 
            tbl = ifelse(type==2, TRUE, FALSE), rho=rho)
         if( type == 1) {
            colnames(x) <- c("Gender", "Improvement")   
         }
         else {
            colnames(x) <- ynames   
            rownames(x) <- xnames   
         }                
         xtbl <- moodle.table(x)
   }      
   if(whichstory == 2) {
         xnames <- c( "Male", "Female" )
         ynames <- c( "low", "medium", "high" )
         x <- gen_cont_table_data(n, A=xnames, B=ynames, 
            tbl = ifelse(type==2, TRUE, FALSE), rho=rho)
         if( type == 1) {
            colnames(x) <- c("Gender", "Income")   
         }
         else {
            colnames(x) <- ynames   
            rownames(x) <- xnames   
         }                
         xtbl <- moodle.table(x)
   } 
   if(whichstory == 3){
         xnames <- c("1", "2", "3")
         ynames <- c( "a", "b", "c" )
         x <- gen_cont_table_data(n, A=xnames, B=ynames, 
            tbl = ifelse(type==2, TRUE, FALSE), rho=rho)
         if( type == 1) {
            colnames(x) <- c("Water", "Growth")   
         }
         else {
            colnames(x) <- ynames   
            rownames(x) <- xnames   
         }                
         xtbl <- moodle.table(x)
   }
   if(whichstory == 4) {
         xnames <- c( "Morning", "Afternoon", "Evening" )
         ynames <- c( "Small", "Average", "High" )
         x <- gen_cont_table_data(n, A=xnames, B=ynames, 
            tbl = ifelse(type==2, TRUE, FALSE), rho=rho)
         if( type == 1) {
            colnames(x) <- c("Time", "Sales")   
         }
         else {
            colnames(x) <- ynames   
            rownames(x) <- xnames   
         }                
         xtbl <- moodle.table(x)
   }
    if(whichstory == 5) {
         xnames <- c( "Course 1", "Course 2" )
         ynames <- c( "A or B", "C", "D, F or W" )
         x <- gen_cont_table_data(n, A=xnames, B=ynames, 
            tbl = ifelse(type==2, TRUE, FALSE), rho=rho)
            
         if( type == 1) {
            colnames(x) <- c("Course", "Grade")   
         }
         else {
            colnames(x) <- ynames   
            rownames(x) <- xnames   
         }       
         xtbl <- moodle.table(x)
   }
    if(whichstory == 1) {
         qtxt <- paste0("A pharmaceutical company has developped a new treatment 
              for a certain disease. They want to find out whether the effect is 
              different for men and women. So they do a clinical trial and after 
              one month determine for each patient whether there has been a substancial
              improvement of not.  
              They find<hr>", xtbl, "<hr>Test at the ", 100*alpha,"% level whether there
              is a difference in the effects of the treatment for men and women")               
                                     
    }  
    if(whichstory == 2) {
         qtxt <- paste0("In a survey people were asked for their gender and  
              their income, categorized as low, medium or high. 
              The answers were<hr>", xtbl, "<hr>Test at the ", 100*alpha,"% level whether there
              is a difference in the incomes of men and women")               
   }
   if(whichstory == 3) {
         qtxt <- paste0("In an experiment in agriculture the researcher uses different
              amounts of water (coded as 1=low, 2=medium, 3=high) and observes the amount of 
              plant growth (coded as a=low, b=medium, c=high)
              He finds<hr>", xtbl, "<hr>Test at the ", 100*alpha,"% level whether there
              is a difference in the effects of the amount of water on the plant growth")               
                    
   }
   if(whichstory == 4) {
         qtxt <- paste0("A store wants to see whether sales are better in the morning, 
              the afternoon or in the evening. They categorize the sales as small, average or high. 
              They find<hr>", xtbl, "<hr>Test at the ", 100*alpha,"% level whether there
              is a difference in the sales depending on the time of day")               
   }                            
   if(whichstory == 5) {
         qtxt <- paste0("A professor teaches two sections of the same course. He wants to know
              whether there is a difference in the final grades.
              He finds<hr>", xtbl, "<hr>Test at the ", 100*alpha,"% level whether there
              is a difference in the final grades of the two courses")               
   }
   list(qtxt = paste0("<h5>",qtxt, g()$qtxt,"</h5>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)     
      
   
}   
