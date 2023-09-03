chi.ind.test <- function(type) {
   category <- "Categorical Data / Test for Independence"
   quizname <- "problem - "
  
   n <- sample(50:80, 1)
   rho <- runif(1, -0.7, 0.7)
   whichstory <- sample(1:3, 1)
   if(whichstory==1) {
      repeat {
        tbl <- gen_cont_table_data(n, c("Supplier_1","Supplier_2"), c("Low","Medium","High"), TRUE, rho=rho)
        tbl <- tbl[ ,c("Low","Medium","High")]
        pval <- chi.ind.test(tbl, return.result=TRUE)     
        if(pval[2]==1) break
      }  
      qtxt <- paste0("<h5>A company has just received shipments of electronic parts from two suppliers. 
      They randomly select ", n, " parts and rate their quality as either low, medium or high. They 
      find <p>", moodle.table(tbl), "<p> 
      The hypothesis test to see whether the suppliers have different 
      ratings has a p value of {2:NM:=", pval[1], ":0.01}</h5>")
   }   
   if(whichstory==2) {
      repeat {
        tbl <- gen_cont_table_data(n, c("Treatment","Control"), c("Slow","Average","Fast"), TRUE, rho=rho)
        tbl <- tbl[ ,c("Slow","Average","Fast")]
        pval <- chi.ind.test(tbl, return.result=TRUE)     
        if(pval[2]==1) break
      }  
      qtxt <- paste0("<h5>A researcher wants to test the reflexes of people after they have
         participated in a treatment program as compared to those who have not in a control group. He
         randomly selects ", n, " people and measures their reflexes. He finds:<p>", moodle.table(tbl), "<p> 
         The hypothesis test to see whether there is a difference between the groups and
         their reflexes has a p value of {2:NM:=", pval[1], ":0.01}</h5>")
   }      
   if(whichstory==3) {
      grps <- c("Low","Average","High")
      repeat {
        tbl <- gen_cont_table_data(n, c("Male","Female"), grps, TRUE, rho=rho)
        tbl <- tbl[ ,grps]
        pval <- chi.ind.test(tbl, return.result=TRUE)     
        if(pval[2]==1) break
      }  
      qtxt <- paste0("<h5>An sociologist is doing a study on the relationship between the gender of 
         a person and and their general happiness, coded as Low, Average or High. She randomly selects ", 
         n, " people and determines their happiness. She finds:<p>", moodle.table(tbl), "<p> 
         The hypothesis test to see whether there is a difference between the genders and
         their happiness has a p value of {2:NM:=", pval[1], ":0.01}</h5>")
   }      
   atxt <- paste0("<h5>command: chi.ind.test<p>
                  p value = ", pval[1], "</h5>")
   
   list(qtxt = qtxt,  atxt = atxt, 
        category = category, quizname = quizname) }
