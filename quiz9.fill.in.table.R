quiz9.fill.in.table <- function(whichstory) {
   category <- paste0("Categorical Data / One Variable - Table / Story = ", whichstory)
   quizname <- "problem - " 
   if(whichstory==1) { 
      varname <- "Size"
      vals <- c("Small", "Medium", "Large", "X-Large")
      n <- sample(50:75, 1)
      x <- sample(vals, size=n, replace=T, prob=c(1, 2, 4, 3))
      tbl.x <- table(factor(x, ordered=T, levels=vals))
      perc.total <- sum(round(tbl.x/n*100, 1))
      qtxt <- "A store is randomly selecting information on the sizes of sold 
      t-shirts. The data is below. Fill in the information in the table."
   }
   if(whichstory==2) { 
      varname <- "Blood Pressure"
      vals <- c("Low", "Medium", "High")
      n <- sample(105:125, 1)
      x <- sample(vals, size=n, replace=T, prob=c(1, 4, 3))
      tbl.x <- table(factor(x, ordered=T, levels=vals))
      perc.total <- sum(round(tbl.x/n*100, 1))
      qtxt <- "A hospital is randomly selecting patients and checking their blood pressure. 
      The data is below. Fill in the information in the table."
   }  
   if(whichstory==3) { 
      varname <- "Grades"
      vals <- c("A", "B", "C", "D", "F")
      n <- sample(110:145, 1)
      x <- sample(vals, size=n, replace=T, prob=c(1, 2, 3, 1, 3))
      tbl.x <- table(factor(x, ordered=T, levels=vals))
      perc.total <- sum(round(tbl.x/n*100, 1))
      qtxt <- "A Professor is randomly selecting students from a course 
      and checking their final grades. 
      The data is below. Fill in the information in the table."
   }   
   qtxt <- paste0(qtxt, "<p><table><tr><th>", varname,
          "</th><th>Counts</th><th>Percentages</th></tr>")
   atxt <- paste0("<table><tr><th>", varname, "</th><th>Counts</th><th>Percentages</th></tr>")
   for(i in 1:length(vals)) {
      qtxt <- paste0(qtxt, "<tr><td>", vals[i], "</td>
              <td> ", nm(tbl.x[i]),   "</td>
              <td> ", nm(round(tbl.x[i]/n*100, 1), eps=0.1), "</tr>")
      atxt <- paste0(atxt, "<tr><td>", vals[i], "</td>
              <td> ", tbl.x[i],   "</td>
              <td> ", round(tbl.x[i]/n*100, 1), "</tr>")   
   }
   qtxt <- paste0(qtxt, "<tr><th>Total</th>
             <td> ", nm(n),   "</td>
              <td> ", nm(perc.total), "</tr>")
   atxt <- paste0(atxt, "<tr><th>Total</th>
              <td> ", n,   "</td>
              <td> ", perc.total, "</tr>")    
   qtxt <- paste0(qtxt, "</table>")     
   atxt <- paste0(atxt, "</table>")
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(x)), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 
