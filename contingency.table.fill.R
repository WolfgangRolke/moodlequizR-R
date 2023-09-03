contingency.table.fill <- function(num_row_col=c(4, 4)) {
    category <- paste("Categorical Data / Contingency Table / Fill in")
    quizname <- " problem -"
    nr <- num_row_col[1]
    nc <- num_row_col[2]
    counts <- matrix( sample( 50:100, size = nr*nc, replace = TRUE), nr, nc) 
    counts <- cbind(counts, apply(counts, 1, sum))
    counts <- rbind(counts, apply(counts, 2, sum))
    counts.questions=counts
    counts.questions[1,1]=nm(counts[1,1])  
    counts.questions[nr+1, 2]=nm(counts[nr+1, 2])  
    counts.questions[2, 3]=nm(counts[2, 3])   
    if(nc>2)  counts.questions[3, 4]=nm(counts[3, 4])    
    if(nc>3)  counts.questions[4, 2]=nm(counts[4, 2])    
    qtxt <- paste0("Fill in the missing numbers. (Last row and last column are totals)",
                 qamatrix(counts.questions)$atxt)
    atxt =  qamatrix(counts)$atxt
    list(qtxt = paste("<h5>", qtxt, "</h5>"), 
         htxt = "",
         atxt = paste("<h5>", atxt, "</h5>"),
         category = category, quizname = quizname)
}    
