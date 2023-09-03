quiz10.contingency.table <- function(num_row_col=c(3, 3), which, with.marginals=TRUE) {
    if(missing(which))
      which <- sample(2:3, 1)
    category <- paste("Categorical Data / Contingency Table / ", 
       c("Grand Total", "Row Total", "Column Total")[which])
    quizname <- " problem -"
    sp <- "<font size=\"+1\">&nbsp;&nbsp;"  
    if( missing(num_row_col) ) {
        nr <- sample( 2:3, 1)        
        nc <- sample( 2:3, 1)
        if(nr+nc == 6) nr <- 2
    }    
    else { nr <- num_row_col[1]; nc <- num_row_col[2]}
    perctype <- c("grand total", "row total", "column total")
    counts <- matrix( sample( 5:20, size = nr*nc, replace = T), ncol = nc) 
    counts <- cbind(counts, apply(counts, 1, sum))
    counts <- rbind(counts, apply(counts, 2, sum))
    
    if(which == 1) 
        perc <- round(counts/counts[nr+1, nc+1]*100, 1)
    if(which == 2) 
        perc <- round(counts/counts[, nc+1]*100, 1)
    if(which == 3) 
        perc <- round( t(t(counts)/counts[nr+1,]*100), 1)
    qtxt <- paste0("Add the percentages based on the <b>", perctype[which], "</b>",
        ifelse(with.marginals, " as well as the marginals", ""), 
        ".<p> Round all percentages to one digit behind the comma.</h5>
        <p><table Border=1>")
    for(i in 1:nr) {
        qtxt <- paste0(qtxt, "<tr>")        
        for(j in 1:nc) 
            qtxt <- paste0(qtxt, "<td>", sp, counts[i, j], "&nbsp;&nbsp;({:NM:%100%", perc[i, j], ":0.05}%)</td>")
        if(with.marginals)
           qtxt <- paste0(qtxt, "<td>", sp, "{:NM:%100%", counts[i, nc+1], "}&nbsp;&nbsp;({:NM:%100%", perc[i, nc+1], ":0.05}%)</td>")
        qtxt <- paste0(qtxt, "</tr>")
    }
    qtxt <- paste0(qtxt, "<tr>")
    if(with.marginals)
        for(j in 1:(nc+1)) 
            qtxt <- paste0(qtxt, "<td>", sp, "{:NM:%100%", counts[nr+1, j], "}&nbsp;&nbsp;({:NM:%100%", perc[nr+1, j], ":0.05}%)</td>")
    qtxt <- paste0(qtxt,"</tr></table>")
    
    nma <- ifelse(with.marginals, 1, 0)
    htxt <- "don't forget the rounding!"
    atxt <- "<table Border=1>"        
    for(i in 1:(nr+nma)) {
        atxt <- paste0(atxt, "<tr>")       
        for(j in 1:(nc+nma)) 
            atxt <- paste0(atxt, "<td>", sp, counts[i, j], "&nbsp;&nbsp;(", perc[i, j], "%)</td>")    
        atxt <- paste0(atxt, "</tr>")
    }
    atxt <- paste0(atxt,"</table>")
    list(qtxt = paste("<h5>", qtxt, "</h5>"), 
         htxt = paste("<h5>", htxt, "</h5>"),
         atxt = paste("<h5>", atxt, "</h5>"),
         category = category, quizname = quizname)
}    
