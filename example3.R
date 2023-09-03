example3=function() {
   category="MoodleR Examples / 3"
   quizname="problem -"
   n=80+1*sample(0:20, 1)
   alpha = 1
   beta = 2
   x=rbeta(n, alpha, beta)
   x=round(x, 7)
   m=sample(1:3,1) # number of digits of data
   x=round(x*10^m, 2-m) # round the data
   out=cbind(min(x), quantile(x,0.25),median(x),quantile(x,0.75),max(x))
   colnames(out)=c("Minimum", "Q1", "Median","Q3","Maximum")
   rownames(out)=NULL
   res=as.list(1:2)
   # Use the qamatrix routine to generate the html code for moodle, display as is and not as a moodle question because that's already done by qamatrix.
   res[[1]]= moodleR::qamatrix(out, c(100,80), c(0, 1-m))$qtxt
   # nothing needed here
   res[[2]]= ""
   qtxt =  paste0( "<p>Find the five number summary of the data  ", res[[1]]," <p>",""," ", res[[2]]," </p>" )
   atxt =  paste0( "<p> ",""," <p># quotes are needed for paste command
", moodleR::qamatrix(out, c(100,80), c(0, m+2))$atxt, " ", res[[2]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
