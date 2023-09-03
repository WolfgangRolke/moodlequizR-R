example4=function() {
   category="MoodleR Examples / 4"
   quizname="problem -"
   n=50+1*sample(0:50, 1)
   x=sample(c("Freshman","Sophomore","Junior","Senior"), size=n, replace=TRUE, prob=c(200,154,122,108))
   res=as.list(1:2)
   res[[1]]= table(x)
   res[[2]]= round(table(x)/length(x)*100,1)
   qtxt =  paste0( "<p>The counts are  ",moodleR::qamatrix(res[[1]], points=100, precision=0)$qtxt," <p>The percentages are ",moodleR::qamatrix(res[[2]], points=c(100,80), precision=c(0,0.1))$qtxt," </p>" )
   atxt =  paste0( "<p>The counts are  ",moodleR::qamatrix(res[[1]])$atxt," <p>The percentages are ",moodleR::qamatrix(res[[2]])$atxt," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
