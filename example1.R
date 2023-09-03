example1=function() {
   category="Examples / 1"
   quizname="problem -"
   n=50+1*sample(0:50, 1)
   m=90+1*sample(0:20, 1)
   s=9+0.1*sample(0:20, 1)
   x=rnorm(n, m, s)
   x=round(x, 1)
   res=as.list(1:1)
   res[[1]]= round(mean(x), 2)
   qtxt =  paste0( "<p>The mean of the data is  ",moodleR::nm(res[[1]], w = c(100,80), eps = c(0,0.01))," </p>" )
   atxt =  paste0( "<p>The mean of the data is  ", res[[1]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
