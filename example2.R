example2=function() {
   category="MoodleR Examples / 2"
   quizname="problem -"
   n=50+1*sample(0:50, 1)
   m=90+1*sample(0:20, 1)
   s=9+0.1*sample(0:20, 1)
   x=rnorm(n, m, s)
   x=round(x, 1)
   res=as.list(1:3)
   res[[1]]= round(mean(x), 2)
   res[[2]]= round(median(x), 2)
   res[[3]]= ifelse(mean(x)>median(x), 1, 2)
   qtxt =  paste0( "<p>The mean is ",moodlequizR::nm(res[[1]], w = c(100,80), eps = c(0,0.01))," <p>The median is  ",moodlequizR::nm(res[[2]], w = c(100,80), eps = c(0,0.01))," <p>The mean  ",moodlequizR::mc(c("is "," is not"), ifelse(1:2== res[[3]], 100, 0))$qmc,"  larger than the median</p>" )
   atxt =  paste0( "<p>The mean is ", res[[1]]," <p>The median is  ", res[[2]]," <p>The mean  ",c("is "," is not")[res[[3]]],"  larger than the median</p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
