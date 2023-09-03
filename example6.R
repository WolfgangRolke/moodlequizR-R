example6=function() {
   category="MoodleR Examples / 6"
   quizname="problem -"
   n=50+1*sample(0:50, 1)
   m=90+1*sample(0:20, 1)
   s=9+0.1*sample(0:20, 1)
   x=rnorm(n, m, s)
   x=round(x, 1)
   alpha=sample(c(90,99), 1) # random confidence level
   cl=t.test(x, conf.level=alpha/100)$conf.int
   res=as.list(1:3)
   res[[1]]= alpha
   res[[2]]= round(cl[1], 2)
   res[[3]]= round(cl[2], 2)
   qtxt =  paste0( "<p>A  ", res[[1]]," % confidence interval for the mean is given by (# comma to appear in moodle quiz at the right place
 ",moodleR::nm(res[[2]], w = c(100,80), eps = c(0,0.01))," , <p># don't forget closing parenthesis
 ",moodleR::nm(res[[3]], w = c(100,80), eps = c(0,0.01))," )</p>" )
   atxt =  paste0( "<p>A  ", res[[1]]," % confidence interval for the mean is given by (# comma to appear in moodle quiz at the right place
 ", res[[2]]," , <p># don't forget closing parenthesis
 ", res[[3]]," )</p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
