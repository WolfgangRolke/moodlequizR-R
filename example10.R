example10=function() {
   category="MoodleR Examples / 10"
   quizname="problem -"
   n=50+1*sample(0:20, 1)
   Days=31:(30+n) # x variable with random sample size
   a=sample(90:110, 1)/10 # random coefficients
   b=sample(20:40, 1)/10
   Length=round(a+b*Days+rnorm(n,0, 20)) # y variable
   x=data.frame(Days=Days,Length=Length)
   res=as.list(1:5)
   res[[1]]= n
   res[[2]]= round(cor(Days,Length), 3)
   res[[3]]= round(coef(lm(Length~Days))[1], 1)
   res[[4]]= ifelse(coef(lm(Length~Days))[2]<0,1,2)
   plt=ggplot(data=x,aes(Days,Length))+geom_point()+geom_smooth(method="lm",se=FALSE)
   plt64=png64(plt)
   res[[5]]= round(coef(lm(Length~Days))[2], 1)
   qtxt =  paste0( "<p>In an experiment in agriculture a researcher grows a plant. On  ", res[[1]],"  consecutive days he measures the length of the plant.  He finds the correlation between Days and Length to be  ",moodleR::nm(res[[2]], w = c(100,80), eps = c(0,0.001))," .<p>The least square regression equation is Length =  ",moodleR::nm(res[[3]], w = c(100,80), eps = c(0,0.1)),"   ",moodleR::mc(c("-","+"), ifelse(1:2== res[[4]], 100, 0))$qmc,"    ",moodleR::nm(res[[5]], w = c(100,80), eps = c(0,0.1)),"  Days</p>" )
   atxt =  paste0( "<p>In an experiment in agriculture a researcher grows a plant. On  ", res[[1]],"  consecutive days he measures the length of the plant.  He finds the correlation between Days and Length to be  ", res[[2]]," .<p>The least square regression equation is Length =  ", res[[3]],"   ",c("-","+")[res[[4]]],"    ", res[[5]],"  Days</p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>",  plt64, moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
