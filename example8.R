example8=function() {
   category="MoodleR Examples / 8"
   quizname="problem -"
   n=80+1*sample(0:20, 1)
   mu=sample(500:700,1)/10
   sig=sample(100:200,1)/10
   x=round(rnorm(n, mu+sig, sig), 1)
   # True mean is larger than that in null hypothesis, but not by much. So sometimes test will reject null, sometimes it will not.
   res=as.list(1:7)
   res[[1]]= mu
   res[[2]]= sig
   res[[3]]= mu
   res[[4]]= mu
   res[[5]]= round(t.test(x, mu=mu)$p.value, 4)
   res[[6]]= ifelse(t.test(x, mu=mu)$p.value<0.1, 1, 2)
   res[[7]]= ifelse(t.test(x, mu=mu)$p.value<0.1, 2, 1)
   qtxt =  paste0( "<p>Here are observations from a normal distribution with mean  ", res[[1]],"  and standard deviation  ", res[[2]]," .<p>The hypothesis test $$H_0: \\mu =$$   ", res[[3]]," vs  $$H_a: \\mu \\ne$$ ", res[[4]]," has a p value of  ", res[[5]]," <p>If we test at the 10% level we should  ",moodleR::mc(c("reject"," fail to reject"), ifelse(1:2== res[[6]], 100, 0))$qmc,"  the null hypothesis<p>In this test we  ",moodleR::mc(c("made the right decision"," comitted the type I error"," comitted the type II error"), ifelse(1:3== res[[7]], 100, 0))$qmc," </p>" )
   atxt =  paste0( "<p>Here are observations from a normal distribution with mean  ", res[[1]],"  and standard deviation  ", res[[2]]," .<p>The hypothesis test $$H_0: \\mu =$$   ", res[[3]]," vs  $$H_a: \\mu \\ne$$ ", res[[4]]," has a p value of  ", res[[5]]," <p>If we test at the 10% level we should  ",c("reject"," fail to reject")[res[[6]]],"  the null hypothesis<p>In this test we  ",c("made the right decision"," comitted the type I error"," comitted the type II error")[res[[7]]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
