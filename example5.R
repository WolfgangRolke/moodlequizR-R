example5=function() {
   category="MoodleR Examples / 5"
   quizname="problem -"
   n=80+1*sample(0:40, 1)
   p=matrix(c(20,31,16,18,34,14),3,2)
   dimnames(p)=list(c("Young","MiddleAge","Old"),c("Male","Female"))
   x=rcategorical(n, p)
   colnames(x)=c("Age","Gender")
   counts=table(x[,1],x[,2])
   res=as.list(1:2)
   res[[1]]= moodleR::qamatrix(counts)$atxt
   a=cbind(counts,apply(counts,1,sum))
   colnames(a)[3]="Total"
   b=round(a/apply(a,2,sum), 1)
   b=rbind(b, apply(b, 2, sum))
   rownames(b)[4]="Total"
   res[[2]]= b
   qtxt =  paste0( "<p>A survey asked people for their gender and their age. The counts are   ", res[[1]]," <p>The column percentages are  ",moodleR::qamatrix(res[[2]], points=c(100,80), precision=c(0,0.1))$qtxt," </p>" )
   atxt =  paste0( "<p>A survey asked people for their gender and their age. The counts are   ", res[[1]]," <p>The column percentages are  ",moodleR::qamatrix(res[[2]])$atxt," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
