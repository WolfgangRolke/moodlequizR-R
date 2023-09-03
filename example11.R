example11=function(num.zeros=0, digits=0) {
   category=paste("System of Equations / Zeros = ", num.zeros, ", Digits = ", digits)
   quizname="problem -"
   if(digits==0) x=1:9
   else x=1:99/10
   repeat {
     coef=sample(x, size=9,replace = TRUE)
     coef=coef*sample(c(-1,1),size=9, replace=TRUE, prob=c(3,1))
     if(num.zeros>0) coef[5]=0
     if(num.zeros>1) coef[c(5, 7)]=0
     A=matrix(coef, 3, 3)
     if(abs(det(A))>0.1) break
   }
   y=sample(-9:9, size=3, replace=TRUE)  
   z=matrix(round(solve(A, y), 1), 3, 1) # correct solution
   rownames(z)=c("x=","y=","z=")
   # So they display correctly in moodle we need to work a bit on the + and - signs
   c1=ifelse(A[,1]<0,"-","")
   c2=ifelse(A[,1]%in%c(0,1), "", abs(A)[,1])
   c3=ifelse(A[,1]==0, "", "x")
   c4=rep("", 3)
   c4[A[,2]<0]="-"
   c4[A[,2]>0]="+"
   c5=abs(A[,2])
   c5[A[,2]==0]=""
   c5[abs(A[,2])==1]=""
   c6=ifelse(A[,2]==0, "", "y")
   c7=rep("", 3)
   c7[A[,3]<0]="-"
   c7[A[,3]>0]="+"
   c8=abs(A[,3])
   c8[A[,3]==0]=""
   c8[abs(A[,3])==1]=""
   c9=ifelse(A[,3]==0, "", "z")
   res[[1]]= moodlequizR::qamatrix(out)$atxt
   res[[2]]= z
   qtxt =  paste0( "<p>Solve this system of equations: ", res[[1]]," <p> ",moodleR::qamatrix(res[[2]], points=100, precision=0)$qtxt," </p>" )
   atxt =  paste0( "<p>Solve this system of equations: ", res[[1]]," <p> ",moodleR::qamatrix(res[[2]])$atxt," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}
