.First=function() {
     whichcomp <- strsplit(getwd(),"/")[[1]][3]
  load(paste0("c:/users/", whichcomp, 
              "/Dropbox/teaching/Resma3/Resma3.RData"),
              env=.GlobalEnv)
}   