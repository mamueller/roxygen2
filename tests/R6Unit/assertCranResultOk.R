assertCranResultOk=function(l,msg="devtools::check failed"){
  ne<-length(l$errors)
  nw<-length(l$warnings)
  nn<-length(l$notes)
  tn<-ne+nw+nn
  cond<-(ne+nw+nn)>0
  print(cond)
  if(cond){
    print(l)
  }
  stopifnot(ne==0)
  stopifnot(nw==0)
  stopifnot(nn==0)
}
