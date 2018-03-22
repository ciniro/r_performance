for (i in 1:tamostral)
{
  Rprof("out.out")
  
  f1(n)
  f2(n)
  f3(n)
  f4(n)
  
  Rprof(NULL) 
  proftable("out.out",'iwin')
}