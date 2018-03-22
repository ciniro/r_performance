for (i in 1:tamostral)
{
  Rprof('out.out')
  
  fc1(n)
  fc2(n)
  fc3(n)
  fc4(n)
  
  Rprof(NULL)
  proftable('out.out','cwin')
}