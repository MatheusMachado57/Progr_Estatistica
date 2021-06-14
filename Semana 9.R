# Semana 9
# Q1

# a
# complexidade linear - O(n) pois para fazer a soma do vetor, o algoritmo
# precisa percorrer o vetor e ir acumulando a soma. Para percorrer o vetor há 
# um gasto de tamanho n = tamanho do vetor.
SomaVetor = function(v){
  soma    = 0
  n       = length(v)
  for(i in 1:n){soma=soma+v[i]}
  return(soma)
}

# grafico representando o tempo computacional
tgasto_a  = NULL
n         = seq(1,1000000,by=10000)  #o i-esimo elemento representa a dimensao do 

# vetor vet para a iteracao i
length(n)
for(i in 1:length(n)){
  print(i)
  vet         = rep(1,n[i])
  tinicial    = Sys.time()
  SomaVetor(vet)
  tfinal      = Sys.time()
  tgasto_a[i] = tfinal-tinicial
}
plot(n, tgasto_a,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_a[length(n)]/n[length(n)]
lines(n,n*cte,col="red", lwd=2)

# b
# complexidade linear - O(n) pois para fazer a soma do vetor, o algoritmo
# precisa percorrer o vetor e ir acumulando a soma. Para percorrer o vetor há 
# um gasto de tamanho n = tamanho do vetor.

SomaVetorR = function(v){
  n = length(v)
  if(n==1){return(v)}
  return(v[n] + SomaVetorR(v[-n]))
}
tgasto_b  = NULL
n         = seq(1,1200,by=10)   
length(n)

for(i in 1:length(n)){
  print(i)
  vet           = rep(1,n[i])
  tinicial      = Sys.time()
  SomaVetorR(vet)
  tfinal        = Sys.time()
  tgasto_b[i]   = tfinal-tinicial
}
plot(n, tgasto_b,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)

# c
# complexidade quadratica pois ha 2 loops, um dentro do outro.
SomaMatriz = function(A,B){
  l = nrow(A)
  c = ncol(A)
  if(l!=c)stop("Erro")
  if((nrow(B)!=l)||(ncol(B)!=c))stop("Erro")
  soma = matrix(0,l,c)
  for(i in 1:l){
    for(j in 1:c){
      soma[i,j] = A[i,j]+B[i,j]
    }
  }
  return(soma)
}
tgasto_c  = NULL
n         = seq(1,1000,by=10)   
length(n)

for(i in 1:length(n)){
  print(i)
  A           = matrix(1,n[i],n[i])
  B           = matrix(1,n[i],n[i])
  tinicial    = Sys.time()
  SomaMatriz(A,B)
  tfinal      = Sys.time()
  tgasto_c[i] = tfinal-tinicial
}
plot(n, tgasto_c,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_c[length(n)]/(n[length(n)]^2)
lines(n,n^2*cte,col="red", type="l",lwd=2)

# d
# complexidade quadratica pois ha 2 loops, um dentro do outro.
BuscaMatriz = function(A,k){
  l = nrow(A)
  c = ncol(A)
  if(l!=c)stop("Erro")
  posicoes=NULL
  for(i in 1:l){
    for(j in 1:c){
      if(k==A[i,j]) posicoes=rbind(posicoes, c(i,j))
    }
  }
  return(posicoes)
}
tgasto_d  = NULL
n         = seq(1,1000,by=10) 
length(n)

for(i in 1:length(n)){
  print(i)
  A           = matrix(1,n[i],n[i])
  tinicial    = Sys.time()
  BuscaMatriz(A,0)    #pior caso: elemento k nao esta presente na matriz A
  tfinal      = Sys.time()
  tgasto_d[i] = tfinal-tinicial
}
plot(n, tgasto_d,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_d[length(n)]/(n[length(n)]^2)
lines(n,n^2*cte,col="red", type="l", lwd=2)

# e
# complexidade linear pois ha um loop
Prod_int = function(v,u){
  if( (class(v)!="numeric") || (class(u)!="numeric") )stop("Os argumentos precisam ser vetores numéricos.")
  n = length(v)
  m = length(u)
  if(n!=m)stop("Os vetores precisam ter o mesmo tamanho.")
  p = 0
  for(i in 1:n){    p = p + v[i]*u[i]  }
  return(p)
}
tgasto_e  = NULL
n         = seq(1,1000000,by=10000)  
length(n)

for(i in 1:length(n)){
  print(i)
  v             = rep(1,n[i])
  u             = rep(1,n[i])
  tinicial      = Sys.time()
  Prod_int(v,u)
  tfinal        = Sys.time()
  tgasto_e[i]   = tfinal-tinicial
}
plot(n, tgasto_e,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_e[length(n)]/n[length(n)]
lines(n,n*cte,col="red", type="l", lwd=2)

# f
# complexidade quadratica pois ha 2 loops, um dentro do outro.
Produto_Av = function(A,v){
  if(class(A)!="matrix") stop("O primeiro argumento deve ser uma matriz.")
  if(class(v)!="numeric") stop("O segundo argumento deve ser um vetor.")
  n = nrow(A)
  m = ncol(A)
  k = length(v)
  if(k!=m)stop("O tamanho do vetor precisa ser igual ao número de colunas da matriz.")
  w = NULL
  for(i in 1:n){
    ai = A[i,]
    w[i] = Prod_int(ai, v) 
  }
  return(w)
}

tgasto_f  = NULL
n         = seq(1,1000,by=10) 
length(n)

for(i in 1:length(n)){
  print(i)
  v           = rep(1,n[i])
  A           = matrix(1,n[i],n[i])
  tinicial    = Sys.time()
  Produto_Av(A,v)
  tfinal      = Sys.time()
  tgasto_f[i] = tfinal-tinicial
}
plot(n, tgasto_f,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_f[length(n)]/(n[length(n)]^2)
lines(n,n^2*cte,col="red", type="l", lwd=2)

# g
EhSimetrica = function(A){
  if(class(A)!="matrix") stop("O argumento deve ser uma matriz.")
  n = nrow(A)
  m = ncol(A)
  if(n!=m){return(FALSE)}else{    
    for(i in 1:(n-1)){
      for(j in (i+1):m){
        if(A[i,j] != A[j,i]){return(FALSE)}
      }
    }
  }
  return(TRUE)
}
tgasto_g  = NULL
n         = seq(2,1000,by=10)  
length(n)

for(i in 1:length(n)){
  print(i)
  A             = matrix(1,n[i],n[i])
  tinicial      = Sys.time()
  EhSimetrica(A)
  tfinal        = Sys.time()
  tgasto_g[i]   = tfinal-tinicial
}
plot(n, tgasto_g,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)
cte = tgasto_g[length(n)]/(n[length(n)]^2)
lines(n,n^2*cte,col="red", type="l", lwd=2)

# h
Permutacoes = function(dados){
  n = length(dados)
  if(n==1){return(dados)}
  L = factorial(n)
  saida = NULL
  for(i in 1:n){
    saida = rbind(saida, cbind(dados[i], Permutacoes(dados[-i])   ) )       
  }
  return(saida)
}
tgasto_h  = NULL
n         = seq(1,10,by=1)  
length(n)

for(i in 1:length(n)){
  print(i)
  A             = seq(1,n[i],by=1)
  tinicial      = Sys.time()
  Permutacoes(A)
  tfinal        = Sys.time()
  tgasto_h[i]   = tfinal-tinicial
}
plot(n, tgasto_h,ylab="Tempo",xlab="n", type="l",
     lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)


# Q2
# a
Seq1 = function(n){
  if( (length(n)>1) || (n%%1!=0) || (n<0) ) stop("Entre com um número natural")
  if(n==0) return (1)
  return(2*Seq1(n-1) + n)
}

Seq2 = function(n){
  if( (length(n)>1) || (n%%1!=0) || (n<0) ) stop("Entre com um número natural")
  if(n==0) return (1)
  return(Seq2(n-1)+Seq2(n-1) + n)
  
}

# b
n = 15
tinicial = Sys.time()
Seq1(n)
tfinal = Sys.time()
tfinal-tinicial

tinicial = Sys.time()
Seq2(n)
tfinal = Sys.time()
tfinal-tinicial

# c
x11()
tgasto = NULL
for(i in 1:20){
  tinicial    = Sys.time()
  Seq1(i)
  tfinal      = Sys.time()
  tgasto[i]   = tfinal-tinicial
}
plot(1:20,tgasto)

x11()
tgasto2 = NULL
for(i in 1:20){
  tinicial      = Sys.time()
  Seq2(i)
  tfinal        = Sys.time()
  tgasto2[i]     = tfinal-tinicial
}
plot(1:20, tgasto2)

plot(1:20, tgasto2-tgasto)

# Complexidade:

par(mfrow=c(2,2))    
plot(seq(1,20,by=1),tgasto, xlab="n",ylab="tempo",main="Seq1")    
plot(seq(1,20,by=1),tgasto2, xlab="n",ylab="tempo",main="Seq2")    

par(mfrow=c(1,1))    
plot(1:20, tgasto2-tgasto, xlab="n",ylab="diferença entre os tempos",main="Seq2 - Seq1")

# Q3
Fibonacci = function(n){
  if( (n%%1!=0) || (n<=0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(1)} 
  return(Fibonacci(n-1)+Fibonacci(n-2))
}
N         = 30
tgastoF   = NULL    
for(i in 1:N){
  tinicial  = Sys.time()
  Fibonacci(i)
  tfinal    = Sys.time()
  tgastoF[i] = tfinal-tinicial
}
par(mfrow=c(2,2))    
x   = seq(1,N,by=1)
cte = tgastoF[length(x)]/(2^x[length(x)]) 
plot(x,tgastoF, xlab="n",ylab="tempo",lwd=2,bty="n",cex.lab=1.4,cex.axis=1.4)    
lines(x, cte*(2^x), col="red",lwd=2)
plot(x, 2^x, xlab="n",ylab="ordem",bty="n",cex.lab=1.4,cex.axis=1.4, lwd=2)    

# Q4
# a
FibonacciS = function(n){
  if((n==1)||(n==2))return(1)
  penultimo = 1
  ultimo    = 1
  for(i in 3:n)
  {
    atual     = ultimo+penultimo
    penultimo = ultimo
    ultimo    = atual
  }
  return(atual)
}
N         = seq(1,300000,l=100)
tgastoFs   = NULL    

for(i in 1:length(N)){
  print(i)
  tinicial  = Sys.time()
  FibonacciS(N[i])
  tfinal    = Sys.time()
  tgastoFs[i] = tfinal-tinicial
}
par(mfrow=c(1,1))    
plot(N,tgastoFs, xlab="n",ylab="tempo")    

N = c(1,2001,4001,6001,40001)    
tgastoFs2   = NULL   

for(i in 1:length(x)){
  tinicial  = Sys.time()
  FibonacciS(N[i])
  tfinal    = Sys.time()
  tgastoFs2[i] = tfinal-tinicial
}
par(mfrow=c(1,1))    
plot(N,tgastoFs2, xlab="n",ylab="tempo")    

# complexidade linear

# Q5
OrdenaBolhaRec = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(v)}
  for(i in 1:(n-1)){
    if(v[i]>v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
    }
  }
  w  = v[-n]
  w0 = OrdenaBolhaRec(w) 
  return(c(w0,v[n]))
}

OrdenaRapidoRec = function(v){
  if(!is.vector(v)) stop("o argumento de entrada precisa ser um vetor")
  n = length(v)
  if(n==1)return(v)
  for(i in 2:n){
    j       = n
    achoui  = F
    if(v[1]<v[i]){
      achoui = T
      while(v[1]<v[j]){j=j-1}
      if(i<j){
        aux   = v[j]
        v[j]  = v[i]
        v[i]  = aux
      }else{
        aux   = v[1]
        v[1]  = v[j]
        v[j]  = aux
        break
      }
    }
  }
  if(!achoui){
    aux   = v[1]
    v[1]  = v[n]
    v[n]  = aux
  }
  
  if(j>1){  
    we  = v[1:(j-1)] 
    weo = OrdenaRapidoRec(we)
    vo  = c(weo, v[j])
  }else{vo = v[j]}
  
  if(j<n){
    wd = v[(j+1):n]
    wdo = OrdenaRapidoRec(wd)
    vo  = c(vo, wdo)
  }
  return(vo)
}
n = seq(1,1000,by=10)    
tgastoO   = NULL    
for(i in 1:length(n)){
  print(i)
  vet       = seq(n[i], 1, by=-1) 
  tinicial  = Sys.time()
  OrdenaBolhaRec(vet)
  tfinal    = Sys.time()
  tgastoO[i] = tfinal-tinicial
}

tgastoO2   = NULL    
for(i in 1:length(x)){
  print(i)
  vet       = seq(1, n[i], by=1) 
  tinicial  = Sys.time()
  OrdenaRapidoRec(vet)
  tfinal    = Sys.time()
  tgastoO2[i] = tfinal-tinicial
}

par(mfrow=c(2,2))    
plot(n,tgastoO, xlab="n",ylab="tempo",main="Bolha")    
plot(n,tgastoO2, xlab="n",ylab="tempo", main="Rápida")    
plot(n,tgastoO2-tgastoO, xlab="n",ylab="tempo",main="Rápida - Bolha")    

# Medindo o tempo gasto
######################################

# ptm = proc.time()
# proc.time() - ptm

# gerando valores
g = rnorm(100000)
h = NULL    

# inicializando o relogio
ptm = proc.time()    

#somando um ao elemento g_i através de um loop
for (i in 1:100000){
  h[i] = g[i] + 1
}

# parando o relogio
proc.time() - ptm

# fazendo a mesma funcao de outra forma  
ptm = proc.time()
h = g + 1
proc.time() - ptm

# The values presented (user, system, and elapsed) will be defined by your 
# operating system, but generally, the user time relates to the execution 
# of the code, the system time relates to your CPU, and the elapsed time is 
# the difference in times since you started the stopwatch (and will be equal 
# to the sum of user and system times if the chunk of code was run 
# altogether). While the difference of .42 seconds may not seem like much,
# this gain in efficiency is huge!
