# Semana 5
# Q1
Mult_vet_esc = function(v,alpha){
  if(class(v)!="numeric") stop("O primeiro argumento precisa ser um vetor numérico.")
  if( (class(alpha)!="numeric") || (length(alpha)>1) ) stop("O segundo argumento precisa ser um escalar.")
  n = length(v)
  w = NULL
  for(i in 1:n){    w[i] = alpha*v[i]  }
  return(w)
}
Mult_vet_esc( c(1,2,3,4), 2)
Mult_vet_esc( 2, c(1,2,3,4))

# Q2
Soma_2_vet = function(v,u){
  if( (class(v)!="numeric") || (class(u)!="numeric") )stop("Os argumentos precisam ser vetores numéricos.")
  n = length(v)
  m = length(u)
  if(n!=m)stop("Os vetores precisam ter o mesmo tamanho.")
  w = NULL
  for(i in 1:n){    w[i] = v[i] + u[i]  }
  return(w)
}
Soma_2_vet( c(1,2,3,4), 2)
Soma_2_vet( c(-1,-2,-3,-4), c(1,2,3,4))

# Q3
# a
# Entrada: 2 vetores numéricos, v e u
# Saída: o vetor w = v-u
# 1 Verifique se v e u são vetores numéricos. Caso contrário, pare o programa retornando uma mensagem de erro.
# 2 defina n como sendo o tamanho do vetor v
# 3 defina m como sendo o tamanho do vetor u
# 4 Se n diferente de m, retorne uma mensagem de erro e pare o programa
# 5 Inicialize o vetor w
# 6 Inicie i=1
# 7 Faça w_i = v_i + u_i
# 8 Incremente i: i=i+1
# 9 Se i<=n, volte para a linha 7
# 10 Retorne w 

# b 
# FORMA1
Sub_2_vet = function(v,u){
  if( (class(v)!="numeric") || (class(u)!="numeric") )stop("Os argumentos precisam ser vetores numéricos.")
  n = length(v)
  m = length(u)
  if(n!=m)stop("Os vetores precisam ter o mesmo tamanho.")
  w = NULL
  for(i in 1:n){    w[i] = v[i] - u[i]  }
  return(w)
}
Sub_2_vet( c(1,2,3,4), 2)
Sub_2_vet( c(-1,-2,-3,-4), c(1,2,3,4))

# OU FORMA2
Sub_2_vet = function(v,u){
  return( Soma_2_vet(v,-u) )
}
Sub_2_vet(c(10,20,30),c(2,2,2))

# OU FORMA3
Sub_2_vet = function(v,u){
  return( Soma_2_vet(v,Mult_vet_esc(u,-1)) )
}
Sub_2_vet(c(10,20,30),c(2,2,2))

# Q4
Prod_int = function(v,u){
  if( (class(v)!="numeric") || (class(u)!="numeric") )stop("Os argumentos precisam ser vetores numéricos.")
  n = length(v)
  m = length(u)
  if(n!=m)stop("Os vetores precisam ter o mesmo tamanho.")
  p = 0
  for(i in 1:n){    p = p + v[i]*u[i]  }
  return(p)
}
Prod_int( c(1,2,3,4), 2)
Prod_int( c(-1,-2,-3,-4), c(1,2,3,4))

# Q5
Ortog = function(v,u){
  p = Prod_int(v,u)
  if(p==0){return(TRUE)} else{return(FALSE)}
}
Ortog( c(-1,-2,-3,-4), c(1,2,3,4))
Ortog( c(1,2), c(-2,1))

# Q6
Mult_mat_esc = function(A,alpha){
  if(class(A)!="matrix")stop("O primeiro argumento deve ser uma matriz.")
  if( (class(alpha)!="numeric") || (length(alpha)>1) )stop("O segundo argumento deve ser um escalar.")
  n = nrow(A)
  m = ncol(A)
  M = matrix(NA,n,m)
  for(i in 1:n){
    for(j in 1:m){
      M[i,j] = alpha*A[i,j]
    }
  }
  return(M)
}
Mult_mat_esc( matrix(c(1,2,3,4),2,2), -3)
Mult_mat_esc( matrix(c(1,2,3,4),2,2), "A")

# Q7
Soma_matrizes = function(A,B){
  if((class(A)!="matrix") || (class(B)!="matrix") )stop("Entre com 2 matrizes.")
  n = nrow(A)
  m = ncol(A)
  l = nrow(B)
  c = ncol(B)
  if((l!=n)||(c!=m))stop("As 2 matrizes precisam ter o mesmo número de linhas e de colunas.")
  M = matrix(NA,n,m)
  for(i in 1:n){
    for(j in 1:m){
      M[i,j] = A[i,j] + B[i,j]
    }
  }
  return(M)
}
Soma_matrizes( matrix(c(1,2,3,4),2,2), matrix(c(-1,-2,-3,-4),2,2))

# Q8
# a 
# Entrada: A e B matrizes com valores numéricos
# Saída: matriz M = A - B
# 1 Defina n = número de linhas de A
# 2 Defina l = número de linhas de B
# 3 Se n for diferente de l, retorne uma mensagem de erro e encerre o algoritmo.
# 4 Defina m = número de colunas de A
# 5 Defina c = número de colunas de B
# 6 Se m for diferente de c, retorne uma mensagem de erro e encerre o algoritmo.
# 7 Crie uma matriz M de dimensão n x m
# 8 Inicie i = 1
# 9 Inicie j = 1
# 10 Faça M_i,j = A_i,j - B_i,j sendo i a i-ésima linha e j a j-ésima coluna
# 11 Incremente j: j = j+1
# 12 Se j <= m , volte para a linha 10
# 13 Incremente i: i = i+1
# 14 Se i <= n , volte para a linha 9
# 15 Retorne M

# b
Subt_matrizes = function(A,B){
  if((class(A)!="matrix") || (class(B)!="matrix") )stop("Entre com 2 matrizes.")
  n = nrow(A)
  m = ncol(A)
  l = nrow(B)
  c = ncol(B)
  if((l!=n)||(c!=m))stop("As 2 matrizes precisam ter o mesmo número de linhas e de colunas.")
  M = matrix(NA,n,m)
  for(i in 1:n){
    for(j in 1:m){
      M[i,j] = A[i,j] - B[i,j]
    }
  }
  return(M)
}
Subt_matrizes( matrix(c(1,2,3,4),2,2), matrix(c(1,2,3,4),2,2))

# Q9
Transposta = function(A){
  if(class(A)!="matrix") stop("O argumento deve ser uma matriz.")
  n = nrow(A)
  m = ncol(A)
  M = matrix(NA,m,n)
  for(i in 1:m){
    for(j in 1:n){
      M[i,j] = A[j,i]
    }
  }
  return(M)
}
Transposta( matrix(c(1,2,3,4),2,2))

# Q10
# a  
# Entrada: uma matriz A matriz com valores numéricos
# Saída: uma variável lógica sendo TRUE quando a matriz for simétrica e FALSE, caso contrário
# 1 Defina n = número de linhas de A
# 2 Defina m = número de colunas de A
# 3 Se n for diferente de m, retorne FALSE. Caso contrário, siga os passos abaixo.
# 4 Inicie i = 1
# 5 Inicie j = i+1
# 6 Se A_i,j for diferente de A_j,i, sendo i a i-ésima linha e j a j-ésima coluna, retorne FALSE e encerre o programa.
# 7 Incremente j: j = j+1
# 8 Se j <= n , volte para a linha 6
# 9 Incremente i: i = i+1
# 10 Se i <= n-1 , volte para a linha 5
# 11 Retorne TRUE 

# b
Simetrica = function(A){
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
Simetrica( matrix(c(1,2,3,4),2,2))
Simetrica( matrix(c(0,2,3,2,0,5,3,5,0),3,3,byrow=TRUE) )
Simetrica( matrix(c(0,2,3,2,0,5,3,15,0),3,3,byrow=TRUE) )

# Q11
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
Produto_Av( matrix(c(1,2,3,4),2,2),c(1,1))

# Q12
Mult_matrizes = function(A,B){
  if((class(A)!="matrix") || (class(B)!="matrix") )stop("Entre com 2 matrizes.")
  n = nrow(A)
  m = ncol(A)
  l = nrow(B)
  k = ncol(B)
  if(l!=m)stop("O número de colunas da primeira matriz precisa ser igual ao número de linhas da segunda matriz.")
  M = matrix(NA,n,k)
  for(i in 1:n){
    for(j in 1:k){
      ai = A[i,]
      bj = B[,j]
      M[i,j] = Prod_int(ai,bj)
    }
  }
  return(M)
}
Mult_matrizes( matrix(c(1,2,3,4),2,2), matrix(c(-1,-2,-3,-4),2,2))
matrix(c(1,2,3,4),2,2) %*% matrix(c(-1,-2,-3,-4),2,2)

# Q13
alpha = 4
beta = -3
v1 = c(2,-3,-1,5,0,-2)
v2 = c(3,4,-1,0,1,1)
v3 = c(1,2,3,4,5)
v4 = c(0,1,1)
M1 = matrix(c(1,3,2,-1,0,1),2,3,byrow=TRUE)
M2 = matrix(c(0,-5,3,-1,1,-1,1,4,0),3,3,byrow=TRUE)
M3 = matrix(c(3,1,-2,10,3,-1),3,2,byrow=TRUE)
M4 = matrix(c(1,1,0,1),2,2,byrow=TRUE)
M5 = matrix(c(3,1,0,1,1,1,3,2,0,3,-5,0,1,2,0,0),4,4,byrow=TRUE)

Mult_vet_esc(v3,alpha)
v3*alpha

Soma_2_vet(v1,v2)
v1+v2

Sub_2_vet(v3,v1)
v3-v1

Prod_int(v1,v2)
v1%*%v2

Prod_int(alpha*v1,v2-v1)
(alpha*v1)%*%(v2-v1)

Prod_int(v1+v2,v1-v2)
(v1+v2)%*%(v1-v2)

Ortog(v1,v2)
v1%*% v2

Mult_mat_esc(M1, beta)
beta* M1 

Transposta(M1)
t(M1)

Simetrica(M1)
Simetrica(M4)
Simetrica(M5)

Produto_Av(M1,v4)
M1%*%v4

Soma_2_vet(Produto_Av(M2,v4), v4)
M2%*%v4 + v4

Mult_matrizes(M1,M2)
M1%*%M2

Mult_matrizes(M2,M1)
M2%*%M1

Mult_matrizes(Transposta(M3),M2)
t(M3) %*% M2

Soma_matrizes(Mult_matrizes(M1,M3), M4)
M1%*%M3 + M4

Mult_matrizes(Mult_matrizes(M1,M2),M3)
M1%*%M2%*%M3

Subt_matrizes(Mult_matrizes(Mult_matrizes(M1,M2),M3), M4)
M1%*%M2%*%M3 - M4
