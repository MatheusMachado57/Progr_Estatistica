# Semana 2

# Q1 - Primeiro guarde nas variáveis a, b e c o tamanho dos lados de um triangulo qualquer.
# Em seguida implemente um código no R que imprime na tela uma mensagem informando se o triângulo em questão é equilátero, isósceles ou escaleno. 
# Teste o código implementado para diferentes valores de a, b e c.
a=10; b=2; c=3
a= 3; b=3; c=3
a= 4; b=4; c=5

if(a==b){
  if(a==c){print("Equilátero")} else{print("Isósceles")}
} else{
  if(a==c){print("Isósceles")} else{print("Escaleno")}
}

#OU
if( (a==b) && (a==c) ){print("Equilátero")} else{
  if( (a==b) || (a==c) || (b==c) ) {print("Isósceles")}else{print("Escaleno")}
}

# Q2 - Para cada item a seguir implemente um código no R para encontrar o que se pede.
# Não use os comandos seq ou algo parecido. 
# Dica: Comece com um vetor nulo e use o(s) controle(s) de fluxo que achar adequado para preenchê-lo.

# a - A sequência com os 100 primeiros múltiplos de 3.
a = NULL
for(j in 0:99){	a = c(a,3*j)	}
a
#OU
for(j in 0:99){	a[j+1] = 3*j	}
a
#OU
a = NULL
j = 0
while(length(a)<100){	a[j+1] = 3*j;   j=j+1	}
a

# b - A sequência com todos os múltiplos de 3 menores que 100.
b = NULL
j = 0
repeat{	
  i = 3*j
  if(i>=100){break}
  b = c(b,i)	
  j = j+1
}
b
#OU
b = NULL
for(i in 0:99){	
  if( (i%%3)==0 )	{b=c(b,i)}
}
b
#OU
b = NULL
i=0
while(i<100){	
  if( (i%%3)==0 )	{b=c(b,i)}
  i=i+1
}
b

# c - A sequência com os 100 primeiros números ímpares.
c = NULL
j = 0
for(i in 1:100){
  c = c(c,2*j+1)
  j = j+1
}
c
#OU
c = NULL
j = 0
while(length(c)<100){
  c = c(c,2*j+1)
  j = j+1
}
c

# Q3 - Usando os controles de fluxo vistos em sala de aula, faça o que se pede. 
# Dica: a partir do segundo item vai ser preciso usar dois loops, um dentro do outro.

# a - Primeiro crie uma matriz 10 × 10 nula. 
# Em seguida, usando um loop, preencha toda a sua primeira linha com o número 1.

M = matrix(0,10,10)
M
for(i in 1:10){M[1,i] = 1}
M
#OU
M = matrix(0,10,10)
i = 0
while(i<=10){i=i+1; M[1,i] = 1}
M

# b - Comece novamente com uma matriz 10 × 10 nula. Preencha cada uma de suas
# linhas com o número que indica a linha em questão. 
# Por exemplo, a primeira linha deve ser preenchido com 1, 
# a segunda com 2 e assim por diante, até a décima linha que deve ser preenchida com 10.

M = matrix(0,10,10)
for(i in 1:10){for(j in 1:10){M[j,i] = j}}
M

# c - Agora comece com uma matriz 100×100 nula e implemente um loop que preenche
# cada coluna com o número correspondente da coluna, isto é, a primeira coluna
# com o número 1 e assim por diante.
l = 100
M = matrix(0,l,l)
for(i in 1:l){for(j in 1:l){M[j,i] = i}}
M

# d - Crie uma matriz 100×100 tal que as posições em linhas pares recebem o número
# 2 e as posições em linhas ímpares o número 1.

l = 100
M = matrix(0,l,l)
for(i in 1:l){for(j in 1:l){
  u = j %% 2 
  if( u==0){M[j,i] = 2} else{M[j,i] = 1}
}}
M

# Q4 - Comece cada item a seguir com uma matriz 100 × 100 nula e não use o comando seq
# ou um vetor pronto. 

# a - Crie uma matriz diagonal 100 × 100 cujos elementos da diagonal principal são os
# números de 1 até 100 em ordem crescente.
n = 100
M = matrix(0,n,n)
for(i in 1:n){M[i,i] = i}
M

# b - Crie uma matriz diagonal 100 × 100 cujos elementos da diagonal principal são os
# números de 1 até 100 em ordem decrescente.
n = 100
M = matrix(0,n,n)
j = n
for(i in 1:n){M[i,i] = j; j= j-1}
M
#OU
for(i in 1:n){M[i,i] = n-i+1}
M

# Q5 - Usando os loops vistos em sala de aula crie as listas definidas em cada item a seguir.

# a - L1 ´e uma lista com 10 posi¸c˜oes tal que cada posi¸c˜ao i dessa lista guarda o n´umero i.
u = NULL
for(i in 1:10){u = c(u,i)}
L1 = as.list(u)
L1
length(L1)
#ou
L1 = list()
for(i in 1:10){L1[[i]] = i}
length(L1)

# b - L2 ´e uma lista com 10 posi¸c˜oes tal que cada posi¸c˜ao i dessa lista guarda um vetor de tamanho i com todas as posi¸c˜oes iguais a 1.
L2=list()
for(i in 1:10){
  v=NULL
  for(j in 1:i){v[j] = 1}
  L2[[i]] = v
}
L2

# c - L3 ´e uma lista com 10 posi¸c˜oes tal que cada posi¸c˜ao i dessa lista guarda um vetor com os 10 primeiros m´ultiplos de i.
L3 = list()  
for(i in 1:10){
  v = NULL
  for(j in 1:10){v[j] = i*(j-1)}
  L3[[i]] = v
}
L3

# d - L4 ´e uma lista com 10 posi¸c˜oes tal que cada posi¸c˜ao i dessa lista guarda um vetor com os i primeiros m´ultiplos de 2.
L4 = list()
for(i in 1:10){
  v = NULL
  for(j in 1:i){v[j] = 2*(j-1)}
  L4[[i]]=v
}
L4

# e
L5 = list()
for(i in 1:10){
  L5[[i]] = matrix(0,i,i)
  for(j in 1:i){L5[[i]][j,j] = 1}
}
L5

# Q6
# a
l = length(L1)
soma = 0
for(i in 1:l){soma = soma + sum(L1[[i]])} 
soma

# b
l = length(L3)
soma = 0
for(i in 1:l){soma = soma + sum(L3[[i]])} 
soma

# Q7
# a
soma = NULL
for(i in 1:10){
  soma[i] = sum(L4[[i]])
}
soma

# b
v = NULL
for(i in 1:10){
  u = soma[i] %% 5
  if(u ==0 ){v[i] = paste(soma[i],"é um múltiplo de 5")
  }else{
   v[i] = paste(soma[i],"não é um múltiplo de 5")}
}
v

# c
n = 0
for(i in 1:10){
  u = soma[i] %% 5
  if(u ==0 ){n = n+1}
}
n

# Q8
# a
x0 = 2
r  = 3
y  = x0
u  = x0
for(i in 1:100){
  u = u + r
  y = c(y, u)
}
y

# b
soma = 0
for(i in 1:35){soma = soma + y[i]}
soma

n1=35
(y[1]+y[n1])*n1/2

# c
soma = 0
for(i in 1:100){
  u = y[i]%%4
  if(u==0){soma=soma+1}
}
soma

# d
soma = 0
for(i in 1:100){
  u = y[i]%%4
  v = y[i]%%5
  if( (u==0) && (v==0) ){soma=soma+1}
}
soma

# e
soma = 0
for(i in 1:100){
  u = y[i]%%4
  v = y[i]%%5
  if( (u==0) || (v==0) ){soma=soma+1}
}
soma

# f
x = y
for(i in 1:100){
  u = x[i]%%2
  if(u!=0){x[i]=0}
}
x

# 2.9
# a
F = c(1,1)
for(i in 3:12){
  F[i] = F[i-1] + F[i-2]
}
F

# b
F = c(1,1)
i = 2
repeat{
  i    = i + 1
  u = F[i-1] + F[i-2]
  if(u>=300) break
  F[i] = u
}
F

# c
F1 = 1
F2 = 1
n  = 2
repeat{
  u = F1 + F2
  if(u>=1000000) break
  F1 = F2
  F2 = u
  n  = n+1
}
n
