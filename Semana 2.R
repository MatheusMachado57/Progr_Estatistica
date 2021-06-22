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

# a - L1 é uma lista com 10 posições tal que cada posição i dessa lista guarda o número i.
u = NULL
for(i in 1:10){u = c(u,i)}
L1 = as.list(u)
L1
length(L1)
#ou
L1 = list()
for(i in 1:10){L1[[i]] = i}
length(L1)

# b - L2 é uma lista com 10 posições tal que cada posição i dessa lista guarda um vetor de tamanho i com todas as posições iguais a 1.
L2=list()
for(i in 1:10){
  v=NULL
  for(j in 1:i){v[j] = 1}
  L2[[i]] = v
}
L2

# c - L3 é uma lista com 10 posições tal que cada posição i dessa lista guarda um vetor com os 10 primeiros múltiplos de i.
L3 = list()  
for(i in 1:10){
  v = NULL
  for(j in 1:10){v[j] = i*(j-1)}
  L3[[i]] = v
}
L3

# d - L4 é uma lista com 10 posiçõoes tal que cada posição i dessa lista guarda um vetor com os i primeiros múltiplos de 2.
L4 = list()
for(i in 1:10){
  v = NULL
  for(j in 1:i){v[j] = 2*(j-1)}
  L4[[i]]=v
}
L4

# e - L5 é uma lista com 10 posições tal que cada posição i dessa lista guarda a matriz identidade de tamanho i×i.
L5 = list()
for(i in 1:10){
  L5[[i]] = matrix(0,i,i)
  for(j in 1:i){L5[[i]][j,j] = 1}
}
L5

# Q6 - Usando as listas L1 e L3 do exercício 2.5, faça o que se pede.
# a - Encontre o valor da soma de todos os números guardados em L1.
l = length(L1)
soma = 0
for(i in 1:l){soma = soma + sum(L1[[i]])} 
soma

# b - Encontre o vetor definido pela soma de todos os vetores guardados em L3.
l = length(L3)
soma = 0
for(i in 1:l){soma = soma + sum(L3[[i]])} 
soma

# Q7 - Usando a lista L4 do exercício 2.5, faça o que se pede.
# a - Crie um vetor soma tal que a sua posição i guarda a soma dos elementos do vetor alocado na posição i da lista L4.
soma = NULL
for(i in 1:10){
  soma[i] = sum(L4[[i]])
}
soma

# b - Crie um vetor v de character tal que a sua posição i guarda o objeto soma[i]
# concatenado com "é um múltiplo de 5" se a o valor da posição i do vetor soma
# for um múltiplo de 5. Caso contrário guarde na posição i de v o objeto soma[i]
# concatenado com "não ´e um múltiplo de 5". Para concatenar textos use o comando paste.
v = NULL
for(i in 1:10){
  u = soma[i] %% 5
  if(u ==0 ){v[i] = paste(soma[i],"é um múltiplo de 5")
  }else{
   v[i] = paste(soma[i],"não é um múltiplo de 5")}
}
v

# c - A partir do vetor soma ou do vetor v criados nos itens anteriores conte o número
# de vetores da lista L4 tais que a sua soma é um número múltiplos de 5. Não é
# para você visualizar soma ou v e contar, e sim para usar um loop e um contador
# para realizar essa conta.

n = 0
for(i in 1:10){
  u = soma[i] %% 5
  if(u ==0 ){n = n+1}
}
n

# Q8 - Uma progressão aritmética (p.a.) é uma sequência numérica em que cada termo, a
# partir do segundo, é igual à soma do termo anterior com uma constante r. O número
# r é chamado de razão. O primeiro termo da sequência será chamado de x0.

# a - Faça um código em R que determine os 100 primeiros termos da progressão
# aritmética cuja termo inicial é x0 = 2 e a razão é r = 3. Vamos chamar o vetor
# com os elementos dessa sequência de y.

x0 = 2
r  = 3
y  = x0
u  = x0
for(i in 1:100){
  u = u + r
  y = c(y, u)
}
y

# Depois que y estiver construído:

# b - Faça um código que determine a soma dos 35 primeiros termos dessa sequência.
# Compare o valor encontrado com o valor fornecido pela fórmula da soma de uma
# p.a.. Você lembra dessa fórmula?

soma = 0
for(i in 1:35){soma = soma + y[i]}
soma

n1=35
(y[1]+y[n1])*n1/2

# c - Faça um código que conte um número de elementos em y múltiplos de 4 
# (lembre do comando %% visto na semana passada, que fornece o resto da divisão).
soma = 0
for(i in 1:100){
  u = y[i]%%4
  if(u==0){soma=soma+1}
}
soma

# d - Faça um código que conte um número de elementos em y múltiplos de 4 e múltiplos de 5 simultaneamente.
soma = 0
for(i in 1:100){
  u = y[i]%%4
  v = y[i]%%5
  if( (u==0) && (v==0) ){soma=soma+1}
}
soma

# e - Faça um código que conte um número de elementos em y múltiplos de 4 ou múltiplos de 5.
soma = 0
for(i in 1:100){
  u = y[i]%%4
  v = y[i]%%5
  if( (u==0) || (v==0) ){soma=soma+1}
}
soma

# f - Vamos agora criar uma nova sequência x a partir da sequência y da seguinte maneira: 
# cada termo par da sequencia y será mantido e os termos ímpares serão substituídos por 0. 
# Faça um código que gere a sequência x assim definida.
x = y
for(i in 1:100){
  u = x[i]%%2
  if(u!=0){x[i]=0}
}
x

# 2.9 - A famosa sequência de Fibonacci é definida da seguinte maneira: 
# os dois primeiros elementos são iguais a [1, 1] e a partir do terceiro elemento cada termo da sequência
# é definido como a soma dos dois termos anteriores. 
# Por exemplo, o terceiro termo é 2 (= 1 + 1), o quarto termo é 3 (= 1 + 2), o quinto termo é 5 (= 2 + 3) e assim por diante.

# a - Faça um c´odigo em R para encontrar os 12 primeiros números da sequência de Fibonacci.
F = c(1,1)
for(i in 3:12){
  F[i] = F[i-1] + F[i-2]
}
F

# b - Faça um código em R para encontrar todos os números da sequência de Fibonacci menores que 300.
F = c(1,1)
i = 2
repeat{
  i    = i + 1
  u = F[i-1] + F[i-2]
  if(u>=300) break
  F[i] = u
}
F

# c - Fa¸ca um código em R que determine o número de termos da sequência de Fibonacci menores que 1.000.000. 
# Veja que nesse caso você não precisa (e nem deve!) guardar os termos da sequência, apenas precisa contar o número de elementos.
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
