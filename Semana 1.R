# Semana 1

# Q1 - 1.1 Defina x, y e z como sendo objetos do tipo "numeric" que guardam os valores 0, -1 e
# 3/2, respectivamente. Faça no prompt do R as contas a seguir e verifique se o resultado
# está como o esperado.

x = 0
y = -1
z = 3/2

# a
a = x+y+z
a
b = y*z
b
c = z/y
c

# b
z^2
z^3
z^x
z^y

# c
sqrt(a)
sqrt(x)
sqrt(y)

# d
b^(1/3)
(-1/c)^(1/4)
z^(2/3)

# e
abs(x)
abs(y)
abs(z)

# f
exp(x)
exp(y)
exp(c)

# g
log(x)
log(a)
log(b)

# h
sqrt(pi)
exp(-x)

# Q2 - Defina ch1, ch2 e ch3 como sendo objetos do tipo "character" que guardam os textos “a”, “b” e “c”, respectivamente.
# a  - Usando a fun¸c˜ao paste a partir de ch1, ch2 e ch3 crie um quarto objeto da classe "character", ch4, definido por “a.b.c”.
ch1 = "a"
ch2 = "b"
ch3 = "c"
ch4 = paste(ch1,".",ch2,".",ch3,sep="")
ch4
#ou
ch4 = paste(ch1,ch2,ch3,sep=".")
ch4

# b - Usando a função paste a partir de ch1, ch2 e ch3 crie um quinto objeto da classe "character", ch5, definido por “abc”.
ch5 = paste(ch1,ch2,ch3,sep="")
ch5

# c - Usando o comando == verifique se ch4 e ch5 s˜ao iguais ou diferentes.
ch4==ch5

# d - Usando o comando != verifique se ch4 e ch5 s˜ao iguais ou diferentes.
ch4!=ch5

# Q3 - O operador %% fornece o resto da divisão entre dois números, por exemplo, 15%%4 fornece o resto da divisão de 15 por 4, que é 3.
# Esse comando será bastante usado durante o curso. Faça os itens a seguir primeiros no papel e depois verifique a resposta usando o R.

# a - Qual a resposta para 18%%5, -5%%2, 15%%5 e 8.3%%3?
18%%5
-5%%2
15%%5
8.3%%3

# b - Como podemos usar o operador %% para testar se um número é par? Fa¸ca o teste no prompt do R e use também os operadores == ou != de forma que a resposta
# seja TRUE se o n´umero for par e FALSE caso contrários.

l = 1
(l %%2) == 0 #serah par se o resultado for TRUE
(l %%2) != 0 #serah par se o resultado for FALSE

# c - Como podemos usar o operador %% para testar se um número é inteiro? Faça o teste no prompt do R e use também os operadores == ou != de forma que a
# resposta seja TRUE se o número for inteiro e FALSE caso contrários.
l = 3
(l %%1) == 0 #serah inteiro se o resultado for TRUE
(l %%1) != 0 #serah inteiro se o resultado for FALSE

# d - Como podemos usar o operador %% para testar se um número é natural, isto é, inteiro e positivo? Faça o teste no prompt do R e use também os operadores ==, 
# !=, && ou || de forma que a resposta seja TRUE se o número for natural e FALSE caso contrários.
l = -1
( (l %%1) == 0 ) && (l>=0) #serah natural se o resultado for TRUE
( (l %%1) != 0 ) | (l<0)   #serah natural se o resultado for FALSE

# Q4 - Digite no prompt do R:
# > a<-seq(1:10); b<-seq(1,20,by=2); c<-seq(20,1,by=-2)
# Usando os operadores +,-,*,/ e tamb´em ==,!=,<,> fa¸ca o que se pede nos itens a seguir.

a = seq(1:10)
b = seq(1,20,by=2)
c = seq(20,1,by=-2)

# a - Crie um array x onde cada posição de x é dada pela subtração entre as respectivas posiçõoes de b e c.
x = b - c
x

# b - Crie um array y onde cada posição de y é o dobro de cada posição de a.
y = 2*a
y

# c - (c) Crie um array z onde cada posição de z é um objeto da classe "logic". 
# A posição i de z vai guardar TRUE se a[i] for igual a b[i] e FALSE caso contrário.
z = NULL
z[a==b] = TRUE 
z[a!=b] = FALSE 
z
#ou
z = (a==b)
z

# d - (d) Crie um array w onde cada posição de w é um objeto da classe "logic". 
# A posição i de w vai guardar TRUE se c[i] for maior que b[i] e FALSE caso contrário.
w = NULL
w[c>b] = TRUE 
w[c<=b] = FALSE 
w
#ou
w = (c>b)
w

# Q5 - No R já exitem alguns objetos pré-definidos que são chamados de constantes. 
# Como exemplo temos a constante pi, já usada no exercício (1), e os arrays letters e LETTERS: 
# sequências com as letras minúsculas e maiúsculas do alfabeto.

# a - Primeiro digite letters e LETTERS para como são exatamente esses objetos.
letters
LETTERS

# b - Qual a classe dos objetos letters e LETTERS? 
# Primeiro tente responder sem usar o comando class e depois verifique a sua resposta usando tal comando.
class(letters)
class(LETTERS)

# c - Sem contar, como podemos encontrar o tamanho dos arrays letters e LETTERS?
# Qual o tamanho deles?
length(letters)
length(LETTERS)

# d - Se digitarmos a<-c(LETTERS,letters) qual a classe do objeto a, qual o seu
# tamanho e como é este objeto. Tente responder sem digitar e depois use o prompt
# para verificar a sua resposta.
a=c(LETTERS,letters)
class(a)
length(a)
a

# e - Se digitarmos b<-paste(LETTERS,letters) qual a classe do objeto b, qual o
# seu tamanho e como é este objeto. Tente responder sem digitar e depois use o
# prompt para verificar a sua resposta.
b=paste(LETTERS,letters)
class(b)
length(b)
b

# Q6 - Crie as seguintes matrizes no R:
# Faça os itens (c) e (d) sem usar um array cheio de 0’s ou 1’s.

#a
matrix(c(1,2,3,4,101,102,103,104),4,2)

# b
matrix(c(1,2,3,4,101,102,103,104),2,4,byrow=TRUE)

# c
matrix(0,3,2)
matrix(rep(0,6),3,2) #usando um array cheio de zeros

# d
matrix(1,3,3)
matrix(rep(1,9),3,3) #usando um array cheio de uns

# Q7 - Digite no prompt do R o seguinte comando:
# > A<-matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),4,3)
# Sem contar, usando os comandos da classe "matrix" que fornece as dimensões de uma matriz, 
# encontre o número de linhas e o número de colunas de A.

A = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),4,3)
dim(A)
nrow(A)
ncol(A)

# Q8
# a - Crie uma lista chamada minha_lista com 4 elementos. O primeiro elemento é o seu nome, 
# o segundo sua idade, o terceiro um array que guarda suas medidas (altura e peso, nessa ordem) e 
# o quarto elemento é outro array que guarda TRUE para as respostas afirmativas e 
# FALSE para as respostas negativas das seguintes perguntas: 

# (i) Você já estagiou? ; 
# (ii) Você já participou de algum projeto como voluntário? ; 
# (iii) Você tem interesse em assuntos relacionados ao meio ambiente?.

minha_lista=list("FULANO",20,c(1.7,60),c(TRUE,TRUE,FALSE))
minha_lista

# b - A partir do objeto minha_lista criado acesse o seu nome.
minha_lista[[1]]

# c - A partir do objeto minha_lista criado acesse a sua idade.
minha_lista[[2]]

# d - A partir do objeto minha_lista criado acesse a sua altura.
minha_lista[[3]][[1]]

# e - A partir do objeto minha_lista criado acesse o seu peso.
minha_lista[[3]][[2]]

# f - A partir do objeto minha_lista criado acesse a resposta para a pergunta “Você tem interesse em assuntos relacionados ao meio ambiente?”.
minha_lista[[4]][[3]]

# Q9 
# a - Refaça o item (a) do exercícios anterior agora com os dados de um amigo ou dados fictícios. 
# Chame essa nova lista de lista_2.
lista_2 = list("Beltrano",35,c(1.7,60),c(TRUE,TRUE,FALSE))

# b - Crie agora outra lista com 2 objetos, vamos chamá-la de dados_alunos. 
# O primeiro objeto dessa lista é a lista criada no exercício anterior, minha_lista, e
# o segundo objeto é a lista criada no primeiro item desse exercício, lista_2.
dados_alunos = list(minha_lista,lista_2)
dados_alunos

# c - A partir do objeto dados_alunos criado acesse o seu nome.
dados_alunos[[1]][[1]]

# d - A partir do objeto dados_alunos criado acesse o nome do seu amigo.
dados_alunos[[2]][[1]]

# e - A partir do objeto dados_alunos criado acesse a sua altura.
dados_alunos[[1]][[3]][[1]]

# f - A partir do objeto dados_alunos criado acesse a resposta do seu amigo para a pergunta “Você já estagiou?”.
dados_alunos[[2]][[4]][[1]]

# Q10 - Qual a diferen¸ca entre os objeto obj1, obj2 e obj3 definidos a seguir?
obj1 = list(1,2,3)
obj2 = list(c(1,2,3))
obj3 = c(1,2,3)
obj1
obj2
obj3
length(obj1)
length(obj2)
length(obj3)

# obj1 e uma lista com 3 elementos e cada elemento desta lista eh um numero
# obj2 e uma lista com um elemento e este elemento eh um vetor com 3 elementos
# obj3 e um vetor com 3 elementos
