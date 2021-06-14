# Semana 1

# Q1
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

# Q2
# a
ch1 = "a"
ch2 = "b"
ch3 = "c"
ch4 = paste(ch1,".",ch2,".",ch3,sep="")
ch4
#ou
ch4 = paste(ch1,ch2,ch3,sep=".")
ch4

# b
ch5 = paste(ch1,ch2,ch3,sep="")
ch5

# c
ch4==ch5

# d
ch4!=ch5

# Q3
# a
18%%5
-5%%2
15%%5
8.3%%3

# b
l = 1
(l %%2) == 0 #serah par se o resultado for TRUE
(l %%2) != 0 #serah par se o resultado for FALSE

# c
l = 3
(l %%1) == 0 #serah inteiro se o resultado for TRUE
(l %%1) != 0 #serah inteiro se o resultado for FALSE

# d
l = -1
( (l %%1) == 0 ) && (l>=0) #serah natural se o resultado for TRUE
( (l %%1) != 0 ) | (l<0)  #serah natural se o resultado for FALSE

# Q4
a = seq(1:10)
b = seq(1,20,by=2)
c = seq(20,1,by=-2)

# a
x = b - c
x

# b
y = 2*a
y

# c
z = NULL
z[a==b] = TRUE 
z[a!=b] = FALSE 
z
#ou
z = (a==b)
z

# d
w = NULL
w[c>b] = TRUE 
w[c<=b] = FALSE 
w
#ou
w = (c>b)
w

# Q5
# a
letters
LETTERS

# b
class(letters)
class(LETTERS)

# c
length(letters)
length(LETTERS)

# d
a=c(LETTERS,letters)
class(a)
length(a)
a

# e
b=paste(LETTERS,letters)
class(b)
length(b)
b

# Q6
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

# Q7
A = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),4,3)
dim(A)
nrow(A)
ncol(A)

# Q8
# a
minha_lista=list("FULANO",20,c(1.7,60),c(TRUE,TRUE,FALSE))
minha_lista

# b
minha_lista[[1]]

# c
minha_lista[[2]]

# d
minha_lista[[3]][[1]]

# e
minha_lista[[3]][[2]]

# f
minha_lista[[4]][[3]]

# Q9
# a
lista_2 = list("Beltrano",35,c(1.7,60),c(TRUE,TRUE,FALSE))

# b
dados_alunos= list(minha_lista,lista_2)
dados_alunos

# c
dados_alunos[[1]][[1]]

# d
dados_alunos[[2]][[1]]

# e
dados_alunos[[1]][[3]][[1]]

# f
dados_alunos[[2]][[4]][[1]]

# Q10
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
