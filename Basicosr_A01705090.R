#Equipo 9
#Cristian Rogelio Espinosa Díaz A01702752
#Oscar Eduardo Nieto Espitia A01705090

#Ejercicio 1
x <- c(10,11,13,-1,6,3)

#Ejercicio 2
est.x <- c(mean(x),sd(x), var(x))
print(est.x)

#Ejercicio 3
secuencia <- 20:50
prom <- mean(20:60)
suma <- sum(51:91)

#Ejercicio 4
sample(-100:50,10,replace=T)

#Ejercicio 5
fb <- numeric(10)
fb[1] <- fb[2]<- 1
cont <- 3
while (cont<=10){
  fb[cont]= fb[cont-1] + fb[cont-2]
  cont <- cont+1
}
print(fb)

#Ejercicio 6
a <- c(10,20,30,4,50,-60)
b <- c(10,20,30,4,50,-60)

#Ejercicio 7

#Ejercicio 8

#Ejercicio 9

#Función enésimo
enesimo <- function(v, n){
  for (i in seq(1, 100, by=n)){
    j<- v[i]
    print (j)
  }
}

v <- 1:100
enesimo(v, 9)
