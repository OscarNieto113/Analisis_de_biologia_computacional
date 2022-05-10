#Samantha Guanipa A01703936
#Hilda Beltrán A01251916
#Óscar Nieto A01705090

main <- function(turn, ARN, comp){
  if (turn == 1){
    #Escribe una función que genere una secuencia aleatoria de DNA de tamaño “n”.
    secuencia <- function(cant){
      ADNv <- c('A', 'T', 'C', 'G', 'N', '-')
      ADNr <- c(sample(ADNv, size = cant, replace = TRUE))
      return(ADNr)
    }
    secuencia(cant <- (readline('Ingrese el tamaño de la secuencia: ')))
  }
  else if (turn == 2){
    #Codifica una función que calcula el tamaño de una secuencia de DNA.
    tam <- function(sec){
      tamano <- length(sec)
      return(tamano)
    }
    tam(secaleatoria)
  }
  else if (turn == 3){
  #Crea una función que recibe una secuencia de DNA e imprime el porcentaje de cada base en la secuencia.
    Porcentaje <- function (sec){
      a=(length(sec[sec=="A"])/ length(sec))*100
      t=(length(sec[sec=="T"])/ length(sec))*100
      g=(length(sec[sec=="G"])/ length(sec))*100
      c=(length(sec[sec=="C"])/ length(sec))*100
      guion=(length(sec[sec=="-"])/ length(sec))*100
      n=(length(sec[sec=="N"])/ length(sec))*100
      porc <-sprintf("A=%d T= %d G=%d C= %d -= %d N=%d",a,t,g,c,guion,n)
      return (porc)
    }
    Porcentaje(secaleatoria)
  }
  else if (turn == 4){
    #Programa una función que transcribe DNA a RNA
    conversionRNA <- function(sec){
      sec[sec=="T"]="U"
      RNA=sec
      #sec[sec=="T"]="T"
      return (RNA)
    }
    conversionRNA(secaleatoria)
  }
  else if (turn == 5){
    #Crea una función que traduce una secuencia de RNA a una secuencia de proteínas.
    conversionproteinas <- function (ARN){
      k <- 1
      m <- 1
      l <- as.numeric(length(ARN))
      v <- vector(length = l)
      l1 <- l / 3
      prot <- vector()
      while (k <= l){if (k %% 3 != 0){v[m] <- ARN[k]; m = m + 1}
        else if (k %% 3 == 0){
          v[m] <- ARN[k]
          m <- 1
          p <- paste0(v[1], v[2], v[3])
          if (p == ('UUU') | p == 'UUC'){prot <- append(prot, 'Phe')}
          else if (p == 'UUA' | p == 'UUG' | p == 'CUU' | p == 'CUC' | p == 'CUA' | p == 'CUG'){prot <- append(prot, 'Leu')}
          else if (p == 'UCU' | p == 'UCC' | p == 'UCA' | p == 'UCG' | p == 'AGU' | p == 'AGC'){prot <- append(prot, 'Ser')}
          else if (p == 'UAU' | p == 'UAC'){prot <- append(prot, 'Tyr')}
          else if (p == 'UGU' | p == 'UGC'){prot <- append(prot, 'Cys')}
          else if (p == 'UGG'){prot <- append(prot, 'Trp')}
          else if (p == 'CCU' | p == 'CCC' | p == 'CCA' | p == 'CCG'){prot <- append(prot, 'Pro')}
          else if (p == 'CAU' | p == 'CAC'){prot <- append(prot, 'His')}
          else if (p == 'CAA' | p == 'CAG'){prot <- append(prot, 'Gin')}
          else if (p == 'CGU' | p == 'CGC' | p == 'CGA' | p == 'CGG' | p == 'AGA' | p == 'AGG'){prot <- append(prot, 'Arg')}
          else if (p == 'AUU' | p == 'AUC' | p == 'AUA'){prot <- append(prot, 'Ile')}
          else if (p == 'AUG'){prot <- append(prot, 'Met')}
          else if (p == 'ACU' | p == 'ACC' | p == 'ACA' | p == 'ACG'){prot <- append(prot, 'Thr')}
          else if (p == 'AAU' | p == 'AAC'){prot <- append(prot, 'Asn')}
          else if (p == 'AAA' | p == 'AAG'){prot <- append(prot, 'Lys')}
          else if (p == 'GUU' | p == 'GUC' | p == 'GUA' | p == 'GUG'){prot <- append(prot, 'Val')}
          else if (p == 'GCU' | p == 'GCC' | p == 'GCA' | p == 'GCG'){prot <- append(prot, 'Ala')}
          else if (p == 'GAU' | p == 'GAC'){prot <- append(prot, 'Asp')}
          else if (p == 'GAA' | p == 'GAG'){prot <- append(prot, 'Glu')}
          else if (p == 'GGU' | p == 'GGC' | p == 'GGA' | p == 'GGG'){prot <- append(prot, 'Gly')}
          else{prot <- append(prot, 'ND')}
          v <- vector(length = l)
        }
        k = k + 1
      }
      return(prot)
    }
    conversionproteinas(ARN)
  }
  else if (turn == 6){
    #Escribe una función que recibe una hebra directa y regresa la hebra inversa.
    HebraInversa <- function (sec){
      Hebinversa <- rev(sec)
      return (Hebinversa)
    }
    HebraInversa(secaleatoria)
  }
  else if (turn == 7){
    #Escribe una función qué recibe una hebra directa y obtiene la hebra complementaria.
    HebraComplementaria <- function (sec){
      sec[sec=="T"]="a"
      sec[sec=="C"]="c"
      sec[sec=="G"]="C"
      sec[sec=="A"]="T"
      comp=sec
      sec[sec=="A"]="T"
      sec[sec=="G"]="C"
      sec[sec=="C"]="G"
      sec[sec=="T"]="A"
      return(comp)
    }
    HebraComplementaria(secaleatoria)
  }
  else if (turn == 8){ 
    #Escribe la función en R para obtener la hebra complementaria inversa, desde una hebra complementaria.
    HebCompInv <- function(complemen){
      hebcompinv <- rev(complemen)
      return(hebcompinv)
    }
    HebCompInv(comp)
  }
}

cont <- 1
while(cont <= 8){
  if (cont == 1){secaleatoria <- main(cont); print('La secuencia de ADN aleatoria es: '); print(secaleatoria); cont = cont + 1}
  else if (cont == 2){tamsec <- main(cont); print('El tamaño de la secuencia es: '); print(tamsec); cont = cont + 1}
  else if (cont == 3){porcentaje <- main(cont); print('El porcentaje de cada nucleotido es el siguiente: '); print(porcentaje); cont = cont + 1}
  else if (cont == 4){convARN <- main(cont); print('La secuencia de ADN convertida a ARN es: '); print(convARN); cont = cont + 1}
  else if (cont == 5){convprot <- main(cont, convARN); print('Las proteínas formadas, ND se muestra cuando tenemos N/-: '); print(convprot); cont = cont + 1}
  else if (cont == 6){inv <- main(cont); print('La hebra inversa de ADN: '); print(inv); cont = cont + 1}
  else if (cont == 7){comp <- main(cont); print('La hebra complementaria de ADN: '); print(comp); cont = cont + 1}
  else if (cont == 8){invcomp <- main(cont, convARN, comp); print('La hebra inversa de la complementaria de ADN: '); print(invcomp); cont = cont + 1}
}
