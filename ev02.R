#Samantha Guanipa A01703936
#Hilda Beltrán A01251916
#Óscar Nieto A01705090
#Cristian Espinosa A01702752

library(Biostrings); library(seqinr); library(adegenet); library(ape);
library(ggtree); library(DECIPHER); library(viridis); library(ggplot2);
library(spider); library(ggmsa)

#Cargar secuencias
codigos <- c('MW633892', 'MW592707', 'MT470219', 'MT534285', 'HG993784',
             'MW822592', 'MW927136', 'MZ026853', 'MW737421', 'MW852494',
             'MW884219', 'MW577029', 'MW938089', 'HG994158', 'MW741552',
             'MW981442', 'MW976780', 'MW308549', 'OD906774', 'MZ021680')
paises <- c('Argentina', 'Brazil', 'Colombia', 'Czech', 'France', 'Germany',
            'India', 'Indonesia', 'Iran', 'Italy', 'Mexico', 'Netherlands',
            'Peru', 'Poland', 'Russia', '   South Africa', 'Spain', 'Turkey',
            'UK', 'USA')
secuenciasobt <- ape::read.GenBank(codigos)

#Obtener longitud de cada secuencia
i = 1; bases <- matrix(data = NA, nrow = 4, ncol = 20); longitudes <- vector()
contenidoGC <- vector()
while (i <= length(codigos)){
  longitudes <- append(longitudes, length(secuenciasobt[[i]]));
  contenidoGC <- append(contenidoGC, ape::GC.content(secuenciasobt[i]))
  bases[ , i] <- ape::base.freq(secuenciasobt[i], freq = TRUE); i = i + 1;
}

#Crear gráfica comparativa de bases de ADN
par(mar=c(6.5, 4, 7, 0)); par(xpd = TRUE);
col <- c('red', 'green', 'blue', 'yellow')
names(bases) <- paises
barplot(bases, main = 'Composicion', col = col, names.arg = paises, las = 2)
legend(0, 53980, legend = c('Adenina', 'Timina', 'Guanina', 'Citosina'), 
       fill = col)

#Crear árbol filogenético
write.dna(secuenciasobt, file = 'secuencias.fasta', format = 'fasta')
secarc <- readDNAStringSet('secuencias.fasta', format = 'fasta')
secuencias <- AlignSeqs(secarc)
writeXStringSet(secuencias, file = 'alineadas.fasta')
secuenciaal <- read.alignment('alineadas.fasta', format = 'fasta')
matdist <- dist.alignment(secuenciaal, matrix = 'similarity')
mat <- as.data.frame(matdist)
write.csv(mat, file = 'Matriz distancia.csv')
arbol <- nj(matdist)
ggtree(arbol) + geom_tiplab() + ggtitle('Árbol filogenético') + xlim(NA, 0.037)

#Imprimir la longitud de secuencia y contenido GC por virus
l <- sort(longitudes, decreasing = TRUE, index.return = TRUE)
p <- vector(); s <- vector(); gc <- vector()
h <- l[[2]]
k <- 1
while (k <= length(codigos)){p <- append(p, paises[h[k]]);
s <- append(s, codigos[h[k]]); gc <- append(gc, contenidoGC[h[k]]); k = k + 1}

df_long <- data.frame(País = p, Secuencia = s, Longitud = l[[1]], 
                      ContenidoGC = gc)
df_long
