#Samantha Guanipa A01703936
#Hilda Beltrán A01251916
#Óscar Nieto A01705090
#Cristian Espinosa A01702752

library(Biostrings); library(seqinr); library(adegenet); library(ape); 
library(ggtree); library(DECIPHER); library(viridis); library(ggplot2); 
library(spider); library(ggmsa)

#Estructura del DNABin:
listav <- c('JX869059', 'AY508724', 'MN908947', 'AY390556', 'AY278489',
            'MN985325', 'AY485277', 'MT292571')
lista <- ape::read.GenBank(listav)

listaigual <- lapply(lista, `length<-`, max(lengths(lista)))
m <- matrix(unlist(listaigual), nrow = 8, byrow = TRUE)
rownames(m) <- names(listaigual)
par(mar=c(4, 7, 5, 3))
ape::image.DNAbin(m, what = c('A', 'G', 'C', 'T', bg = 'white', 
                              show.labels = TRUE, legend = TRUE, grid = TRUE))
str(lista)

#Concentrar en un arhcivo todas las secuencias:
write.dna(lista, file = 'secuencias.fasta', format = 'fasta')

#Cargar las secuencias
secuencias <- readDNAStringSet('secuencias.fasta', format = 'fasta')
y <- readAAStringSet("secuencias.fasta", format="fasta", use.names = TRUE)

#Mostrar secuencia no alineada
secuencias

#Alineamiento de secuencias
secalineadas <- AlignSeqs(secuencias)

#Visualizar el resultado del alineamiento
secalineadas

#En el navegador
BrowseSeqs(secalineadas)

#Secuencia alineada en nuevo archivo
writeXStringSet(secalineadas, file = 'alineadas.fasta')

#Leer archivo de secuencia alineada
secuenciaal <- read.alignment('alineadas.fasta', format = 'fasta')

#Crear una matriz de distancia
matdist <- dist.alignment(secuenciaal, matrix = 'similarity')

#Visualizar matriz de distancia
matdist
pmat <- as.data.frame(as.matrix(matdist))
table.paint(pmat) + scale_color_viridis()

#Árbol filogenético
arbol <- bionj(matdist)

#Plot árbol
arbol <- ladderize(arbol)
plot(arbol)

#Plot con ggtree
plotarbol <- ggtree(arbol) + 
  geom_tiplab() + ggtitle('Árbol filogenético')
plotarbol

#Alineamiento de las secuencias
alineamiento <- ggmsa(secalineadas, start = 164, end = 213, color = 'LETTER')
alineamiento

#Árbol filogenético y alineamiento de secuencias
plotarbol1 <- ggtree(arbol) + geom_tiplab() 
data1 <- tidy_msa(y, 164, 213)
plotarbol1 + geom_facet(geom = geom_msa, data = data1, panel = 'msa', 
                       color = 'LETTER') + xlim_tree(1)
