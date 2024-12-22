# INSTAL·LACIÓ DE PAQUETS I CÀRREGA DE LLIBRERIES
# S'instal·len i es carreguen els paquets de Bioconductor necessaris per executar el codi sencer
suppressMessages(install.packages("BiocManager"))

suppressMessages(BiocManager::install("affy", force = TRUE)) 
suppressMessages(library(affy)) 

suppressMessages(BiocManager::install("Biobase", force = TRUE)) 
suppressMessages(library(Biobase)) 

suppressMessages(BiocManager::install("affyPLM", force = TRUE)) 
suppressMessages(library(affyPLM))


# REPRODUCCIÓ DE LA IMATGE DEL MICROARRAY
# Es llegeix les dades de l'arxiu CEL
microarray.raw.data <- ReadAffy(path, verbose=TRUE)

# Visualitzar la imatge del microarray
image(microarray.raw.data[,1], col=rainbow(255))
