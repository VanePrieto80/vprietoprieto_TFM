# INSTAL·LACIÓ DE PAQUETS I CÀRREGA DE LLIBRERIES
# S'instal·len i es carreguen els paquets de Bioconductor necessaris per executar el codi sencer
suppressMessages(install.packages("BiocManager"))

suppressMessages(BiocManager::install("limma", force = TRUE)) 
suppressMessages(library(limma)) 

suppressMessages(BiocManager::install("GEOquery", force = TRUE)) 
suppressMessages(library(GEOquery)) 

suppressMessages(BiocManager::install("genefilter", force = TRUE)) 
suppressMessages(library(genefilter))

# S'instal·len i es carreguen els paquets de Cran necessaris per executar el codi sencer
suppressMessages(install.packages("mRMRe"))
suppressMessages(library(mRMRe))

suppressMessages(install.packages("caret"))
suppressMessages(library(caret))

suppressMessages(install.packages("pROC"))
suppressMessages(library(pROC))

suppressMessages(install.packages("MLmetrics"))
suppressMessages(library(MLmetrics))

suppressMessages(install.packages("Rtsne"))
suppressMessages(library(Rtsne))

suppressMessages(install.packages("randomForest"))
suppressMessages(library(randomForest))

suppressMessages(install.packages("dplyr"))
suppressMessages(library(dplyr))

suppressMessages(install.packages("plyr"))
suppressMessages(library(plyr))

suppressMessages(install.packages("e1071"))
suppressMessages(library(e1071))

suppressMessages(install.packages("class"))
suppressMessages(library(class))

suppressMessages(install.packages("randomForest"))
suppressMessages(library(randomForest))

suppressMessages(install.packages("xgboost"))
suppressMessages(library(xgboost))

suppressMessages(install.packages("smotefamily"))
suppressMessages(library(smotefamily))

suppressMessages(install.packages("keras"))
suppressMessages(library(keras))

suppressMessages(install.packages("ingredients"))
suppressMessages(library(ingredients))

suppressMessages(install.packages("DALEX"))
suppressMessages(library(DALEX))

suppressMessages(install.packages("rpart"))
suppressMessages(library(rpart))