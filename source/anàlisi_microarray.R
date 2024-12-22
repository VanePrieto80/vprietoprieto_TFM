# CÀRREGA DE DADES GSE199633
# Es carreguen les dades des de GEO
gse <- getGEO("GSE199633", GSEMatrix = TRUE)

# Es comprova quantes plataformes s'utilitzen
length(gse)

# Es carrega el primer objecte que és està el conjunt de dades a analitzar
gse <- gse[[1]]
gse


# ELIMINACIÓ DE REGISTRES
# S'eliminen els registres amb NA a les principals variables clíniques
# S'eliminen els registres amb RIN<7
indices_remove <- c(023,037,038,039,041,044,048,050,051,054,056,061,063,068,071,072,073,074,075,076,077,079,083,087,093,094,096,097,098,
                    103,109,110,111,113,118,119,120,126,127,128,136,137,149,156,164,169,170,171,177,183,187,189,196,201,204,211,217,221,
                    222,223,229,232,234,237,238,242,246,251,258,262,269,285,292,304,307,318,320,325,338,352,353,355,357,358,361,364,366,
                    372,373,376,398,400,403,410,413,416,420,421,423,424,425,428,432,436,440,442,444,447,448,449,450,451,452,453,454,455,
                    457,463,465,468,470,471,472,474,475,478,482,483,484,485,493,495,497,498,500,503,508,517,518,521,527,529,530,537,541,
                    543,544,559,564,566,567,568,572,574,576,581,584,586,590,592,593,601,606,611,612,614,627,628,629,631,633)

gse <- gse[,-indices_remove]

# Es creen variables amb l'arxiu descarregat
sampleInfo <- pData(gse) ## Informació de la mostra
genAnnotation <- fData(gse) ## Anotació del gen
exprsData <- exprs(gse) ## Dades de l'expressió

# S'INSPECCIONEN LES DADES DE L'EXPRESSIÓ GENÉTICA
# S'inspecciona les dades d'expressió genética
# Si els valors es troben entre 0 i 16, es troba a escala logarítmica
summary(exprsData)

# Es normalitza utilitzant el mètode de normalització quantílica
normalized_data <- normalizeBetweenArrays(exprsData, method = "quantile")

# Es visualitza les dades abans i després de la normalització
boxplot(exprsData, main = "Abans de la normalització")
boxplot(normalized_data, main = "Després de la normalització")

# Es visualitza els 25 primers registres abans i després de la normalització
boxplot(exprsData[,1:25],col = rainbow(25), main = "Abans de la normalització")
boxplot(normalized_data[,1:25],col = rainbow(25), main = "Després de la normalització")

#S'INSPECCIONEN LES VARIABLES CLÍNIQUES
# S'inspeccionen les variables clíniques
head(sampleInfo)

# Es seleccionen les variables clíniques a estudiar i es renomenen
sampleInfo <- dplyr::select(sampleInfo, 'age at diagnosis:ch1', 'age at diagnosis (dichotomized):ch1', 'race:ch1', 'histology:ch1',
                            'er status:ch1', 'pr status:ch1', 'her2 status:ch1','pt:ch1', 'pn:ch1', 'ptnm:ch1', 'type of surgery:ch1', 
                            'radiotherapy:ch1', 'type of radiotherapy:ch1', 'chemotherapy:ch1', 'locoregional recurrence:ch1', 'death:ch1')
sampleInfo <- dplyr::rename(sampleInfo, age = 'age at diagnosis:ch1', pensioner = 'age at diagnosis (dichotomized):ch1', race='race:ch1',
                            histology='histology:ch1', er='er status:ch1', pr='pr status:ch1', her2='her2 status:ch1', pt='pt:ch1', pn='pn:ch1',
                            ptnm='ptnm:ch1', surgery='type of surgery:ch1', radiotherapy='radiotherapy:ch1',
                            typeRadiotherapy='type of radiotherapy:ch1', chemotherapy='chemotherapy:ch1', 
                            recurrence='locoregional recurrence:ch1', survivor='death:ch1')
head(sampleInfo)

# Es modifiquen les variables
# Variables que descriuen les pacients
sampleInfo$age <- as.integer(sampleInfo$age)
sampleInfo$pensioner <- factor(sampleInfo$pensioner, labels=c("No", "Yes"))
sampleInfo$race <- factor(sampleInfo$race, labels=c("NA", "AA", "Other", "White"))
sampleInfo$race <- factor(sampleInfo$race, levels=c("White", "AA", "Other", "NA"))

# Variables que descriuen la malaltia
sampleInfo$histology <- as.factor(sampleInfo$histology)
sampleInfo$er <- as.factor(sampleInfo$er)
sampleInfo$pr <- as.factor(sampleInfo$pr)
sampleInfo$her2 <- as.factor(sampleInfo$her2)
sampleInfo$pt <- as.factor(sampleInfo$pt)
sampleInfo$pn <- revalue(sampleInfo$pn, c("1mi"="1"))
sampleInfo$pn <- as.factor(sampleInfo$pn)
sampleInfo$ptnm <- as.factor(sampleInfo$ptnm)

# Variables que descriuen el tractament
sampleInfo$surgery <- as.factor(sampleInfo$surgery)
sampleInfo$radiotherapy <- factor(sampleInfo$radiotherapy, labels=c("NA", "No", "Yes"))
sampleInfo$radiotherapy <- factor(sampleInfo$radiotherapy, levels=c("No", "Yes", "NA"))
sampleInfo$typeRadiotherapy <- factor(sampleInfo$typeRadiotherapy, labels=c("NA","EBR", "PBI"))
sampleInfo$typeRadiotherapy <- factor(sampleInfo$typeRadiotherapy, levels=c("EBR", "PBI", "NA"))
sampleInfo$chemotherapy <- factor(sampleInfo$chemotherapy, labels=c("No","NA", "Yes"))
sampleInfo$chemotherapy <- factor(sampleInfo$chemotherapy, levels = c ("No", "Yes", "NA"))

# Variables que descriuen el post-tractament
sampleInfo$recurrence <- as.factor(sampleInfo$recurrence)
sampleInfo$survivor <- as.factor(sampleInfo$survivor)

summary(sampleInfo)

# Es representen gràficament les variables que descriuen els pacients
options(repr.plot.width=12, repr.plot.height=12)
par(mar=c(5, 5, 4, 2) + 0.1, cex=2.3, cex.lab=2) 

hist(sampleInfo$age, xlim = c(20, 100), main = "Edat de les pacients", xlab = "", ylab = "", col = "darkgreen")
plot(factor(sampleInfo$pensioner), yaxp = c(0, 300, 12), main = "Pacients en edat de jubilació", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$race), yaxp = c(0, 425, 17), main = "Raça de les pacients", col = c("darkgreen", "darkblue", "darkred", "purple"))

# Es representen gràficament les variables que descriuen la malaltia
par(mar=c(5, 5, 4, 2) + 0.1, cex=2.3, cex.lab=2)

plot(factor(sampleInfo$histology), yaxp = c(0, 375, 15), main = "Histologia del tumor primari de mama", 
     col = c("darkgreen", "darkblue", "darkred"))
plot(factor(sampleInfo$er), yaxp = c(0, 325, 13), main = "Estat del receptor d'estrogen", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$pr), yaxp = c(0, 300, 12), main = "Estat del receptor de progesterona", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$her2), yaxp = c(0, 375, 15), main = "Estat del factor de creixement epidèrmic humà tipus 2", 
     col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$pt), yaxp = c(0, 225, 9), main = "Classificació del tumor", col = c("darkgreen", "darkblue", "darkred", "orange"))
plot(factor(sampleInfo$pn), yaxp = c(0, 275, 11), main = "Classificació dels limfonodes regionals",
     col = c("darkgreen", "darkblue", "darkred"))
plot(factor(sampleInfo$ptnm), yaxp = c(0, 225, 9), main = "Estadis segons classificació TNM", col = c("darkgreen", "darkblue", "darkred"))

# Es representen gràficament les variables que descriuen el tractament
par(mar=c(5, 5, 4, 2) + 0.1, cex=2.3, cex.lab=2)

plot(factor(sampleInfo$surgery), yaxp = c(0, 300, 12), main = "Tipus de cirurgia", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$radiotherapy), yaxp = c(0, 300, 12), main = "Tractament amb radioteràpia", 
     col = c("darkgreen", "darkblue", "purple"))
typeRadiotherapy <- sampleInfo$typeRadiotherapy[!sampleInfo$typeRadiotherapy == "NA"]
plot(factor(typeRadiotherapy), yaxp = c(0, 225, 9), main = "Tipus de radioteràpia rebuda", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$chemotherapy), yaxp = c(0, 300, 12), main = "Tractament amb quimioteràpia", 
     col = c("darkgreen", "darkblue", "purple"))

# Es representen gràficament les variables que descriuen el post-tractament
par(mar=c(5, 5, 4, 2) + 0.1, cex=2.3, cex.lab=2)

plot(factor(sampleInfo$recurrence), yaxp = c(0, 450, 10), main = "Recurrència locorregional", col = c("darkgreen", "darkblue"))
plot(factor(sampleInfo$survivor), yaxp = c(0, 400, 8), main = "Pacients supervivents a la malaltia", col = c("darkgreen", "darkblue"))


# ES CREA UNA FUNCIÓ PER CALCULAR LES MÈTRIQUES DELS MODELS
evaluate_model <- function(predictions, labels) {
  # Funció per avaluar un model de classificació
  
  # Paràmetres:
  # - predictions: Vector de prediccions generades pel model.
  # - labels: Vector d'etiquetes reals corresponents a les dades de prova.
  
  # Llibreries necessàries:
  # - caret: Per crear la matriu de confusió.
  # - MLmetrics: Per calcular mètriques davaluació com Accuracy, Precision, Recall i F1 Score.
  # - pROC: Per calcular la corba ROC i AUC.
  
  # Es crea la matriu de confusió
  conf_matrix <- confusionMatrix(predictions, labels)
  
  # Es calculen les mètriques bàsiques
  accuracy <- MLmetrics::Accuracy(predictions, labels)
  precision <- MLmetrics::Precision(predictions, labels)
  recall <- MLmetrics::Recall(predictions, labels)
  f1_score <- MLmetrics::F1_Score(predictions, labels)
  
  # Es calcula l'AUC
  roc_curve <- pROC::multiclass.roc(labels,  as.numeric(predictions))
  auc <- pROC::auc(roc_curve)
  
  # Es crea un data frame amb totes les mètriques
  metrics <- data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    AUC = auc
  )
  
  return(metrics)
}


# ES CREA UNA FUNCIÓ PER FILTRAR LES DADES AMB F, REDUIR LA DIMENSIONALITAT AMB PCA O AMBDUES
# Es crea la variable index amb la que es realitzará totes les particions train i test
set.seed(123)
index<- createDataPartition(sampleInfo$ptnm, p = 0.8, list = FALSE)

preprocess_data <- function(data = normalized_data, labels = sampleInfo$ptnm, method = c("f", "pca", "f_pca"), genes = 1000, dim = 60, index = index, split_ratio = 0.8) {
  # Funció per preprocessar dades d'expressió gènica
  
  # Paràmetres:
  # - data: Dataframe amb les dades normalitzades d'expressió gènica.
  # - labels: Vector amb les etiquetes corresponents a cada mostra.
  # - method: Mètode de preprocessament a utilitzar ("f", "pca", "f_pca").
  # - genes: Nombre de gens a seleccionar basat en el F (per defecte, 1000).
  # - dim: Nombre de components principals a seleccionar en l'anàlisi PCA (per defecte, 60).
  # - index: Índexs per dividir les dades en conjunts d'entrenament i prova.
  # - split_ratio: Proporció de dades per al conjunt d'entrenament (per defecte, 0.8).
  
  # Llibreries necessàries:
  # - genefilter: Per calcular el F.
  # - stats: Per realitzar l'anàlisi PCA.
  
  method <- match.arg(method)
  pca_result <- NULL
  pca_data <- NULL
  processed_data <- NULL
  
  if (method == "f") {
    # Es converteixen les etiquetes a factor
    labels <- as.factor(labels)
    
    # Es calcula l'F per a cada gen
    f <- rowFtests(data, labels)$statistic
    
    # Se seleccionen els gens amb els F-Scores més alts
    top_genes <- order(f, decreasing = TRUE)[1:genes]
    
    # Es creen un nou dataframe amb els gens seleccionats
    selected_genes_data <- data[top_genes, ]
    
    # Es crea el dataframe amb què es treballarà
    processed_data <- as.data.frame(t(selected_genes_data))
    processed_data$labels <- labels
  }
  
  if (method == "pca") {
    # Es transposen les dades normalitzades
    pca_data <- as.data.frame(t(data))
    
    # Es realitza PCA
    pca_result <- prcomp(pca_data, scale. = TRUE)
    
    # Se seleccionen els components principals
    processed_data <- as.data.frame(pca_result$x[, 1:dim])
    processed_data$labels <- labels
  }
  
  if (method == "f_pca") {
    # Es converteixen les etiquetes a factor
    labels <- as.factor(labels)
    
    # Es calcula l'F-Score per a cada gen
    f <- rowFtests(data, labels)$statistic
    
    # Se seleccionen els gens amb els F-Scores més alts
    top_genes <- order(f, decreasing = TRUE)[1:genes]
    
    # Es crea un nou dataframe amb els gens seleccionats
    selected_genes_data <- data[top_genes, ]
    
    # Es crea el dataframe amb què es treballarà
    f_data <- as.data.frame(t(selected_genes_data))
    pca_data <- f_data
    
    # Es realitza PCA
    pca_result <- prcomp(f_data, scale. = TRUE)
    
    # Se seleccionen els components principals
    processed_data <- as.data.frame(pca_result$x[, 1:dim])
    processed_data$labels <- labels
  }
  
  # Es calcula l'index si no se'n proporciona un
  if (is.null(index)) {
    set.seed(123)
    index <- sample(1:nrow(processed_data), size = split_ratio * nrow(processed_data))
  }
  
  # Es dividixen les dades en conjunts d'entrenament i prova
  train_data <- processed_data[index, ]
  test_data <- processed_data[-index, ]
  
  # Es tornen els conjunts de dades d'entrenament i prova, i els resultats PCA si es van aplicar
  return(list(train_data = train_data, test_data = test_data, pca_result = pca_result, pca_data = pca_data))
}


# ES CREA UNA FUNCIÓ PER BALANCEJAR LES CLASES, REDUIR REGISTRES PER IGUALAR LES CLASSES O AMBDUES
process_train <- function(train_data, test_data, method = c("smote", "undersample", "both"), k = 5, dup_size = 0) {
  # Funció per balancejar dades d'entrenament
  
  # Paràmetres:
  # - train_data: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test_data: Dataframe amb les dades de prova, utilitzat per copiar els noms de les columnes.
  # - method: Mètode de balanceig a utilitzar ("smote", "undersample", "both").
  # - k: Nombre de veïns més propers a utilitzar en el mètode SMOTE (per defecte, 5).
  # - dup_size: Mida de duplicació en el mètode SMOTE (per defecte, 0).
  
  # Llibreries necessàries:
  # - smotefamily: Per aplicar el mètode SMOTE.
  # - dplyr: Per manipular i submostrejar les dades.
  
  method <- match.arg(method)
  if (method == "smote" || method == "both") {
    # S'aplica SMOTE per balancejar les dades
    set.seed(123)
    smote_result <- smotefamily::SMOTE(train_data[, -ncol(train_data)], train_data$labels, K = k, dup_size = dup_size)
    
    # Es fusionen les dades balancejades
    train_data <- data.frame(smote_result$data)
    train_data <- dplyr::rename(train_data, labels = class)
    train_data$labels <- as.factor(train_data$labels)
  }
  
  if (method == "undersample" || method == "both") {
    # Es determina la mida de la classe minoritària
    min_class_size <- min(table(train_data$labels))
    
    # Se submostregen les classes per igualar la mida
    set.seed(123)
    train_data <- train_data %>%
      dplyr::group_by(labels) %>%
      dplyr::sample_n(min_class_size) %>%
      dplyr::ungroup()
  }
  
  # Es copien els noms de les columnes de test_data
  col_names_test <- colnames(test_data)
  
  # S'assignen els noms de columnes corregits a train_data
  colnames(train_data) <- col_names_test
  
  return(train_data)
}


# ES CALCULA EL NOMBRE ÓPTIM DE LES COMPONENTS PRINCIPALS
# Es transposen les dades
data_transposed <- t(normalized_data)

# Es realitza PCA
pca_result <- prcomp(data_transposed, scale. = TRUE)

# Es calcula la variància explicada
explained_variance <- summary(pca_result)$importance[2, ]

# Es grafica la variància explicada acumulada
plot(cumsum(explained_variance), type = "b", xlab = "Nombre de components", ylab = "Variança explicada acumulada", main = "Gràfic de colze")
abline(h = 0.90, col = "red", lty = 2)  # Línea de referencia para el 90% de varianza explicada

# Valors propis
eigenvalues <- pca_result$sdev^2

# Es grafiquen els valors propis
plot(eigenvalues, type = "b", xlab = "Nombre de components", ylab = "Valors propis", main = "Scree Plot")
abline(h = 1, col = "red", lty = 2)  # Línea de referencia para el criterio de Kaiser

# Nombre de components segons el criteri de Kaiser
num_components_kaiser <- sum(eigenvalues > 1)
num_components_kaiser


# S'ENTRENA MODEL KNN AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 1000, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

model_knn <- function(train, test, k = 3) {
  # Funció per entrenar i avaluar un model KNN
  
  # Paràmetres:
  # - train: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test: Dataframe amb les dades de prova, incloent les etiquetes.
  # - k: Nombre de veïns més propers a utilitzar en el mètode KNN (per defecte, 3).
  
  # Llibreries necessàries:
  # - class: Per aplicar el mètode KNN.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la precisió per classe.
  
  # Es preparen les dades d´entrenament i prova
  train_data <- train[, -ncol(train)]
  train_labels <- train$labels
  test_data <- test[, -ncol(test)]
  test_labels <- test$labels
  
  # S'entrena i predeix amb KNN
  set.seed(123)
  predictions_knn <- class::knn(train = train_data, test = test_data, cl = train_labels, k = k)
  
  # S'avalua el model KNN
  metrics_knn <- evaluate_model(predictions_knn, test_labels)
  
  # Es crea la matriu de confusió
  confusion_matrix_knn <- confusionMatrix(predictions_knn, test_labels)
  cm_knn <- as.data.frame(confusion_matrix_knn$table)
  
  # Es crea el gràfic de la matriu de confusió
  plot_confusion <- ggplot(cm_knn, aes(x = Reference, y = Prediction, fill = Freq)) + 
    geom_tile() + 
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "blue") + 
    labs(title = "Matriu de confusió amb KNN", x = "Valors reals", y = "Prediccions") +  
    theme( 
      axis.text.y = element_text(size = 24), 
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18)
    )
  
  # Es crea la taula de contingència
  contingency_table_knn <- table(predictions_knn, test_labels)
  
  # Es calcula la precisió per classe (probabilitat condicional)
  precision_knn <- prop.table(contingency_table_knn, 2)
  
  # Es crea un dataframe per graficar
  precision_knn_df <- as.data.frame(as.table(precision_knn))
  colnames(precision_knn_df) <- c("Predicció", "Classe_Real", "Precisió")
  
  # Es crea el gràfic de precisió per classe
  plot_precision <- ggplot(precision_knn_df, aes(x = Classe_Real, y = Precisió, fill = Predicció)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Precisió de KNN per classe",
         x = "Classe real",
         y = "Precisió") +
    theme(
      axis.text.y = element_text(size = 24), 
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18)
    )
  # Es torna les mètriques i els gràfics
  return(list(metrics = metrics_knn, plot_confusion = plot_confusion, plot_precision = plot_precision))
}

knn_f <- model_knn(train_f, test_f)
metrics_knn_f <- knn_f$metrics
plot_confusion_knn_f <- knn_f$plot_confusion
plot_precision_knn_f <- knn_f$plot_precision

metrics_knn_f
# S'ajusta la mida dels gràfics a mostrar a la pantalla
options(repr.plot.width=12, repr.plot.height=12)
plot_confusion_knn_f
plot_precision_knn_f


# S'ENTRENA MODEL KNN AMB PCA
data_pca <- preprocess_data(method = "pca", dim = 29, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data

knn_pca <- model_knn(train_pca, test_pca)
metrics_knn_pca <- knn_pca$metrics
plot_confusion_knn_pca <- knn_pca$plot_confusion
plot_precision_knn_pca <- knn_pca$plot_precision

metrics_knn_pca
plot_confusion_knn_pca
plot_precision_knn_pca


# S'ENTRENA MODEL KNN AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 500, dim = 15, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data

knn_f_pca <- model_knn(train_f_pca, test_f_pca)
metrics_knn_f_pca <- knn_f_pca$metrics
plot_confusion_knn_f_pca <- knn_f_pca$plot_confusion
plot_precision_knn_f_pca <- knn_f_pca$plot_precision

metrics_knn_f_pca
plot_confusion_knn_f_pca
plot_precision_knn_f_pca


# S'ENTRENA MODEL KNN AMB DADES BALANCEJADES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_smote <- process_train(train_f_pca, test_f_pca, method = "smote")
knn_smote <- model_knn(train_smote, test_f_pca)
metrics_knn_smote <- knn_smote$metrics
plot_confusion_knn_smote <- knn_smote$plot_confusion
plot_precision_knn_smote <- knn_smote$plot_precision

metrics_knn_smote
plot_confusion_knn_smote
plot_precision_knn_smote


# S'ENTRENA MODEL KNN AMB DADES REDUÏDES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_reduce <- process_train(train_f_pca, test_f_pca, method = "undersample")
knn_reduce <- model_knn(train_reduce, test_f_pca)
metrics_knn_reduce <- knn_reduce$metrics
plot_confusion_knn_reduce <- knn_reduce$plot_confusion
plot_precision_knn_reduce <- knn_reduce$plot_precision

metrics_knn_reduce
plot_confusion_knn_reduce
plot_precision_knn_reduce


# S'ENTRENA MODEL KNN AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_smote_reduce <- process_train(train_f_pca, test_f_pca, method = "both")
knn_smote_reduce <- model_knn(train_smote_reduce, test_f_pca)
metrics_knn_smote_reduce <- knn_smote_reduce$metrics
plot_confusion_knn_smote_reduce <- knn_smote_reduce$plot_confusion
plot_precision_knn_smote_reduce <- knn_smote_reduce$plot_precision

metrics_knn_smote_reduce
plot_confusion_knn_smote_reduce
plot_precision_knn_smote_reduce


# S'ENTRENA MODEL DT AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 600, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

model_dt <- function(train, test, gen_annotation = genAnnotation, num_genes = 25, pca_result = NULL, pca_components = NULL) {
  # Funció per entrenar i avaluar un model d'arbre de decisió
  
  # Paràmetres:
  # - train: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test: Dataframe amb les dades de prova, incloent les etiquetes.
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - num_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - pca_result: Objecte opcional amb els resultats de l'anàlisi PCA.
  # - pca_components: Nombre de components principals a utilitzar en el càlcul de la importància dels gens.
  
  # Llibreries necessàries:
  # - rpart: Per entrenar el model d'arbre de decisió.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  # S'entrena el model d'arbre de decisió
  tree_model <- rpart(labels ~ ., data = train, method = "class")
  
  # S'avalua el model
  predictions_dt <- predict(tree_model, test, type = "class")
  metrics_dt <- evaluate_model(predictions_dt, test$labels)
  
  # Es crea la matriu de confusió
  confusion_matrix_dt <- confusionMatrix(predictions_dt, test$labels)
  cm_dt <- as.data.frame(confusion_matrix_dt$table)
  
  # Es crea el gràfic de la matriu de confusió
  plot_confusion <- ggplot2::ggplot(cm_dt, ggplot2::aes(x = Reference, y = Prediction, fill = Freq)) + 
    ggplot2::geom_tile() + 
    ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1, size = 8) +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") + 
    ggplot2::labs(title = "Matriu de confusió amb DT", x = "Valors reals", y = "Prediccions") + 
    ggplot2::theme( 
      axis.text.y = ggplot2::element_text(size = 24), 
      axis.text.x = ggplot2::element_text(size = 24),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 20),
      legend.text = ggplot2::element_text(size = 18)
    )
  
  # S'obté la importància de les característiques
  importance <- tree_model$variable.importance
  importance_df <- data.frame(Dimension = names(importance), Importance = importance)
  
  if (!is.null(pca_result) && !is.null(pca_components)) {
    loadings <- pca_result$rotation[, 1:pca_components]
    
    # Es mapegen les dimensions principals als gens originals
    gene_importance <- rowSums(abs(loadings) * importance[colnames(loadings)])
    gene_importance_df <- data.frame(Gene = rownames(loadings), Importance = gene_importance)
    top_genes <- gene_importance_df[order(-gene_importance_df$Importance), ][1:num_genes, ]
  } else {
    top_genes <- importance_df[order(-importance_df$Importance), ][1:num_genes, ]
    colnames(top_genes)[1] <- "Gene"  # Asegurarse de que la columna se llama "Gene"
  }
  
  # Es crea un gràfic de barres dels gens més importants amb els noms
  plot_importancia <- ggplot2::ggplot(top_genes, ggplot2::aes(x = reorder(Gene, Importance), y = Importance)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Importància dels gens més rellevants\namb DT",
                  x = "Genes",
                  y = "Importancia") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 20), 
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtra fdata per incloure només els gens més importants
  fdata_dt <- gen_annotation[gen_annotation$ID %in% top_genes$Gene, ]
  
  # S'ordena fdata_dt perquè coincideixi amb l'ordre de top_genes
  fdata_dt <- fdata_dt[match(top_genes$Gene, fdata_dt$ID), ]
  
  # Es visualitza l'arbre de decisió
  tree_plot <- recordPlot({
    plot(tree_model)
    text(tree_model, use.n = TRUE)
  })
  
  return(list(
    tree_plot = tree_plot,
    metrics_dt = metrics_dt,
    top_genes = top_genes,
    plot_confusion = plot_confusion,
    plot_importancia = plot_importancia,
    fdata_dt = fdata_dt
  ))
}

dt_f <- model_dt(train_f, test_f)
metrics_dt_f <- dt_f$metrics_dt
plot_confusion_dt_f <- dt_f$plot_confusion
plot_precision_dt_f <- dt_f$plot_importancia
fdata_dt_f <- dt_f$fdata_dt

metrics_dt_f
plot_confusion_dt_f
plot_precision_dt_f
fdata_dt_f


# S'ENTRENA MODEL DT PCA
data_pca <- preprocess_data(method = "pca", dim = 465, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data
result_pca <- data_pca$pca_result
data_pca <- data_pca$pca_data

dt_pca <- model_dt(train_pca, test_pca, pca_result = result_pca, pca_components = 465)
metrics_dt_pca <- dt_pca$metrics_dt
plot_confusion_dt_pca <- dt_pca$plot_confusion
fdata_dt_pca <- dt_pca$fdata_dt

metrics_dt_pca
plot_confusion_dt_pca
fdata_dt_pca


# S'ENTRENA MODEL DT AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 100, dim = 15, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data
result_f_pca <- data_f_pca$pca_result

dt_f_pca <- model_dt(train_f_pca, test_f_pca, pca_result = result_f_pca, pca_components = 15)
metrics_dt_f_pca <- dt_f_pca$metrics_dt
plot_confusion_dt_f_pca <- dt_f_pca$plot_confusion
fdata_dt_f_pca <- dt_f_pca$fdata_dt

metrics_dt_f_pca
plot_confusion_dt_f_pca
fdata_dt_f_pca


# S'ENTRENA MODEL DT AMB DADES BALANCEJADES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_smote <- process_train(train_f_pca, test_f_pca, method = "smote")
dt_smote <- model_dt(train_smote, test_f_pca, pca_result = result_f_pca, pca_components = 15)
metrics_dt_smote <- dt_smote$metrics_dt
plot_confusion_dt_smote <- dt_smote$plot_confusion
fdata_dt_smote <- dt_smote$fdata_dt

metrics_dt_smote
plot_confusion_dt_smote
fdata_dt_smote


# S'ENTRENA MODEL DT AMB DADES REDUÏDES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_reduce <- process_train(train_f_pca, test_f_pca, method = "undersample")
dt_reduce <- model_dt(train_reduce, test_f_pca, pca_result = result_f_pca, pca_components = 15)
metrics_dt_reduce <- dt_reduce$metrics_dt
plot_confusion_dt_reduce <- dt_reduce$plot_confusion
fdata_dt_reduce <- dt_reduce$fdata_dt

metrics_dt_reduce
plot_confusion_dt_reduce
fdata_dt_reduce


# S'ENTRENA MODEL DT AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f_pca i test_f_pca que són els que millor accuracy mostrava
train_smote_reduce <- process_train(train_f_pca, test_f_pca, method = "both")
dt_smote_reduce <- model_dt(train_smote_reduce, test_f_pca, pca_result = result_f_pca, pca_components = 15)
metrics_dt_smote_reduce <- dt_smote_reduce$metrics_dt
plot_confusion_dt_smote_reduce <- dt_smote_reduce$plot_confusion
fdata_dt_smote_reduce <- dt_smote_reduce$fdata_dt

metrics_dt_smote_reduce
plot_confusion_dt_smote_reduce
fdata_dt_smote_reduce


# S'ENTRENA MODEL SVM AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 10000, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

model_svm <- function(train, test, method = c("f", "pca"), top_n_genes = 25, gen_annotation = genAnnotation, dim = NULL, pca_result = NULL, pca_data = NULL) {   
  # Funció per entrenar i avaluar un model SVM
  
  # Paràmetres:
  # - train: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test: Dataframe amb les dades de prova, incloent les etiquetes.
  # - method: Mètode de selecció de característiques a utilitzar ("f", "pca").
  # - top_n_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - dim: Nombre de components principals a utilitzar en el càlcul de la importància dels gens.
  # - pca_result: Objecte opcional amb els resultats de l'anàlisi PCA.
  # - pca_data: Dataframe opcional amb les dades transformades per PCA.
  
  # Llibreries necessàries:
  # - e1071: Per entrenar el model SVM.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  method <- match.arg(method)
  set.seed(123)
  
  # S'entrena amb SVM
  model_svm <- e1071::svm(labels ~ ., data = train, type = 'C-classification', kernel = "linear")
  
  # Es fa predicció i avaluació
  predictions_svm <- predict(model_svm, newdata = test)
  
  # S'avalua el model
  metrics_svm <- evaluate_model(predictions_svm, test$labels)
  
  # Es realitza la matriu de confusió
  confusion_matrix_svm <- confusionMatrix(predictions_svm, test$labels)
  cm_svm <- as.data.frame(confusion_matrix_svm$table)
  
  # Es renomenen les columnes perquè siguin consistents amb els noms esperats
  colnames(cm_svm) <- c("Reference", "Prediction", "Freq")
  
  # Es crear el gràfic de la matriu de confusió
  plot_confusion <- ggplot2::ggplot(cm_svm, ggplot2::aes(x = Reference, y = Prediction, fill = Freq)) + 
    ggplot2::geom_tile() + 
    ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1, size = 8) +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") + 
    ggplot2::labs(title = "Matriu de confusió amb SVM", x = "Valors Reals", y = "Prediccions") + 
    ggplot2::theme( 
      axis.text.y = ggplot2::element_text(size = 24), 
      axis.text.x = ggplot2::element_text(size = 24),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 20),
      legend.text = ggplot2::element_text(size = 18)
    )
  
  if (method == "fscore") {
    # S'obtenen els coeficients del model
    coeficientes_svm <- t(model_svm$coefs) %*% model_svm$SV
    
    # S'extreuen els noms dels gens corresponents
    gene_names_svm <- colnames(train)
    
    # Es fa la mitjana dels coeficients absoluts per característica
    coef_abs_svm <- apply(abs(coeficientes_svm), 2, mean)
    
    # S'ordenen els coeficients pel seu valor absolut
    importancia_genes_svm <- sort(coef_abs_svm, decreasing = TRUE)
    
    # S'ordenen els noms dels gens en el mateix ordre que la importància
    sorted_indices_svm <- order(coef_abs_svm, decreasing = TRUE)
    sorted_gene_names_svm <- gene_names_svm[sorted_indices_svm]
    
    # Se seleccionen els noms dels 25 gens més importants
    top_gene_names_svm <- sorted_gene_names_svm[1:top_n_genes]
    
    # Es crea un dataframe amb els gens més importants i les seues importàncias
    importancias_svm <- data.frame(
      Gen = top_gene_names_svm,
      Importancia = importancia_genes_svm[1:top_n_genes]
    )
  }
  
  if (method == "pca") {
    # S'obtenen els coeficients del model
    coeficientes_svm <- as.matrix(t(model_svm$coefs) %*% model_svm$SV)
    
    # S'obtenen els Loadings de PCA
    loadings_pca <- as.matrix(pca_result$rotation[, 1:dim])
    
    # Es calcula les importàncies dels components PCA i dels gens
    importancias_componentes_svm <- abs(coeficientes_svm %*% t(loadings_pca))
    importancia_genes_svm <- colSums(importancias_componentes_svm)
    genes_importantes_svm <- order(importancia_genes_svm, decreasing = TRUE)[1:25]
    top_gene_names_svm <- colnames(pca_data)[genes_importantes_svm]
    
    # Es crea un dataframe amb els gens més importants i les seves importàncias
    importancias_svm <- data.frame(
      Gen = top_gene_names_svm,
      Importancia = importancia_genes_svm[genes_importantes_svm]
    )
  }
  
  # Es crea un gràfic de barres dels gens més importants amb els noms
  plot_importancia <- ggplot2::ggplot(importancias_svm, ggplot2::aes(x = reorder(Gen, Importancia), y = Importancia)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Importància dels gens més rellevants\namb SVM",
                  x = "Gens",
                  y = "Importància") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 20), 
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtra fdata per incloure només els gens més importants
  fdata_svm <- gen_annotation[gen_annotation$ID %in% top_gene_names_svm, ]
  
  # S'ordenen top_genes_fdata perquè coincideixin amb l'ordre de top_gene_names
  fdata_svm <- fdata_svm[match(top_gene_names_svm, fdata_svm$ID), ]
  
  # Es retornen els resultats
  return(list(
    metrics = metrics_svm,
    plot_confusion = plot_confusion,
    plot_importancia = plot_importancia,
    fdata_svm = fdata_svm
  ))
}

svm_f <- model_svm(train_f, test_f, method = "f")
metrics_svm_f <- svm_f$metrics
plot_confusion_svm_f <- svm_f$plot_confusion
plot_importancia_svm_f <- svm_f$plot_importancia
fdata_svm_f <- svm_f$fdata_svm

metrics_svm_f
plot_confusion_svm_f
plot_importancia_svm_f
fdata_svm_f


# S'ENTRENA MODEL SVM AMB PCA
data_pca <- preprocess_data(method = "pca", dim = 96, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data
result_pca <- data_pca$pca_result
data_pca <- data_pca$pca_data

svm_pca <- model_svm(train_pca, test_pca, method = "pca", dim = 96, pca_result = result_pca, pca_data = data_pca)
metrics_svm_pca <- svm_pca$metrics
plot_confusion_svm_pca <- svm_pca$plot_confusion
plot_importancia_svm_pca <- svm_pca$plot_importancia
fdata_svm_pca <- svm_pca$fdata_svm

metrics_svm_pca
plot_confusion_svm_pca
plot_importancia_svm_pca
fdata_svm_pca


# S'ENTRENA MODEL SVM AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 10000, dim = 95, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data
result_f_pca <- data_f_pca$pca_result
data_f_pca <- data_f_pca$pca_data

svm_f_pca <- model_svm(train_f_pca, test_f_pca, method = "pca", dim = 95, pca_result = result_f_pca, pca_data = data_f_pca)
metrics_svm_f_pca <- svm_f_pca$metrics
plot_confusion_f_svm_pca <- svm_f_pca$plot_confusion
plot_importancia_svm_f_pca <- svm_f_pca$plot_importancia
fdata_svm_f_pca <- svm_f_pca$fdata_svm

metrics_svm_f_pca
plot_confusion_f_svm_pca
plot_importancia_svm_f_pca
fdata_svm_f_pca


# S'ENTRENA MODEL SVM AMB DADES BALANCEJADES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostrava
train_smote <- process_train(train_f, test_f, method = "smote")
svm_smote <- model_svm(train_smote, test_f, method = "fscore")
metrics_svm_smote <- svm_smote$metrics
plot_confusion_svm_smote <- svm_smote$plot_confusion
plot_importancia_svm_smote <- svm_smote$plot_importancia
fdata_svm_smote <- svm_smote$fdata_svm

metrics_svm_smote
plot_confusion_svm_smote
plot_importancia_svm_smote
fdata_svm_smote


# S'ENTRENA MODEL SVM AMB DADES REDUÏDES

# S'aplica a train_f i test_f ja que són els que millor accuracy mostrava
train_reduce <- process_train(train_f, test_f, method = "undersample")
svm_reduce <- model_svm(train_reduce, test_f, method = "fscore")
metrics_svm_reduce <- svm_reduce$metrics
plot_confusion_svm_reduce <- svm_reduce$plot_confusion
plot_importancia_svm_reduce <- svm_reduce$plot_importancia
fdata_svm_reduce <- svm_reduce$fdata_svm

metrics_svm_reduce
plot_confusion_svm_reduce
plot_importancia_svm_reduce
fdata_svm_reduce


# S'ENTRENA MODEL SVM AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostrava
train_smote_reduce <- process_train(train_f, test_f, method = "both")
svm_smote_reduce <- model_svm(train_smote_reduce, test_f, method = "fscore")
metrics_svm_smote_reduce <- svm_smote_reduce$metrics
plot_confusion_svm_smote_reduce <- svm_smote_reduce$plot_confusion
plot_importancia_svm_smote_reduce <- svm_smote_reduce$plot_importancia
fdata_svm_smote_reduce <- svm_smote_reduce$fdata_svm

metrics_svm_smote_reduce
plot_confusion_svm_smote_reduce
plot_importancia_svm_smote_reduce
fdata_svm_smote_reduce


# S'ENTRENA MODEL RF AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 100, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

model_rf <- function(train, test, gen_annotation = genAnnotation, ntree = 500, top_n_genes = 25, dim = 60, pca_result = NULL, pca_data = NULL) {
  # Funció per entrenar i avaluar un model Random Forest
  
  # Paràmetres:
  # - train: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test: Dataframe amb les dades de prova, incloent les etiquetes.
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - ntree: Nombre d'arbres a utilitzar en el model Random Forest (per defecte, 500).
  # - top_n_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - dim: Nombre de components principals a utilitzar en el càlcul de la importància dels gens.
  # - pca_result: Objecte opcional amb els resultats de l'anàlisi PCA.
  # - pca_data: Dataframe opcional amb les dades transformades per PCA.
  
  # Llibreries necessàries:
  # - randomForest: Per entrenar el model Random Forest.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  # Es preparen les dades d´entrenament i prova
  train_data <- train[, -ncol(train)]
  train_labels <- train$labels
  test_data <- test[, -ncol(test)]
  test_labels <- test$labels
  
  # S'entrena el model Random Forest
  set.seed(123)
  model_rf <- randomForest::randomForest(x = train_data, y = train_labels, ntree = ntree, mtry = sqrt(ncol(train_data)))
  
  # Es fa predicció i avaluació del model
  predictions_rf <- predict(model_rf, newdata = test_data)
  
  # Es realitza avaluació el model
  metrics_rf <- evaluate_model(predictions_rf, test$labels)
  
  # Es realitza la matriu de confusió
  confusion_matrix_rf <- confusionMatrix(predictions_rf, test$labels)
  cm_rf <- as.data.frame(confusion_matrix_rf$table)
  
  # Es crea el gràfic de la matriu de confusió
  plot_confusion <- ggplot2::ggplot(cm_rf, ggplot2::aes(x = Reference, y = Prediction, fill = Freq)) + 
    ggplot2::geom_tile() + 
    ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1, size = 8) +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") + 
    ggplot2::labs(title = "Matriu de confusió amb Random Forest", x = "Valors reals", y = "Prediccions") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme( 
      axis.text.y = ggplot2::element_text(size = 24), 
      axis.text.x = ggplot2::element_text(size = 24),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5),
      legend.title = ggplot2::element_text(size = 20),
      legend.text = ggplot2::element_text(size = 18)
    )
  
  # Visualización de la Importancia de los Genes
  importance_rf <- randomForest::importance(model_rf)
  
  if (!is.null(pca_result) && !is.null(pca_data)) {
    # Es calcula la importància dels components
    importance_df <- data.frame(Componente = rownames(importance_rf), Importancia = importance_rf[, 1])
    
    # S'obtenen els Loadings de PCA
    loadings_pca <- pca_result$rotation[, 1:dim]
    
    # Es calculen les importància dels dels genss
    importancia_genes <- abs(loadings_pca %*% importance_rf[, 1])
    importancia_genes_df <- data.frame(Gen = rownames(loadings_pca), Importancia = rowSums(importancia_genes))
    top_genes_rf <- importancia_genes_df[order(-importancia_genes_df$Importancia), ][1:top_n_genes, ]
    
    # Es crea un dataframe amb els gens més importants i les seues importàncias
    importancias_rf <- data.frame(
      Gen = top_genes_rf$Gen,
      Importancia = top_genes_rf$Importancia
    )
  } else {
    # Es visualitza la importància dels gens
    importance_df <- data.frame(Gen = rownames(importance_rf), Importancia = importance_rf[, 1])
    
    # Se seleccionen els 25 gens més importants
    top_genes_rf <- importance_df[order(-importance_df$Importancia), ][1:top_n_genes, ]
    
    # Es crea un dataframe amb els gens més importants i les seves importàncies sense PCA
    importancias_rf <- data.frame(
      Gen = top_genes_rf$Gen,
      Importancia = top_genes_rf$Importancia
    )
  }
  
  # Es crea un gràfic de barres de la importància dels gens
  plot_importancia <- ggplot2::ggplot(importancias_rf, ggplot2::aes(x = reorder(Gen, Importancia), y = Importancia)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Importància dels gens més rellevants\namb RF",
                  x = "Gens",
                  y = "Importància") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 20), 
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtra fdata per incloure només els gens més importants
  fdata_rf <- gen_annotation[gen_annotation$ID %in% importancias_rf$Gen, ]
  
  # S'ordena fdata_rf perquè coincideixi amb l'ordre d'importància_rf
  fdata_rf <- fdata_rf[match(importancias_rf$Gen, fdata_rf$ID), ]
  
  # Es retornen els resultats
  return(list(
    metrics = metrics_rf,
    plot_confusion = plot_confusion,
    plot_importancia = plot_importancia,
    fdata_rf = fdata_rf
  ))
}

rf_f <- model_rf(train_f, test_f, ntree = 400)
metrics_rf_f <- rf_f$metrics
plot_confusion_rf_f <- rf_f$plot_confusion
plot_importancia_rf_f <- rf_f$plot_importancia
fdata_rf_f <- rf_f$fdata_rf

metrics_rf_f
plot_confusion_rf_f
plot_importancia_rf_f
fdata_rf_f


# S'ENTRENA MODEL RF AMB PCA
data_pca <- preprocess_data(method = "pca", dim = 110, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data
result_pca <- data_pca$pca_result
data_pca <- data_pca$pca_data

rf_pca <- model_rf(train_pca, test_pca, ntree = 650, dim = 110, pca_result = result_pca, pca_data = data_pca)
metrics_rf_pca <- rf_pca$metrics
plot_confusion_rf_pca <- rf_pca$plot_confusion
plot_importancia_rf_pca <- rf_pca$plot_importancia
fdata_rf_pca <- rf_pca$fdata_rf

metrics_rf_pca
plot_confusion_rf_pca
plot_importancia_rf_pca
fdata_rf_pca


# S'ENTRENA MODEL RF AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 500, dim = 40, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data
result_f_pca <- data_f_pca$pca_result
data_f_pca <- data_f_pca$pca_data

rf_f_pca <- model_rf(train_f_pca, test_f_pca, ntree = 900, dim = 40, pca_result = result_pca, pca_data = data_pca)
metrics_rf_f_pca <- rf_f_pca$metrics
plot_confusion_rf_f_pca <- rf_f_pca$plot_confusion
plot_importancia_rf_f_pca <- rf_f_pca$plot_importancia
fdata_rf_f_pca <- rf_f_pca$fdata_rf

metrics_rf_f_pca
plot_confusion_rf_f_pca
plot_importancia_rf_f_pca
fdata_rf_f_pca


# S'ENTRENA MODEL RF AMB DADES BALANCEJADES
# S'aplica a train_f_pca i test_f_pca ja que són els que millor accuracy mostrava
train_smote <- process_train(train_f_pca, test_f_pca, method = "smote")
rf_smote <- model_rf(train_smote, test_f_pca, genAnnotation, ntree = 500, dim = 40, pca_result = result_pca, pca_data = data_pca)
metrics_rf_smote <- rf_smote$metrics
plot_confusion_rf_smote <- rf_smote$plot_confusion
plot_importancia_rf_smote <- rf_smote$plot_importancia
fdata_rf_smote <- rf_smote$fdata_rf

metrics_rf_smote
plot_confusion_rf_smote
plot_importancia_rf_smote
fdata_rf_smote


# S'ENTRENA MODEL FR AMB DADES REDUÏDES
# S'aplica a train_f_pca i test_f_pca ja que són els que millor accuracy mostrava
train_reduce <- process_train(train_f_pca, test_f_pca, method = "undersample")
rf_reduce <- model_rf(train_reduce, test_f_pca, ntree = 1100, dim = 40, pca_result = result_pca, pca_data = data_pca)
metrics_rf_reduce <- rf_reduce$metrics
plot_confusion_rf_reduce <- rf_reduce$plot_confusion
plot_importancia_rf_reduce <- rf_reduce$plot_importancia
fdata_rf_reduce <- rf_reduce$fdata_rf

metrics_rf_reduce
plot_confusion_rf_reduce
plot_importancia_rf_reduce
fdata_rf_reduce


# S'ENTRENA MODEL RF AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f_pca i test_f_pca ja que són els que millor accuracy mostrava
train_smote_reduce <- process_train(train_f_pca, test_f_pca, method = "both")
rf_smote_reduce <- model_rf(train_smote_reduce, test_f_pca, ntree = 1100, dim = 40, pca_result = result_pca, pca_data = data_pca)
metrics_rf_smote_reduce <- rf_smote_reduce$metrics
plot_confusion_rf_smote_reduce <- rf_smote_reduce$plot_confusion
plot_importancia_rf_smote_reduce <- rf_smote_reduce$plot_importancia
fdata_rf_smote_reduce <- rf_smote_reduce$fdata_rf

metrics_rf_smote_reduce
plot_confusion_rf_smote_reduce
plot_importancia_rf_smote_reduce
fdata_rf_smote_reduce


# S'ENTRENA MODEL ANN AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 50, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

# Es defineix el model
set.seed(123)
model_ann_f_keras <- keras_model_sequential() %>%
  layer_dense(units = 254, activation = 'relu', input_shape = c(50)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

model_ann_f <- function(model, train, test, gen_annotation = genAnnotation, num_genes = 25, epochs = 100, batch_size = 32) {
  # Funció per entrenar i avaluar un model ANN amb dades originals o passades per un filtre
  
  # Paràmetres:
  # - model: Model de xarxa neuronal artificial (ANN) predefinit.
  # - train: Dataframe amb les dades d'entrenament, incloent les etiquetes.
  # - test: Dataframe amb les dades de prova, incloent les etiquetes.
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - num_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - epochs: Nombre d'èpoques per entrenar el model (per defecte, 100).
  # - batch_size: Mida del lot per entrenar el model (per defecte, 32).
  
  # Llibreries necessàries:
  # - keras: Per construir i entrenar el model ANN.
  # - DALEX: Per explicar el model ANN.
  # - ingredients: Per calcular la importància de les característiques.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  set.seed(123)
  # Se separen les característiques i les etiquetes
  train_features <- train[, -ncol(train)]
  train_labels <- as.numeric(train[, ncol(train)]) - 1
  test_features <- test[, -ncol(test)]
  test_labels <- as.numeric(test[, ncol(test)]) - 1
  
  # Es compila el model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  
  # Es converteixen les etiquetes a format categòric
  train_labels_cat <- to_categorical(train_labels, num_classes = 3)
  
  # S'entrena el model
  history <- model %>% fit(
    as.matrix(train_features), train_labels_cat,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = 0.2
  )
  
  # Es realitza les prediccions al conjunt de prova
  predictions <- model %>% predict(as.matrix(test_features)) %>% k_argmax()
  
  # Es converteixen les etiquetes i prediccions a factors
  true_labels <- as.factor(test_labels)
  pred_labels <- as.factor(as.array(predictions))
  
  # S'assegura que els nivells dels factors coincideixin
  levels <- union(levels(true_labels), levels(pred_labels))
  true_labels <- factor(true_labels, levels = levels)
  pred_labels <- factor(pred_labels, levels = levels)
  
  # S'avalua el model
  metrics <- evaluate_model(pred_labels, true_labels)
  
  # Es crea la matriu de confusió
  confusion_matrix <- confusionMatrix(pred_labels, true_labels)
  
  # Es converteix la matriu de confusió a un dataframe
  cm_df <- as.data.frame(confusion_matrix$table)
  
  # Es grafica la matriu de confusió
  plot_confusion <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) + 
    geom_tile() + 
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "blue") + 
    labs(title = "Matriu de confusión amb ANN", x = "Valors reals", y = "Prediccions") + 
    theme(
      axis.text.y = element_text(size = 24), 
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18)
    )
  
  # Es crea explainer 
  explainer <- DALEX::explain(
    model,
    data = as.matrix(train_features),
    y = train_labels,
    label = "ANN"
  )
  
  # Es calcula la importància de les característiques
  vi <- ingredients::feature_importance(explainer, loss_function = loss_root_mean_square)
  vi_df <- as.data.frame(vi)
  top_genes <- vi_df[order(-vi_df$dropout_loss), ][1:num_genes, ]
  
  # Es crea un gràfic de barres de la importància dels gens
  plot_importancia <- ggplot2::ggplot(top_genes, ggplot2::aes(x = reorder(variable, -dropout_loss), y = dropout_loss)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Importància dels gens més rellevants\namb ANN",
                  x = "Gens",
                  y = "Importància") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 20), 
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtra fdata per incloure només els gens més importants
  fdata <- gen_annotation[gen_annotation$ID %in% top_genes$variable, ]
  
  # S'ordena fdata perquè coincideixi amb l'ordre de top_genes
  fdata <- fdata[match(top_genes$variable, fdata$ID), ]
  
  return(list(
    model = model,
    history = history,
    metrics = metrics,
    plot_confusion = plot_confusion,
    plot_importancia = plot_importancia,
    top_genes = top_genes,
    fdata = fdata
  ))
}

ann_f <- model_ann_f(model_ann_f_keras, train_f, test_f, epochs = 200, batch_size = 64)
metrics_ann_f <- ann_f$metrics
plot_confusion_ann_f <- ann_f$plot_confusion
plot_importancia_ann_f <- ann_f$plot_importancia
fdata_ann_f <- ann_f$fdata

metrics_ann_f
plot_confusion_ann_f
plot_importancia_ann_f
fdata_ann_f


# S'ENTRENA MODEL ANN AMB PCA
data_pca <- preprocess_data(method = "pca", dim = 30, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data
result_pca <- data_pca$pca_result
data_pca <- data_pca$pca_data

set.seed(123)
model_ann_pca_keras <- keras_model_sequential() %>%
  layer_dense(units = 254, activation = 'relu', input_shape = c(30)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

model_ann_pca <- function(model, train_pca, test_pca, result_pca, gen_annotation = genAnnotation, num_genes = 25, epochs = 100, batch_size = 32) {
  # Funció per entrenar i avaluar un model ANN amb dades PCA
  
  # Paràmetres:
  # - model: Model de xarxa neuronal artificial (ANN) predefinit.
  # - train_pca: Dataframe amb les dades d'entrenament transformades per PCA, incloent les etiquetes.
  # - test_pca: Dataframe amb les dades de prova transformades per PCA, incloent les etiquetes.
  # - result_pca: Objecte amb els resultats de l'anàlisi PCA.
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - num_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - epochs: Nombre d'èpoques per entrenar el model (per defecte, 100).
  # - batch_size: Mida del lot per entrenar el model (per defecte, 32).
  
  # Llibreries necessàries:
  # - keras: Per construir i entrenar el model ANN.
  # - DALEX: Per explicar el model ANN.
  # - ingredients: Per calcular la importància de les característiques.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  set.seed(123)
  # Es compila el model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  
  # Se separen les característiques i les etiquetes
  train_features <- train_pca[, -ncol(train_pca)]
  train_labels <- as.numeric(train_pca[, ncol(train_pca)]) - 1
  test_features <- test_pca[, -ncol(test_pca)]
  test_labels <- as.numeric(test_pca[, ncol(test_pca)]) - 1
  
  # Es converteixen les etiquetes a format categòric
  train_labels_cat <- to_categorical(train_labels, num_classes = 3)
  test_labels_cat <- to_categorical(test_labels, num_classes = 3)
  
  # S'entrena el model
  history <- model %>% fit(
    as.matrix(train_features), train_labels_cat,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = 0.2
  )
  
  # S'avalua el model
  metrics <- model %>% evaluate(as.matrix(test_features), test_labels_cat)
  
  # Es realitza prediccions al conjunt de prova
  predictions <- model %>% predict(as.matrix(test_features)) %>% k_argmax()
  
  # Es converteixen les etiquetes i prediccions a factors
  true_labels <- as.factor(test_labels)
  pred_labels <- as.factor(as.array(predictions))
  
  # S'assegura que els nivells dels factors coincideixin
  levels <- union(levels(true_labels), levels(pred_labels))
  true_labels <- factor(true_labels, levels = levels)
  pred_labels <- factor(pred_labels, levels = levels)
  
  # S'avalua el model
  metrics <- evaluate_model(pred_labels, true_labels)
  
  # Es crea la matriu de confusió
  confusion_matrix <- confusionMatrix(pred_labels, true_labels)
  
  # Es converteix la matriu de confusió a un dataframe
  cm_df <- as.data.frame(confusion_matrix$table)
  
  # Es grafica la matriu de confusió
  plot_confusion <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) + 
    geom_tile() + 
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "blue") + 
    labs(title = "Matriu de confusió amb ANN", x = "Valors reals", y = "Prediccions") + 
    theme(
      axis.text.y = element_text(size = 24), 
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18)
    )
  
  # Es crea explainer
  explainer <- DALEX::explain(
    model,
    data = as.matrix(train_features),
    y = train_labels,
    label = "ANN"
  )
  
  # Es calcula la importància de les característiques
  vi <- ingredients::feature_importance(explainer, loss_function = loss_root_mean_square)
  vi_df <- as.data.frame(vi)
  top_components <- vi_df[order(-vi_df$dropout_loss), ][1:num_genes, ]
  
  # S'assegura que les variables a top_components existeixen a result_pca$rotation
  valid_components <- top_components$variable %in% colnames(result_pca$rotation)
  top_components <- top_components[valid_components, ]
  
  # Es mapejen les components principals als gens originals
  gene_importance <- apply(result_pca$rotation[, top_components$variable, drop = FALSE], 1, function(x) sum(abs(x)))
  gene_importance_df <- data.frame(Gene = rownames(result_pca$rotation), Importance = gene_importance)
  
  # Se seleccionen els gens més importants
  top_genes <- gene_importance_df[order(-gene_importance_df$Importance), ][1:num_genes, ]
  
  # Es crea un gràfic de barres de la importància dels gens
  plot_importancia <- ggplot2::ggplot(top_genes, ggplot2::aes(x = reorder(Gene, -Importance), y = Importance)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Importància dels gens més rellevants\namb ANN",
                  x = "Gens",
                  y = "Importància") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 20), 
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title.y = ggplot2::element_text(size = 26),
      axis.title.x = ggplot2::element_text(size = 26),
      plot.title = ggplot2::element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtra fdata per incloure només els gens més importants
  fdata <- gen_annotation[gen_annotation$ID %in% top_genes$Gene, ]
  
  # S'ordena fdata perquè coincideixi amb l'ordre de top_genes
  fdata <- fdata[match(top_genes$Gene, fdata$ID), ]
  
  list(
    model = model,
    history = history,
    metrics = metrics,
    plot_confusion = plot_confusion,
    top_genes = top_genes,
    plot_importancia = plot_importancia,
    fdata = fdata
  )
}

ann_pca <- model_ann_pca(model_ann_pca_keras, train_pca, test_pca, result_pca, epochs = 150, batch_size = 16)
metrics_ann_pca <- ann_pca$metrics
plot_confusion_ann_pca <- ann_pca$plot_confusion
plot_importancia_ann_pca <- ann_pca$plot_importancia
fdata_ann_pca <- ann_pca$fdata

metrics_ann_pca
plot_confusion_ann_pca
plot_importancia_ann_pca
fdata_ann_pca


# S'ENTRENA MODEL ANN AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 10000, dim = 30, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data
result_f_pca <- data_f_pca$pca_result
data_f_pca <- data_f_pca$pca_data

set.seed(123)
model_ann_f_pca_keras <- keras_model_sequential() %>%
  layer_dense(units = 254, activation = 'relu', input_shape = c(30)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

ann_f_pca <- model_ann_pca(model_ann_f_pca_keras, train_f_pca, test_f_pca, result_pca, epochs = 150, batch_size = 16)
metrics_ann_f_pca <- ann_f_pca$metrics
plot_confusion_ann_f_pca <- ann_f_pca$plot_confusion
plot_importancia_ann_f_pca <- ann_f_pca$plot_importancia
fdata_ann_f_pca <- ann_f_pca$fdata

metrics_ann_f_pca
plot_confusion_ann_f_pca
plot_importancia_ann_f_pca
fdata_ann_f_pca


# S'ENTRENA MODEL ANN AMB DADES BALANCEJADES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_smote <- process_train(train_f, test_f, method = "smote")
train_smote <- as.data.frame(train_smote)

ann_smote <- model_ann_f(model_ann_f_keras, train_smote, test_f, epochs = 200, batch_size = 64)
metrics_ann_smote <- ann_smote$metrics
plot_confusion_ann_smote <- ann_smote$plot_confusion
plot_importancia_ann_smote <- ann_smote$plot_importancia
fdata_ann_smote <- ann_smote$fdata

metrics_ann_smote
plot_confusion_ann_smote
plot_importancia_ann_smote
fdata_ann_smote


# S'ENTRENA MODEL ANN AMB DADES REDUÏDES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_reduce <- process_train(train_f, test_f, method = "undersample")
train_reduce <- as.data.frame(train_reduce)

ann_reduce <- model_ann_f(model_ann_f_keras, train_reduce, test_f, epochs = 200, batch_size = 64)
metrics_ann_reduce <- ann_reduce$metrics
plot_confusion_ann_reduce <- ann_reduce$plot_confusion
plot_importancia_ann_reduce <- ann_reduce$plot_importancia
fdata_ann_reduce <- ann_reduce$fdata

metrics_ann_reduce
plot_confusion_ann_reduce
plot_importancia_ann_reduce
fdata_ann_reduce


# S'ENTRENA MODEL ANN AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_smote_reduce <- process_train(train_f, test_f, method = "both")
train_smote_reduce <- as.data.frame(train_smote_reduce)

ann_smote_reduce <- model_ann_f(model_ann_f_keras, train_smote_reduce, test_f, epochs = 200, batch_size = 64)
metrics_ann_smote_reduce <- ann_smote_reduce$metrics
plot_confusion_ann_smote_reduce <- ann_smote_reduce$plot_confusion
plot_importancia_ann_smote_reduce <- ann_smote_reduce$plot_importancia
fdata_ann_smote_reduce <- ann_smote_reduce$fdata

metrics_ann_smote_reduce
plot_confusion_ann_smote_reduce
plot_importancia_ann_smote_reduce
fdata_ann_smote_reduce


# S'ENTRENA MODEL XGBOOST AMB F
# Es creen train i test
f_data <- preprocess_data(method = "f", genes = 50, index = index)
train_f <- f_data$train_data
test_f <- f_data$test_data

# Es preparen les dades d´entrenament i prova
train_data <- train_f[, -ncol(train_f)]
train_labels <- as.numeric(as.factor(train_f$labels)) - 1  
test_data <- test_f[, -ncol(test_f)]
test_labels <- as.numeric(as.factor(test_f$labels)) - 1

# Les dades es converteixen a l'estructura de xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_labels)
# Es defineixen els paràmetres de XGBoost
params_f <- list(
  objective = "multi:softmax",
  num_class = length(unique(train_labels)),
  eta = 0.01,
  max_depth = 15,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

model_xgb <- function(dtrain, dtest, params, gen_annotation = genAnnotation, result_pca = NULL, pca_components = NULL, num_genes = 25, nrounds = 5000, early_stopping_rounds = 10) {
  # Funció per entrenar i avaluar un model XGBoost
  
  # Paràmetres:
  # - dtrain: Objecte xgb.DMatrix amb les dades d'entrenament.
  # - dtest: Objecte xgb.DMatrix amb les dades de prova.
  # - params: Llista de paràmetres per entrenar el model XGBoost.
  # - gen_annotation: Dataframe amb les anotacions dels gens.
  # - result_pca: Objecte opcional amb els resultats de l'anàlisi PCA.
  # - pca_components: Nombre de components principals a utilitzar en el càlcul de la importància dels gens.
  # - num_genes: Nombre de gens més importants a seleccionar (per defecte, 25).
  # - nrounds: Nombre màxim de rondes per entrenar el model (per defecte, 5000).
  # - early_stopping_rounds: Nombre de rondes sense millora per aturar l'entrenament anticipadament (per defecte, 10).
  
  # Llibreries necessàries:
  # - xgboost: Per entrenar el model XGBoost.
  # - caret: Per crear la matriu de confusió.
  # - ggplot2: Per crear els gràfics de la matriu de confusió i la importància dels gens.
  
  # S'entrena el model
  set.seed(123)
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    verbose = 0
  )
  
  # Es fa la predicció amb el model entrenat
  predictions <- predict(model, newdata = dtest)
  
  # Es converteixen les prediccions i etiquetes a factors amb els mateixos nivells
  test_labels <- getinfo(dtest, "label")
  predictions <- factor(predictions, levels = unique(test_labels))
  test_labels <- factor(test_labels, levels = unique(test_labels))
  
  # S'avalua el model
  metrics <- evaluate_model(predictions, test_labels)
  
  # Es crea la matriu de confusió
  confusion_matrix <- confusionMatrix(predictions, test_labels)
  cm_df <- as.data.frame(confusion_matrix$table)
  
  # Es crea el gràfic de la matriu de confusió
  plot_confusion <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) + 
    geom_tile() + 
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "blue") + 
    labs(title = "Matriu de confusió amb XGBoost", 
         x = "Valors reals", y = "Prediccions") + 
    theme_minimal() + 
    theme( 
      axis.text.y = element_text(size = 24), 
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 18)
    )
  
  # S'obté la importància de les característiques
  importance <- xgb.importance(model = model)
  importance_df <- as.data.frame(importance)
  
  if (!is.null(result_pca) && !is.null(pca_components)) {
    # S'obtenen les càrregues dels components principals
    loadings <- result_pca$rotation[, 1:pca_components]
    
    # Se seleccionen els components principals més importants
    top_components <- importance_df[order(-importance_df$Gain), ][1:num_genes, ]
    valid_features <- top_components$Feature %in% colnames(loadings)
    top_components <- top_components[valid_features, ]
    
    # Es mapeja les components principals als gens originals
    gene_importance <- rowSums(abs(loadings[, top_components$Feature, drop = FALSE]))
    top_genes <- data.frame(
      variable = names(sort(gene_importance, decreasing = TRUE)[1:num_genes]),
      Gain = sort(gene_importance, decreasing = TRUE)[1:num_genes]
    )
  } else {
    # Se seleccionen els gens més importants
    top_genes <- importance_df[order(-importance_df$Gain), ][1:num_genes, ]
    top_genes <- data.frame(
      variable = top_genes$Feature,
      Gain = top_genes$Gain
    )
  }
  
  # Es crea un gràfic de barres de la importància dels gens
  plot_importancia <- ggplot(top_genes, aes(x = reorder(variable, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Importància dels gen més rellevants\namb XGBoost",
         x = "Gens",
         y = "Importància") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 20), 
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 26),
      axis.title.x = element_text(size = 26),
      plot.title = element_text(size = 30, hjust = 0.5)
    )
  
  # Es filtren aquells gens presents a gen_annotation
  filtered_genes <- top_genes[top_genes$variable %in% gen_annotation$ID, ]
  
  # Es filtren fdata per incloure només els gens més importants
  fdata <- gen_annotation[gen_annotation$ID %in% filtered_genes$variable, ]
  
  # S'ordena fdata perquè coincideixi amb l'ordre de filtered_genes
  fdata <- fdata[match(filtered_genes$variable, fdata$ID), ]
  
  return(list(
    model = model,
    metrics = metrics,
    plot_confusion = plot_confusion,
    plot_importancia = plot_importancia,
    top_genes = top_genes,
    fdata = fdata
  ))
}

result_xgb_f <- model_xgb(dtrain, dtest, params_fscore)
metrics_xgb_f <- result_xgb_f$metrics
plot_confusion_xgb_f <- result_xgb_f$plot_confusion
plot_importancia_xgb_f <- result_xgb_f$plot_importancia
fdata_xgb_f <- result_xgb_f$fdata

metrics_xgb_f
plot_confusion_xgb_f
plot_importancia_xgb_f
fdata_xgb_f


# S'ENTRENA MODEL XGBOOST AMB PCA
data_pca <- preprocess_data(method = "pca", dim = 150, index = index)
train_pca <- data_pca$train_data
test_pca <- data_pca$test_data
result_pca <- data_pca$pca_result
data_pca <- data_pca$pca_data

train_data <- train_pca[, -ncol(train_pca)]
train_labels <- as.numeric(as.factor(train_pca$labels)) - 1  
test_data <- test_pca[, -ncol(test_pca)]
test_labels <- as.numeric(as.factor(test_pca$labels)) - 1

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_labels)

params_pca <- list(
  objective = "multi:softmax",
  num_class = length(unique(train_labels)),
  eta = 0.001,
  max_depth = 5,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

xgb_pca <- model_xgb(dtrain, dtest, params_pca, result_pca= result_pca, pca_components = 150)
metrics_xgb_pca <- xgb_pca$metrics
plot_confusion_xgb_pca <- xgb_pca$plot_confusion
plot_importancia_xgb_pca <- xgb_pca$plot_importancia
fdata_xgb_pca <- xgb_pca$fdata

metrics_xgb_pca
plot_confusion_xgb_pca
plot_importancia_xgb_pca
fdata_xgb_pca


# S'ENTRENA MODEL XGBOOST AMB F I PCA
data_f_pca <- preprocess_data(method = "f_pca", genes = 500, dim = 50, index = index)
train_f_pca <- data_f_pca$train_data
test_f_pca <- data_f_pca$test_data
result_f_pca <- data_f_pca$pca_result
data_f_pca <- data_f_pca$pca_data

train_data <- train_f_pca[, -ncol(train_f_pca)]
train_labels <- as.numeric(as.factor(train_f_pca$labels)) - 1  
test_data <- test_f_pca[, -ncol(test_f_pca)]
test_labels <- as.numeric(as.factor(test_f_pca$labels)) - 1

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_labels)

params_f_pca <- list(
  objective = "multi:softmax",
  num_class = length(unique(train_labels)),
  eta = 0.001,
  max_depth = 10,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

xgb_f_pca <- model_xgb(dtrain, dtest, params_f_pca, result_pca= result_pca, pca_components = 50)
metrics_xgb_f_pca <- xgb_f_pca$metrics
plot_confusion_xgb_f_pca <- xgb_f_pca$plot_confusion
plot_importancia_xgb_f_pca <- xgb_f_pca$plot_importancia
fdata_xgb_f_pca <- xgb_f_pca$fdata

metrics_xgb_f_pca
plot_confusion_xgb_f_pca
plot_importancia_xgb_f_pca
fdata_xgb_f_pca


# S'ENTRENA MODEL XGBOOST AMB DADES BALANCEJADES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_smote <- process_train(train_f_pca, test_f_pca, method = "smote")

train_data <- train_smote[, -ncol(train_smote)]
train_labels <- as.numeric(as.factor(train_smote$labels)) - 1 

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)

xgb_smote <- model_xgb(dtrain, dtest, params_f_pca, result_pca= result_pca, pca_components = 50)
metrics_xgb_smote <- xgb_smote$metrics
plot_confusion_xgb_smote <- xgb_smote$plot_confusion
plot_importancia_xgb_smote <- xgb_smote$plot_importancia
fdata_xgb_smote <- xgb_smote$fdata

metrics_xgb_smote
plot_confusion_xgb_smote
plot_importancia_xgb_smote
fdata_xgb_smote


# S'ENTRENA MODEL XGBOOST AMB DADES REDUÏDES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_reduce <- process_train(train_f_pca, test_f_pca, method = "undersample")

train_data <- train_reduce[, -ncol(train_reduce)]
train_labels <- as.numeric(as.factor(train_reduce$labels)) - 1 

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)

xgb_reduce <- model_xgb(dtrain, dtest, params_f_pca, result_pca= result_pca, pca_components = 50)
metrics_xgb_reduce <- xgb_reduce$metrics
plot_confusion_xgb_reduce <- xgb_reduce$plot_confusion
plot_importancia_xgb_reduce <- xgb_reduce$plot_importancia
fdata_xgb_reduce <- xgb_reduce$fdata

metrics_xgb_reduce
plot_confusion_xgb_reduce
plot_importancia_xgb_reduce
fdata_xgb_reduce


# S'ENTRENA MODEL XGBOOST AMB DADES BALANCEJADES I REDUÏDES
# S'aplica a train_f i test_f ja que són els que millor accuracy mostraba
train_smote_reduce <- process_train(train_f_pca, test_f_pca, method = "both")
train_data <- train_smote_reduce[, -ncol(train_smote_reduce)]
train_labels <- as.numeric(as.factor(train_smote_reduce$labels)) - 1 

dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)

xgb_smote_reduce <- model_xgb(dtrain, dtest, params_f_pca, result_pca= result_pca, pca_components = 50)
metrics_xgb_smote_reduce <- xgb_smote_reduce$metrics
plot_confusion_xgb_smote_reduce <- xgb_smote_reduce$plot_confusion
plot_importancia_xgb_smote_reduce <- xgb_smote_reduce$plot_importancia
fdata_xgb_smote_reduce <- xgb_smote_reduce$fdata

metrics_xgb_smote_reduce
plot_confusion_xgb_smote_reduce
plot_importancia_xgb_smote_reduce
fdata_xgb_smote_reduce


# ES RECUPEREN ELS GENS QUE ESTAN PRESENTS A TOTS ELS MODELS
# Es realitza una combinació de tots els datframe fdata
fdata <- rbind(fdata_dt_f, fdata_dt_pca, fdata_dt_f_pca, fdata_dt_smote, fdata_dt_reduce, fdata_dt_smote_reduce, 
               fdata_svm_f, fdata_svm_pca, fdata_svm_f_pca, fdata_svm_smote, fdata_svm_reduce, fdata_svm_smote_reduce,
               fdata_rf_f, fdata_rf_pca, fdata_rf_f_pca, fdata_rf_smote, fdata_rf_reduce, fdata_rf_smote_reduce,
               fdata_ann_f, fdata_ann_pca, fdata_ann_f_pca, fdata_ann_smote, fdata_ann_reduce, fdata_ann_smote_reduce,
               fdata_xgb_f, fdata_xgb_pca, fdata_xgb_f_pca, fdata_xgb_smote, fdata_xgb_reduce, fdata_xgb_smote_reduce)

# S'analitza la freqüència dels gens
value_counts <- as.data.frame(table(fdata$ID))

# Es filtren els gens més freqüents
filtered_counts <- subset(value_counts, Freq > 5)

ggplot(filtered_counts, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Gens més freqüents segons els models",
       x = "Gens",
       y = "Freqüència") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 20), 
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    plot.title = element_text(size = 30, hjust = 0.5)
  )




