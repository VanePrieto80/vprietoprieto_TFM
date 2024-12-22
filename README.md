# **README - Cerca de gens involucrats en el grau histològic de tumors de mama**


## Autora

- Vanesa Prieto Prieto (vprietop@uoc.edu)


# Descripció del projecte

Aquest projecte correspon al treball de final del màster universitari en ciències de dades.


## Resum del treball

El càncer de mama és el tipus de càncer més freqüent entre les dones a nivell mundial i també és una de les principals causes de mort d’aquesta malaltia. Les diverses formes en que aquesta malaltia s’exposa ha portat als últims temps a realitzar una atenció clínica individualitzada, gràcies en gran part als avanços biològics i tecnològics.

La tecnologia de microarrays ha esdevingut una eina essencial en el descobriment de la informació genètica, permetent identificar perfils genètics específics associats amb diferents subtipus de càncer i el desenvolupament de teràpies dirigides.

A aquest projecte es proposa l’ús de tècniques d’aprenentatge automàtic per a obtenir un conjunt de possibles gens responsables dels diferents graus histològics del càncer de mama per mitjà de l'aplicació dels mètodes supervisats de classificació DT, SVM, RF, ANN i XGBoost.

A més, s’avalua la millor manera de reduir el nombre de dades a avaluar mitjançant filtres o reducció de la dimensionalitat o ambdues tècniques. Així com a avaluar la necessitat o no de realitzar alguna tècnica per balancejar les classes que es troben desequilibrades.

Els millors resultats s’aconsegueixen amb l’algoritme SVM, utilitzant un filtre basat amb estadístiques F i sense utilitzar cap tipus de mètode per equilibra la variable etiqueta d’estudi.


## Estructura del Repositori

El repositori està organitzat en les següents carpetes i arxius:

- `/dataset/`: Aquesta carpeta conté el conjunt de dades per realitzar la visualització d'un microarray.

- `/source/`: Aquesta carpeta conté els arxius de codi font.

- `/memòria.pdf`: Aquest arxiu conté la memòria del treball de final de màster.

- `/Requeriments.r`: Aquest arxiu conté les biblioteques necessàries.
  
- `/README.md`: El present arxiu.



## Requisits i configuració

Instal·lar les biblioteques necessàries mitjançant el codi r.

Utilització de Kaggle per evitar descarregar les dades de GEO i disminuir el temps d'execució.



## Llicència

Aquesta obra està subjecta a una llicència de Reconeixement-NoComercial-SenseObraDerivada 3.0 Espanya de Creative Commons.

