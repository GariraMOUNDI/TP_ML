Fichier `PimaIndiansDiabetes.csv`

Chargement des données:
```R
PimaIndiansDiabetes <- read.table("PimaIndiansDiabetes.csv", sep = ";", header=TRUE)
```

On a différentes mesures physiologiques et médicales chez des femmes Amérindiennes du peuple Pima :

* `pregnant`: Number of times pregnant
* `glucose`: Plasma glucose concentration (glucose tolerance test)
* `pressure`: Diastolic blood pressure (mm Hg)
* `triceps`: Triceps skin fold thickness (mm)
* `insulin`: 2-Hour serum insulin (mu U/ml)
* `mass`: Body mass index (weight in kg/(height in m)\^2)
* `pedigree`: Diabetes pedigree function
* `age`: Age (years)

Ces personnes appartiennent à deux groupes, les unes présentant du diabète (variable `diabetes` = 1), et les autres ne présentant pas de diabètes (variable `diabetes` = 0).

Question : Peut-on utiliser ces mesures pour modéliser voire prédire la présence/absence de diabètes chez ces femmes ?
