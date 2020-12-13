Fichier `superconductor.csv`

Chargement des données:
```R
superconductor <- read.table("superconductor.csv", sep = ";", header=TRUE)
```

On a différentes mesures et caractéristiques concernant des matériaux supraconducteurs, c'est-à-dire des matériaux offrant une résistance très faible au courant électrique (moins de résistance = moins de perte d'énergie), ainsi que leur température critique en degré Kelvin (variable `critical_temp`), c'est-à-dire la température (souvent très basse) à laquelle la propriété de supraconductivité est effective.

Question : Peut-on modéliser la température critique en fonction de ces différentes mesures ?
