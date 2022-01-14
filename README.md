# Sujet2-Bilan_electrique_de_consommation

Objectif

On construit un shiny qui permet de visualiser le bilan électrique (consommation et production locale) à une maille locale à partir des données Enedis au pas annuelle
et de comparer plusieurs collectivités. Les mailles pourront être des régions, des départements ou des communes.
Les données ne seront pas récupérées par API mais pourront être téléchargées à l’adresse : https://data.enedis.fr/explore/?sort=modified (prendre les six jeux de données : 
conso (resp prod) commune, département et région, au pas annuel.

Les inputs sont : 
- maille d’intérêt : département, région ou commune
- année(s) d’intérêt : parmi celles disponibles dans les jeux de données 
- noms de la ou les mailles à regarder (au moins une , mais l’idée est de pouvoir en mettre deux ou trois pour comparer)

Les outputs sont, pour chaque collectivité :

Dans un premier onglet “Résumé” : 
- un graphique en barres, avec, pour une année moyenne de la période considérée,
une barre qui représente les consommations (empilées, une couleur par segment de clientèle) et une autres les productions (empilées, une couleur par type de production)
- deux valuebox qui représentent pour l’une la conso totale sur la période et pour l’autre la production totale sur la période
- un graphique interactif de courbes qui représente l’évolution des consos par segment et des productions par segment selon l’année.

Dans un second onglet “Détails” : 
- un datatable qui présente les consommations sur l’ensemble de la période choisie, avec une ligne par segment de clientèle , une colonne par collectivité, 
une colonne pour la collectivité “moyenne FRANCE” et l’écart en pourcentage à cette moyenne France 
- un datatable des productions avec une ligne par filière de production, une colonne par collectivité, une colonne pour la collectivité “moyenne FRANCE” et l’écart en pourcentage à cette moyenne France - en dessous de chacun de ces datatable, un bouton fileoutput qui permet de télécharger les données ci dessus.

Il faudra également rajouter la collectivité “moyenne france” qui sera la moyenne.
Par défaut, ce sont les chiffres de cette collectivité moyenne qui s’afficheront. 
Dans le cas de la maille département, il faudra proposer dans les choix les moyennes régionales (ex: si on entre le département de Paris, 
on doit avec dans les choix la moyenne Ile de France). De même, pour la maille commune, il faudra proposer dans les choix la moyenne régionale et la moyenne départementale.
