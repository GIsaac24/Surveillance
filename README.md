# Surveillance — Dashboard préparation MVE RCA

Ce dépôt contient l’application Shiny et l’export HTML du tableau de bord de préparation de la République Centrafricaine face à la menace d’importation de la Maladie à Virus Ebola.

## Contenu

- `CAR_Dashboard_Preparation_Menace_MVE/` : application Shiny et sources R/RMarkdown.
- `outputs/CAR_Dashboard_Preparation_Menace_MVE.html` : export HTML partageable.
- `outputs/templates_donnees_dashboard_MVE.xlsx` : template de données utilisé pour les PoE fluviaux et autres feuilles structurées.
- `data/` : fichiers de données utilisés par l’application, notamment `data_aeroport.xlsx`.
- `shapefile/` : couches géographiques utilisées pour les cartes.

## Lancer l’application

Depuis R :

```r
shiny::runApp("CAR_Dashboard_Preparation_Menace_MVE")
```

## Régénérer l’export HTML

Depuis le dossier `CAR_Dashboard_Preparation_Menace_MVE` :

```r
source("global.R", encoding = "UTF-8")
export_dashboard_html()
```

## Sources de données PoE

- Points d’entrée fluviaux : feuille `surveillance_poe` du fichier `outputs/templates_donnees_dashboard_MVE.xlsx`.
- Aéroport Bangui M’Poko : fichier `data/data_aeroport.xlsx`.

## Contact

Pour toute information, contactez :

- Dr Jean Méthode MOYEN, Coordonnateur du COUSP — Tél. : +23672248722 — Mail : jmethodemoyen@gmail.com
- Dr Daniel WEA YOUNGAÏ, Incident Manager — Tél. : +23672569182 — Mail : youngaiwea@gmail.com
- M. Isaac Simplice KENGUELA, Suivi-Évaluation — Tél. : +23672601806 — Mail : sikendba2016@gmail.com
