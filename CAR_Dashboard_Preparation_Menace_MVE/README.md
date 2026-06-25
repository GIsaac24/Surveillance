# CAR_Dashboard_Preparation_Menace_MVE

Tableau de bord applicatif pour le suivi de la préparation de la République Centrafricaine face à la menace d’importation de la Maladie à Virus Ebola, souche Bundibugyo.

## Lancer l’application Shiny

Depuis R ou RStudio :

```r
shiny::runApp("C:/Users/user/Documents/Préparation EBOLA/SitReps/Data surveillance/analyse_poe/analyse_poe/CAR_Dashboard_Preparation_Menace_MVE")
```

## Régénérer l’export HTML

Depuis le dossier `CAR_Dashboard_Preparation_Menace_MVE` :

```r
source("global.R", encoding = "UTF-8")
export_dashboard_html()
```

Le fichier est généré dans :

```text
../outputs/CAR_Dashboard_Preparation_Menace_MVE.html
```

## Données attendues

Le dashboard lit prioritairement les fichiers disponibles dans le dossier parent `../data`.

Fichiers déjà pris en charge :

- `data_poe.xlsx` pour la surveillance aux points d’entrée ;
- `data_rdc.xlsx` pour la courbe épidémique RDC ;
- les fichiers Excel de piliers déjà présents dans `data` ;
- `Contexte.docx`, `Contexte.txt`, `Contexte.xlsx` ou `Contexte.csv` ;
- les images institutionnelles dans `data/images` ;
- les shapefiles dans `../shapefile`, avec priorité à `DS et RS/CAR_ADM2_DS.shp`.

Le modèle Excel `templates_donnees_dashboard_MVE.xlsx` contient les feuilles suivantes :

1. `contexte`
2. `chronologie_activites`
3. `surveillance_poe`
4. `formations`
5. `matrice_piliers`
6. `activites_par_piliers`
7. `materiels_laboratoire`
8. `sites_laboratoires`
9. `districts_prioritaires`
10. `donnees_rdc`
11. `donnees_ouganda`
12. `parametres`

## Hypothèses importantes

- Les températures ≥ 38°C sont traitées comme signaux fébriles/alertes à vérifier, pas comme cas suspects MVE sans investigation clinique.
- Les données RDC/Ouganda de référence intégrées dans le modèle proviennent de la page OMS “Alert and response”, image publiée le 23/06/2026, données au 21/06/2026.
- Les fichiers de piliers existants contiennent surtout des lignes d’activités/budgets. Quand aucun statut ou taux d’avancement n’est fourni, le dashboard affiche un avancement à 0 % afin de ne pas inventer de progrès.
- Les cartes et graphiques interactifs utilisent désormais `leaflet` et `plotly`. Installation minimale recommandée :

```r
install.packages(c("shiny", "bslib", "htmltools", "readxl", "dplyr", "tidyr", "stringr", "lubridate", "ggplot2", "scales", "forcats", "reactable", "sf", "plotly", "leaflet"))
```
- L’application Shiny est la version interactive principale : onglets cliquables, panneaux de survol, cartes avec zoom au survol des districts prioritaires et affichage des sous-préfectures/sites laboratoire. L’export HTML reste une version partageable et consultable hors serveur.

## Structure

```text
CAR_Dashboard_Preparation_Menace_MVE/
├── app.R
├── global.R
├── ui.R
├── server.R
├── dashboard_static.Rmd
├── build_static_dashboard.R
├── R/
├── modules/
├── www/
├── outputs/
└── README.md
```

## Actualisation

Pour actualiser le dashboard :

1. mettre à jour les fichiers dans `data` ou remplir `templates_donnees_dashboard_MVE.xlsx` puis placer la version renseignée dans `data` ;
2. relancer l’application Shiny pour la version interactive ;
3. relancer `export_dashboard_html()` pour produire une version HTML partageable.

Par défaut, le classeur modèle livré dans `outputs` n’est pas utilisé comme source de données réelle, car il contient des exemples fictifs. Pour forcer sa lecture depuis `outputs`, définir avant lancement :

```r
Sys.setenv(MVE_USE_OUTPUT_TEMPLATE = "TRUE")
```
