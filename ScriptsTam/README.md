# ScriptsTam

## Analyses exploratoires

Scripts qui commencent avec eda-\*.R

## Modèles

2 scripts principaux pour modèle de base (approche naïve) et modèle à 2 étapes.

Pour entraîner les modèles, il faut exécuter les scripts correspondant : step-o, step-c ou step-r

À la première exécution des scripts principaux tous les modèles (lasso, random forest) sont entraînés une 1re fois.

Ils sont stockés dans le dossier *models*.

Pour entraîner un modèle de nouveau, il faut soit supprimer le modèle correspondant dans le dossier modèle, soit exécuter séparément le script step-o, step-c ou step-r correspondant.

### Formattage des données
format_data.R

### Modèle de base 
- Script principal : one_step.R
- Modèles : step_o_\*.R

### Modèle à 2 étapes
- Script principal : two_steps.R
- Modèles C : step_c_\*.R
- Modèles R : step_r_\*.R
- Stats pour étape C seulement : two_steps-c_only.R

### Importance des variables
var_imp_plots.R
