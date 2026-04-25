# Dynamics of Networks : Évaluation d'une politique de financement de la recherche

**Étude d’impact (2012) en deux volets : (1) analyse économétrique des performances individuelles et des externalités (spillovers), (2) modélisation de la formation de liens (dyades) avec focus sur le genre et le statut core/non-core.**

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/Python-3776AB?style=for-the-badge&logo=python&logoColor=white)
![Jupyter](https://img.shields.io/badge/Jupyter-F37626?style=for-the-badge&logo=jupyter&logoColor=white)
![fixest](https://img.shields.io/badge/fixest-444444?style=for-the-badge&logo=r&logoColor=white)
![data.table](https://img.shields.io/badge/data.table-444444?style=for-the-badge&logo=r&logoColor=white)
![NetworkX](https://img.shields.io/badge/NetworkX-333333?style=for-the-badge&logo=networkx&logoColor=white)
![Pandas](https://img.shields.io/badge/Pandas-150458?style=for-the-badge&logo=pandas&logoColor=white)
![Matplotlib](https://img.shields.io/badge/Matplotlib-11557C?style=for-the-badge&logo=matplotlib&logoColor=white)
![Word](https://img.shields.io/badge/Word-2B579A?style=for-the-badge&logo=microsoft-word&logoColor=white)
![PowerPoint](https://img.shields.io/badge/PowerPoint-B7472A?style=for-the-badge&logo=microsoft-powerpoint&logoColor=white)

> Travail réalisé sous **R** et **Python** : **fixest** et **data.table** pour l'économétrie de haute dimension (DiD), **NetworkX** et **igraph** pour les mesures structurelles des réseaux, **Matplotlib/Seaborn** pour les visualisations d'études d'événements, **Word** pour le rapport final et **PowerPoint** pour la soutenance.

<br>

## 🛠️ Compétences mobilisées

- **Causal Inference** : Modèles Diff-in-Differences (DiD), Event Studies, validation des tendances parallèles et gestion des effets fixes (individus, temps, dyades).
- **Network Science** : Analyse de la densité, clustering (local/global), centralité, composantes connexes et évolution de la force des liens (poids des collaborations).
- **Spillover Analysis** : Mesure de l'impact indirect sur les nœuds non-traités exposés au réseau financé (mesure d'exposition pré-traitement).
- **Hétérogénéité & Homophilie** : Analyse par attributs (Genre M/F, statut Core/Non-Core) et impact sur la formation de liens intra vs extra-groupes.
- **Data Engineering** : Nettoyage de panels dyadiques massifs, pivotement ego/alter et construction d'indicateurs de réseaux dynamiques.

<br>

## 📌 Résultats clés

Après le lancement de la politique en 2012, le financement agit comme un **puissant catalyseur relationnel** : on observe une densification massive des collaborations **intra-groupes** (+0.54 partenaires internes en moyenne) au détriment d'une ouverture externe. La structure du réseau se recentre, créant un effet de **consolidation interne** où les membres "Core" captent l'essentiel de la connectivité additionnelle. 

L'analyse révèle des **spillovers négatifs significatifs** : les chercheurs non-traités, initialement connectés aux bénéficiaires, voient leur probabilité de collaboration chuter de 18,5%, signe d'un repli des acteurs financés sur leur propre noyau. Enfin, concernant l'inclusion, si la politique renforce les liens de manière neutre entre les genres à l'intérieur des groupes, elle accentue les positions dominantes pré-existantes, favorisant une structure de réseau plus hiérarchisée et moins fragmentée.
