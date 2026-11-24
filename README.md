# Fichiers principaux à utiliser pour les rédacteurs

Le fichier avec la sélection par rédacteur des groupes taxonomiques. Essentiel à consulter avant de proposer une genre ou section de Carex à traiter. Regarder la version la plus à jour de _Nouvelle_Flore_groupes_taxo_selection_redacteurs__
- [Sélection des groupes taxonomiques par rédacteur](https://github.com/kl-higgins/projet-Nouvelle-Flore/tree/master/output%20data)

La présentation des normes et standards à suivre pour le projet.
- [Normes et standards du projet](https://github.com/kl-higgins/projet-Nouvelle-Flore/tree/master/documents%20et%20rapports/presentationspptx)

Le formulaire (gabarit) à utiliser pour l'entrée de données: prendre la version la plus à jour (v017+) : _Formulaire_FVQ_v_
- [Formulaire de saisie des données](https://github.com/kl-higgins/projet-Nouvelle-Flore/tree/master/documents%20et%20rapports)

# Guide des scripts — Nouvelle Flore du Québec

Ce dossier rassemble les scripts de préparation des données, d’attribution des groupes taxonomiques et de compilation des traitements. Les scripts utilisent des chemins relatifs : ouvrir `ProjetNouvelleFlore.Rproj` ou définir le répertoire de travail à la racine du projet.

Ces scripts forment des étapes du projet et ne sont pas conçus pour être exécutés systématiquement de 1 à 4 à chaque analyse. Ils conservent des décisions et modifications manuelles propres à différentes phases. Plusieurs scripts utilisent automatiquement le fichier daté le plus récent; vérifiez les entrées sélectionnées avant de lancer une étape.

## Scripts

### 1. `1_Couverture_geographique_projet_Evaluation.R`

Prépare la couverture géographique des taxons à partir de VASCAN, puis intègre les listes du nord de l’Ontario : EMVS du Natural Heritage Information Centre (NHIC) et liste de S. Meades. Le script nettoie et apparie les noms, classe les taxons et produit des synthèses. Il peut être relancé avec une nouvelle version de VASCAN.

-   **Entrées :** données VASCAN et listes de référence dans `input data/`.
-   **Sorties :** fichiers datés `taxon_sel_VASCANv37_17_*.txt` et `distribution_taxons_sel_*.txt` dans `output data/`.
-   **Exécution :** locale seulement. Les données de S. Meades ne sont pas autorisées au partage.

### 2. `2_Association manuelle vers groupes taxonomniques_etapes_preparatoire.R`

Prépare les regroupements taxonomiques, intègre les statuts EMVS du Québec, les annotations d’équipe et les informations sur l’état de production des volumes FNA. Produit des tableaux utilisés pour l’attribution des groupes aux rédacteurs. Ce script est conservé comme historique : ne pas le modifier. Les changements aux grands regroupements sont prévus dans des scripts ultérieurs.

-   **Entrées :** fichiers VASCAN dans `output data/`; classeurs et listes de regroupements, EMVS, annotations et état de production FNA dans `input data/`.
-   **Sorties :** notamment `distribution_taxons_regroupements<date>.txt` et fichiers d’attribution dans `output data/`.
-   **Exécution :** locale seulement. Aucune mise à jour prévue : historique des changements.

### 2b. `2b_Attribution des groupes taxonomiques aux redacteurs_phaseNOV2026.R`

Version de préparation des attributions pour la retraite de rédaction de novembre 2026. Elle calcule les effectifs par groupe, ajuste les assignations aux rédacteurs, regroupe certains genres pour les clés et produit un classeur de sélection. C’est une version historique : ne pas la modifier ni la relancer; se fier aux sorties existantes et utiliser 2c pour les mises à jour.

-   **Entrées :** fichiers taxonomiques et d’attribution dans `output data/`; classeur de retraite dans `input data/retraites redaction/`.
-   **Sorties :** fichiers d’attribution et classeur dans `output data/`, tableau d’analyse dans `output data archivage/`.
-   **Exécution :** à distance possible MAIS aucune mise à jour prévue, historique seulement.

### 2c. `2c_Attribution des groupes taxonomiques aux redacteurs_phaseDEC2026 .R`

Version de travail pour la phase de décembre 2026. **C’est le script prévu pour être modifié** lors des mises à jour d’attribution. Les blocs « Ajouts - sélections par rédacteurs » et « Regroupement de genres pour les clés » enregistrent les propositions, approbations et décisions. Garder 2b comme référence historique de novembre.

-   **Entrées :** fichiers taxonomiques, de distribution et d’attribution les plus récents dans `output data/`.
-   **Sorties :** `attribution_regroupements_check_<date>.txt` et classeur daté `Nouvelle_Flore_groupes_taxo_selection_redacteurs_<date>.xlsx` dans `output data/`. Le script contient aussi des exemples de sélection par famille ou genre.
-   **Exécution :** à distance possible, mise à jour prévue.

### 3. `3_Association automatique vers groupes taxonomiques.R`

Associe automatiquement les taxons aux regroupements, calcule les effectifs, intègre les statuts EMVS et contient des exemples de sélection par famille ou genre.

-   **Entrées :** fichiers taxonomiques et d’attribution dans `output data/`; liste EMVS dans `input data/`.
-   **Exécution :** locale seulement, car le script dépend de données locales non partageables dans `input data/`.

### 4. `4_Construction_base_donnees_traitements_taxo.R`

Compile les traits des formulaires de traitements ODS, les compare aux vocabulaires contrôlés du formulaire maître et ajoute les métadonnées de version (rédacteurs, réviseurs, date et numéro de version). Le fichier `Structures_ajoutes_manuellement<date>.csv` sert à ajouter (manuellement) des lignes à la feuille « Structures » du formulaire.

-   **Entrées :** `documents et rapports/Formulaire_FVQ_v017.ods`, fichiers VASCAN/distribution/attribution de `output data/` et formulaires inscrits dans `fichiers.traitement`.
-   **Sorties :** `Structures_ajoutes_manuellement<date>.csv` et `Traits_compilation<date>.csv` dans `output data/`.
-   **Exécution :** à distance possible lorsque les formulaires ODS sont accessibles (par exemple depuis GitHub) et que les dépendances sont disponibles.

## Exécution et précautions

-   **À distance : seuls 2c et 4 sont prévus pour être exécutés à distance.** Les scripts 1, 2, 2b et 3 dépendent de fichiers locaux dans `input data/` qui ne peuvent pas être partagés. 2c et 4 nécessitent quand même leurs propres entrées, accessibles depuis `output data/`, `traitement data/` qui sont disponibles à partir du GitHub.
-   Installer les paquets R requis par le script avant l’exécution: `dplyr`, `tidyr`, `data.table`, `taxize` et `stringr`, `readxl`, `openxlsx` et `readODS`.
-   Vérifier les dates, chemins codés en dur et fichiers sélectionnés automatiquement. Pour 2c, apporter les changements d’attribution dans les blocs prévus. Pour 4, actualiser `fichiers.traitement` et ajouter les formulaires correspondant aux nouvelles séries de données ou versions du formulaire.
-   Examiner les contrôles de vocabulaire et les sorties avant de les réutiliser. `View()` nécessite une session R interactive.
