
# Attribution manuelle des taxons sélectionnés pour le projet Nouvelle Flore par groupe taxonomique
# ====================================================================================
# Sections :
#   1. Chargement et préparation des données VASCAN
#   2. Regroupement des taxons par groupe taxonomique
#   3. Intégration des statuts provinciaux EMVS
#   4. Intégration des annotations et priorisation des groupes
#   5. Intégration des informations sur les volumes de la Flora of North America (FNA)
#   6. Exportation du tableau de sélection
#
# Notes pour le document R Markdown :
# - Présenter les règles de regroupement par grand groupe, sous-groupe et famille.
# - Documenter les réassignations manuelles et les groupes encore à déterminer.
# - Distinguer le nombre de taxons retenus pour le projet des taxons à statut EMVS du Québec.
# - Expliquer l'intégration des annotations du groupe de travail, des rédacteurs
#   et du comité, ainsi que le traitement des genres dont le traitement FNA est incomplet.
# ====================================================================================

library(tidyr)
library(dplyr)
library(data.table)
library(taxize)
library(stringr)

# ==============================================================================
# 1. Chargement et préparation des données VASCAN
# ==============================================================================

# Charger les données taxonomiques sélectionnées et leur distribution géographique.
taxons_fichiers <- list.files("output data",
                       pattern = "^taxon_sel_VASCANv37_17", full.names = TRUE)
taxons_fichiers_date <- as.Date(stringr::str_extract(taxons_fichiers, "\\d{4}-\\d{2}-\\d{2}"))
taxons_fichier_recent <- taxons_fichiers[which.max(taxons_fichiers_date)]
taxons_tous <- as.data.table(read.table(taxons_fichier_recent))

distribution_fichiers <- list.files("output data",
                              pattern = "^distribution_taxons_sel", full.names = TRUE)
distribution_fichiers_date <- as.Date(stringr::str_extract(distribution_fichiers, "\\d{4}-\\d{2}-\\d{2}"))
distribution_fichier_recent <- distribution_fichiers[which.max(distribution_fichiers_date)]
distribution_sel <- as.data.table(read.table(distribution_fichier_recent))


# Associer les données taxonomiques à la distribution en excluant les taxons "omettre"
# dont le territoire est classé "omettre".
taxons_prj <- left_join(
  distribution_sel[territoire != "omettre", ],
  taxons_tous |> 
    select(
      #-bibliographicCitation, -license, -references,-nameAccordingToID, -nameAccordingTo,
      -taxonomicStatus
      )
)

# Objet produit : taxons_prj
# Table des taxons sélectionnés pour le projet, enrichie de leur distribution
# géographique et de leur classification taxonomique.


# ==============================================================================
# 2. Regroupement des taxons par groupe taxonomique
# ==============================================================================

# 2.1. Définir les groupes taxonomiques de base.
# Pour Carex, utiliser les sections taxonomiques indiquées dans higherClassification.
sections.carex <- taxons_prj |>
  filter(genus == "Carex") |>
  mutate(section = str_extract(higherClassification, "Carex sect\\. [^;]+"))

taxons_prj <- left_join(taxons_prj, sections.carex |> select(taxonID, section)) |>
  mutate(groupe_taxo = if_else(!is.na(section), section, genus))

head(taxons_prj)

# Compter les taxons par groupe, famille et ordre, en excluant les taxons
# du territoire potentiel du nord de l'Ontario.
gr_taxo <- taxons_prj[territoire != "potentiel ON", 
                      list(nb.taxa = length(taxonID)),
                      by = .(groupe_taxo, family, order)]

head(gr_taxo)

gr_taxo[groupe_taxo%like%"Carex sect. O",]

# 2.2. Charger les grands regroupements taxonomiques.
groupes_full <- as.data.table(readxl::read_xlsx("input data/grands_regroupements_taxonomiques.xlsx", sheet = "groupes_taxo_v20260817"))
groupes_trim <- groupes_full |> select(Grand_groupe, Sous_groupe, `Familles incluses`, R_code)

# Les blocs commentés ci-dessous sont conservés comme pistes de calcul.
# Ils permettaient d'explorer le nombre de genres ou de taxons par regroupement.
# groupes$nombre_genres <- 1
# for (i in 1:nrow(groupes)) {
#   group_vec <- strsplit(groupes_trim$R_code[i], ",")[[1]] |> trimws()
#   groupes[i, ]$nombre_taxons <- nrow(taxons_prj[family %in% group_vec, ])
# }

# Décomposer les listes de familles pour obtenir une ligne par famille.
groupes.par.familles <- groupes_trim |>
  separate_rows(R_code, sep = ",\\s*") |>
  dplyr::rename(family = R_code) |>
  as.data.table() |>
  select(-`Familles incluses`)

# Vérification éventuelle des familles affectées à plusieurs regroupements.
# groupes.par.familles[family %in% groupes.par.familles[duplicated(family), ]$family, ] |> arrange(family)

# 2.3. Réassigner manuellement certaines familles à un seul groupe.
# Ces décisions visent à harmoniser les regroupements avec l'organisation
# retenue pour le projet. Elles devront être documentées dans le rapport.

groupes.par.familles[family == "Apiaceae", Sous_groupe := "Apiaceae sensu lato"]
groupes.par.familles[family == "Apiaceae", Grand_groupe := "Dicotylédones terrestres"]

groupes.par.familles[family == "Ranunculaceae", Sous_groupe := "Ranunculaceae"]
groupes.par.familles[family == "Ranunculaceae", Grand_groupe := "Dicotylédones terrestres"]

groupes.par.familles[family == "Rosaceae", Sous_groupe := "Rosaceae herbacées"]  # Certains genres seront réassignés manuellement.
groupes.par.familles[family == "Rosaceae", Grand_groupe := "Dicotylédones terrestres"]

groupes.par.familles[family == "Polygonaceae", Sous_groupe := "Polygonaceae"]
groupes.par.familles[family == "Polygonaceae", Grand_groupe := "Dicotylédones terrestres"]

groupes.par.familles[family == "Plantaginaceae", Sous_groupe := "Autres dicotylédones herbacées terrestres"]
groupes.par.familles[family == "Plantaginaceae", Grand_groupe := "Dicotylédones terrestres"]

groupes.par.familles[family == "Grossulariaceae", Sous_groupe := "Arbres et arbustes feuillus"]  # Famille comprenant des formes herbacées et ligneuses.
groupes.par.familles[family == "Grossulariaceae", Grand_groupe := "Angiospermes ligneuses"]

groupes.par.familles[family == "Caprifoliaceae", Sous_groupe := "Arbres et arbustes feuillus"]  # Famille majoritairement ligneuse.
groupes.par.familles[family == "Caprifoliaceae", Grand_groupe := "Angiospermes ligneuses"]

groupes.par.familles <- groupes.par.familles |> distinct()
groupes.par.familles

# Associer les regroupements aux effectifs taxonomiques.
gr_taxo <- left_join(gr_taxo, groupes.par.familles)

# 2.4. Attribuer provisoirement certains groupes selon leur ordre taxonomique.
# Ces règles constituent des affectations par défaut et ne remplacent pas
# une validation des familles et genres concernés.
gr_taxo[order == "Caryophyllales" & is.na(Sous_groupe), Sous_groupe := "Autres dicotylédones herbacées terrestres"]
gr_taxo[order == "Ericales" & is.na(Sous_groupe), Sous_groupe := "Autres dicotylédones herbacées terrestres"]
gr_taxo[order == "Malpighiales" & is.na(Sous_groupe), Sous_groupe := "Autres dicotylédones herbacées terrestres"]

gr_taxo[is.na(Sous_groupe), Sous_groupe := "Autres dicotylédones herbacées terrestres probables - à déterminer"]
gr_taxo[is.na(Grand_groupe), Grand_groupe := "Dicotylédones terrestres"]

# 2.5. Réassigner certains genres de Rosaceae aux groupes ligneux.
gr_taxo[groupe_taxo == "Crataegus", Sous_groupe := "Arbres et arbustes feuillus"]
gr_taxo[groupe_taxo == "Crataegus", Grand_groupe := "Angiospermes ligneuses"]

gr_taxo[groupe_taxo == "Amelanchier", Sous_groupe := "Arbres et arbustes feuillus"]

gr_taxo[
  groupe_taxo %in% c("Amelanchier", "Aronia", "Malus", "Physocarpus", "Prunus", "Rosa", "Rubus", "Sorbus", "Spiraea"),
  Grand_groupe := "Angiospermes ligneuses"
]

# Vérifier l'affectation de Polygonaceae et calculer les effectifs par sous-groupe.
gr_taxo[Sous_groupe == "Polygonaceae", ]

calcul.nb.taxa.sous.groupe <- gr_taxo[, list(nb.taxa.sous.groupe = sum(nb.taxa)), by = .(Sous_groupe)]

groupes_trim[Sous_groupe == "Polygonaceae", ]

groupes <- left_join(groupes_trim, calcul.nb.taxa.sous.groupe)
groupes[!is.na(nb.taxa.sous.groupe), ]


# ==============================================================================
# 3. Intégration des statuts provinciaux EMVS
# ==============================================================================

# 3.1. Charger la liste des éléments de la flore signalés au Québec.
emvs <- as.data.table(readxl::read_xlsx("input data/FO_Signalement_plantes_EMVS_20260626.xlsm", sheet = "Éléments")) |>
  filter(Groupe == "Embryophytes")

# Extraire le nom scientifique et retirer les annotations de mise en forme.
emvs[, scientificNameSimple := trimws(sub(",.*", "", `Nom scientifique`))]
emvs[, scientificNameSimple := gsub("-p11", "", scientificNameSimple, fixed = TRUE)]
emvs[, scientificNameSimple := gsub("-p01", "", scientificNameSimple, fixed = TRUE)]
emvs[, scientificNameSimple := gsub("-p07", "", scientificNameSimple, fixed = TRUE)]
emvs[, scientificNameSimple := gsub("-p03", "", scientificNameSimple, fixed = TRUE)]
emvs[, scientificNameSimple := gsub("-p09", "", scientificNameSimple, fixed = TRUE)]
emvs[, scientificNameSimple := trimws(scientificNameSimple)]

# Une autre méthode d'extraction des noms avait été envisagée.
# emvs$scientificNameSimple <- split_string(emvs$`Nom scientifique`)

# 3.2. Ajouter les statuts EMVS aux taxons du projet.
# La liste EMVS peut comprendre des taxons absents de l'extraction VASCAN,
# notamment certains taxons disparus qui n'étaient pas inclus dans le téléchargement.
taxons_prj <- left_join(
  taxons_prj,
  emvs |> select(-Groupe, -`Nom scientifique`, -`Nom commun`) |> filter(!duplicated(scientificNameSimple))
)

# Examiner les taxons auxquels un statut provincial a été associé.
taxons_prj[!is.na(`Statut provincial`), ]

# Identifier les familles représentées dans les données EMVS associées.
familles_EMVS <- unique(taxons_prj[scientificNameSimple %in% emvs$`Nom scientifique`, ]$family)

taxons_prj[family %in% familles_EMVS, list(nb = length(taxonID)), by = .(family)]

# Repérer les taxons EMVS qui ne figurent pas dans la liste taxonomique du projet.
emvs[!`Nom scientifique` %in% taxons_prj$scientificNameSimple, ]

# Résultat initial : 28 taxons EMVS ne correspondent pas directement aux noms
# de taxons du projet. Ce nombre doit être recalculé et les cas examinés.

# 3.3. Calculer le nombre de taxons EMVS par grand regroupement.
groupes$nombre_emvs <- 1

for (i in 1:nrow(groupes)) {
  group_vec <- strsplit(groupes$R_code[i], ",")[[1]] |> trimws()
  nb_emvs <- nrow(taxons_prj[family %in% group_vec & scientificNameSimple %in% emvs$`Nom scientifique`, ])
  groupes[i, ]$nombre_emvs <- nb_emvs
}

groupes[1:4, ]


# ==============================================================================
# 4. Intégration des annotations et priorisation des groupes
# ==============================================================================

# 4.1. Charger les annotations du groupe de travail du portail Google Drive incluant les assignations à chaque personne
annotations <- as.data.table(readxl::read_xlsx("input data/Nouvelle_Flore_groupes_taxo_selection_20261022.xlsx", sheet = "Sheet1"))

# Pour chaque genre, conserver en priorité la ligne ayant une valeur de Priorisation.
# Le tableau final associe les regroupements taxonomiques aux propositions,
# à l'intérêt des rédacteurs, à l'approbation du comité et à l'état d'avancement.
gr_taxo_final <- left_join(
  gr_taxo,
  annotations |>
    select(genus, groupe.taxonomique, Priorisation, Proposition_Bioinformatique:`état`) |>
    arrange(is.na(Priorisation)) |>
    filter(!duplicated(genus)),
  by = c("groupe_taxo" = "genus")
) |>
  rename(
    groupe_principal = Grand_groupe,
    sous_groupe = Sous_groupe,
    regroupement_cle = groupe.taxonomique,
    genre_section = groupe_taxo,
    Proposition_Responsable = Proposition_Bioinformatique,
    Interet_Redacteur = Proposition_Redacteur
  ) |>
  select(
    groupe_principal, sous_groupe, order, family, regroupement_cle, genre_section,
    nb.taxa, Priorisation, Proposition_Responsable, Interet_Redacteur,
    Approbation_Comite_Date, `état`
  ) |>
  mutate(mise_a_jour_date = as.Date(NA)) |>
  arrange(groupe_principal, sous_groupe, order, family, regroupement_cle, genre_section)

names(gr_taxo_final)
View(gr_taxo_final)

# Exportation intermédiaire possible du tableau de sélection.
# writexl::write_xlsx(
#   gr_taxo_final,
#   path = paste0("output data/Nouvelle_Flore_groupes_taxo_selection_", Sys.Date(), ".xlsx")
# )

#faire des annotations manuelles dans le fichier Excel

# ==============================================================================
# 5. Intégration des informations sur les volumes de la Flora of North America
# ==============================================================================

# 5.1. Charger le tableau annoté et la liste des traitements FNA incomplets.
annotations2 <- as.data.table(readxl::read_xlsx("output data/Nouvelle_Flore_groupes_taxo_selection_2026-10-22.xlsx", sheet = "Sheet1"))

FNA_attente <- as.data.table(read.csv("input data/FNA_volumes_incomplete_v20260817.csv")) |>
  mutate(FNA_statut = "FNA_incomplet")

# 5.2. Associer les informations FNA aux groupes taxonomiques.
annotations_FNA_en_production <- left_join(
  annotations2,
  FNA_attente,
  by = c("genre_section" = "Family.Genus")
)

# Pour les groupes dont le traitement FNA est incomplet, remplacer certaines
# priorisations par un statut d'attente. Les cas complexes conservent cette précision.
annotations_FNA_en_production[
  FNA_statut == "FNA_incomplet" & (is.na(Priorisation) | Priorisation == "phase 1"),
  Priorisation := "attente FNA"
]

annotations_FNA_en_production[
  FNA_statut == "FNA_incomplet" & Priorisation == "Complexe",
  Priorisation := "attente FNA + Complexe"
]

annotations_FNA_en_production[
  FNA_statut == "FNA_incomplet" & Priorisation == "Complexe: révision de spécimens nécessaire",
  Priorisation := "attente FNA + Complexe: révision de spécimens nécessaire"
]

# Retirer les colonnes techniques issues du fichier FNA.
annotations_FNA_en_production <- annotations_FNA_en_production |>
  select(-Author.Email, -Status, -X, -FNA_statut)


# ==============================================================================
# 6. Exportation du tableau de sélection
# ==============================================================================

# Exporter le tableau final enrichi avec les informations sur les traitements FNA.
writexl::write_xlsx(
  annotations_FNA_en_production,
  path = paste0("output data/Nouvelle_Flore_groupes_taxo_selection_ajoutFNA_", Sys.Date(), ".xlsx")
)

# Objet exporté : annotations_FNA_en_production
# Tableau de sélection des groupes taxonomiques enrichi des informations de
# priorisation et de l'état des traitements FNA.

# ==============================================================================
# 7. Exportation des valeurs manuellement assignées vers un grand tableau
#    incluant la distribution territoriale, les groupements taxonomiques et la priorisation
# ==============================================================================

distribution_sel_annotations<-left_join(distribution_sel, taxons_prj%>%select(groupe_taxo, taxonID))%>%
          left_join(annotations_FNA_en_production%>%
                      select(genre_section, groupe_principal, sous_groupe, regroupement_cle, Priorisation)%>%
                      rename("groupe_taxo"="genre_section"))
write.table(distribution_sel_annotations,
            paste0("output data/distribution_taxons_regroupements", Sys.Date(), ".txt"))


# Points à documenter :
# - Les familles et genres réassignés manuellement.
# - Les groupes dont l'affectation reste provisoire.
# - Les écarts entre les taxons de VASCAN et ceux de la liste EMVS.
# - Les règles de priorisation appliquées aux traitements FNA incomplets.
# ====================================================================================
