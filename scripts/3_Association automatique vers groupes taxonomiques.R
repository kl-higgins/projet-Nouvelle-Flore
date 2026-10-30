# Attribution automatisée (pour mises à jour) des taxons sélectionnés pour le projet Nouvelle Flore par groupe taxonomique
# ====================================================================================
# Sections :
#   1. Chargement et préparation des données VASCAN
#   2. Regroupement des taxons par groupe taxonomique
#   3. Intégration des statuts provinciaux EMVS
#   4. Exportation des taxons par regroupement_cle pour les rédacteurs
#
# ====================================================================================

library(tidyr)
library(dplyr)
library(data.table)
library(taxize)
library(stringr)

# ==============================================================================
# 1. Chargement et préparation des données VASCAN#####
# ==============================================================================

# Charger les données taxonomiques sélectionnées et leur distribution géographique.
taxons_fichiers <- list.files("output data",
                              pattern = "^taxon_sel_VASCANv37_17", full.names = TRUE)
taxons_fichiers_date <- as.Date(stringr::str_extract(taxons_fichiers, "\\d{4}-\\d{2}-\\d{2}"))
taxons_fichier_recent <- taxons_fichiers[which.max(taxons_fichiers_date)]
taxons_tous <- as.data.table(read.table(taxons_fichier_recent))

distribution_fichiers <- list.files("output data",
                                    pattern = "^distribution_taxons_regroupements", full.names = TRUE)
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
# géographique, de leur classification taxonomique, des regroupements pour le projet et
# des annotations de difficultés ou étapes du projet

# ==============================================================================
# 2. Calculs de nombre de taxons par groupe taxonomique#####
# ==============================================================================

# Compter les taxons par groupe, famille et ordre, en excluant les taxons
# du territoire potentiel du nord de l'Ontario.
gr_taxo <- taxons_prj[territoire != "potentiel ON", 
                      list(nb.taxa = length(taxonID)),
                      by = .(groupe_taxo, sous_groupe, family, order)]

calcul.nb.taxa.sous.groupe <- gr_taxo[, list(nb.taxa.sous.groupe = sum(nb.taxa)), by = .(sous_groupe)]

taxons_prj<-left_join(taxons_prj, gr_taxo)%>%left_join(calcul.nb.taxa.sous.groupe)

# ==============================================================================
# 3. Intégration des statuts provinciaux EMVS####
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


View(taxons_prj)

# ==============================================================================
# 4. Exportation de la sélection des taxons pour un rédacteur####
# ==============================================================================

redacteur_family = 'Osmundaceae'
redacteur_genus = 'Cicuta'

taxons_prj[family==redacteur_family&territoire!='omettre',]%>%
          select(taxonID, dist, source, territoire, regroupement_cle, family, genus,
                 scientificName,scientificNameSimple, `Statut provincial`)

taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, regroupement_cle, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)



