
# Analyse des taxons de l'Est du Canada à partir de VASCAN et de la liste de S. Meades
# =================================================================================
# Sections :
#   1. Chargement et préparation des données VASCAN
#   2. Nettoyage et validation de la liste de S. Meades (nord de l'Ontario)
#   3. Synthèse de la distribution géographique par province selon VASCAN
#   4. Intégration des données EMVS du nord de l'Ontario
#   5. Intégration de la liste de S. Meades des espèces du nord de l'Ontario
#   6. Classification finale des taxons et exportation
#   7. Calcul des ajouts de taxons par genre par territoire
#
# Notes pour le document R Markdown :
# - Expliquer les critères de sélection du territoire et les sources de données.
# - Distinguer les taxons présents, absents, exclus et à distribution indéterminée.
# - Documenter les corrections taxonomiques manuelles et les cas non résolus.
# - Présenter les règles d'intégration des listes EMVS et de S. Meades.
# - Recalculer les effectifs après chaque mise à jour des sources (surtout VASCAN).
# =================================================================================

library(tidyr)
library(dplyr)
library(data.table)
library(taxize)
library(stringr)

# ==============================================================================
# 1. Chargement et préparation des données VASCAN
# ==============================================================================

# Les données VASCAN ont été téléchargées depuis l'IPT de Canadensys :
# https://data.canadensys.net/ipt/resource?r=vascan
# VERSION du 2026-08-84 utilisée

# 1.1. Charger la distribution et les données taxonomiques de tous les taxons du Canada.
vascan.taxon.dist.can <- as.data.table(read.delim("input data/VASCAN 2026-08-04 dwca-vascan-v37.17/distribution.txt"))
vascan.taxon.raw.can <- as.data.table(read.delim("input data/VASCAN 2026-08-04 dwca-vascan-v37.17/taxon.txt"))

# 1.2. Créer un nom scientifique simplifié, sans autorité taxonomique.
vascan.taxon.raw.can[, scientificNameSimple := paste(genus, specificEpithet)]
vascan.taxon.raw.can[taxonRank == "variety", scientificNameSimple := paste(genus, specificEpithet, "var.", infraspecificEpithet)]
vascan.taxon.raw.can[taxonRank == "subspecies", scientificNameSimple := paste(genus, specificEpithet, "subsp.", infraspecificEpithet)]

#manuellement créer pour Symphyotrichum lanceolatum subsp. lanceolatum
vascan.taxon.raw.can[higherClassification%like%'Symphyotrichum lanceolatum subsp. lanceolatum',
                     scientificNameSimple :=paste('Symphyotrichum lanceolatum subsp. lanceolatum', "var.", infraspecificEpithet)]

# 1.3. Associer à chaque taxon son nom accepté simplifié.
acc <- vascan.taxon.raw.can |>
  filter(taxonomicStatus == "accepted") |>
  select(acceptedNameUsageID, scientificNameSimple) |>
  dplyr::rename(accScientificName = scientificNameSimple)

vascan.taxon.raw.can <- left_join(vascan.taxon.raw.can, acc)

# 1.4. Restreindre la distribution à l'Est du Canada.
provinces_est <- c("Quebec", "Labrador", "New Brunswick", "Newfoundland", "Nova Scotia", "Ontario", "Prince Edward Island")

vascan.taxon.dist <- vascan.taxon.dist.can[locality %in% provinces_est] |>
  dplyr::rename(taxonID = id)

# Conserver les taxons associés aux distributions retenues - seulement les rangs espèces, variétés et sous-espèces
vascan.taxon.raw <- vascan.taxon.raw.can[acceptedNameUsageID %in% vascan.taxon.dist$taxonID]

names(vascan.taxon.raw)
names(vascan.taxon.dist)

# Objet produit : vascan.taxon.raw
# Taxons de l'Est du Canada selon l'extraction VASCAN, avec nom simplifié et
# nom accepté associé.

statuts_distribution <- data.table(
  statutSiteWeb = c("Indigène", "Introduit", "Éphémère", "Exclus", "Disparu", "Douteux", "Absent"),
  statutEstablishmentMeans=c("native", "introduced", "-", "-", "-", "-", "-"),
  statutOccurrenceStatus=c("present", "present", "irregular", "excluded", "absent", "doubtful", "aucune valeur (NA)"),
  definition = c(
    "Taxon présent uniquement en raison de processus naturels, sans intervention humaine.",
    "Taxon établi (naturalisé) dans une région autre que son aire d'origine, après y avoir été introduit délibérément ou accidentellement dans le cadre d'activités humaines. Le taxon est jugé introduit au Canada s'il s'y est établi après la colonisation européenne.",
    "Taxon non établi de façon permanente dans une région, mais présent de manière récurrente dans la nature, quasi annuellement, généralement à partir de cultures (blé, tomate, etc.).",
    "Taxon qui a déjà été signalé dans une région, mais ne s'y est pas établi ou avait été identifié de façon erronée.",
    "Taxon indigène à une région, mais actuellement considéré comme n'y étant plus présent. Le taxon n'est déclaré disparu qu'après des recherches actives, et cet état est généralement déterminé par un organisme de conservation.",
    "Taxon signalé dans une région par une source quelconque, sans que l'information ait été validée.",
    "Taxon inconnu dans la région."
  )
)
statuts_distribution
# ==============================================================================
# 2. Nettoyage et validation de la liste de S. Meades (nord de l'Ontario)
# ==============================================================================

# 2.1. Charger et fusionner les listes de monocotylédones et de dicotylédones.
# Les fichiers sont partagés sans entente de divulgation et doivent rester privés.
meades_monocots <- as.data.table(readxl::read_xls("input data/Northern Ontario Pteridophytes-Monocots_20260914.xls", sheet = "Sheet1"))
meades_dicots <- as.data.table(readxl::read_xls("input data/Northern Ontario Dicots_20260914.xls", sheet = "Sheet1"))

meades <- rbind(meades_monocots, meades_dicots, fill = TRUE)
meades <- meades[`Subspecies or Variety` != "subsp. / var." | is.na(`Subspecies or Variety`)]

# 2.2. Créer les noms selon les entrée de l'auteur (originalNameSimple) et les noms à corriger/nettoyer
meades[, originalNameSimple := paste(`Original Species Name`, `Subspecies or Variety`, sep = " ")]
meades[is.na(`Subspecies or Variety`), originalNameSimple := `Original Species Name`]
meades[, scientificNameSimple := originalNameSimple]
meades <- meades[!is.na(originalNameSimple)]#enlever les lignes superflues sans noms scientifiques

# Nettoyer quelques caractères de mise en forme dans la colonne scientificNameSimple
meades[grepl("`", originalNameSimple, fixed = TRUE), scientificNameSimple := gsub("`", " ", scientificNameSimple, fixed = TRUE)]
meades[grepl(",", originalNameSimple, fixed = TRUE), scientificNameSimple := gsub(",", ".", scientificNameSimple, fixed = TRUE)]

# 2.3. Attribuer une classe de distribution à partir des indications de la liste.
meades[, distribution := "nord Ontario selon S. Meades"]
meades[Range_classes == "absent", distribution := "absent"]

# Les points d'interrogation signalent une localisation incertaine.
meades[grepl("\\?\\?", originalNameSimple), distribution := "uncertain"]
meades[grepl("\\?\\?", originalNameSimple), scientificNameSimple := gsub("\\?\\?", " ", scientificNameSimple)]
meades[grepl("\\?", originalNameSimple), distribution := "uncertain"]
meades[grepl("\\?", originalNameSimple), scientificNameSimple := gsub("\\?", " ", scientificNameSimple)]

meades[, scientificNameSimple := trimws(scientificNameSimple)]
meades[grepl("no dot|no report", `Notes on Range`), distribution := "absent"]

# 2.4. Corrections taxonomiques manuelles.
# La valeur de gauche correspond au nom erroné, valeur de droite correspond au nom utilisé propre, 
corrections <- c(
  "Juncus alpinoarticulatus var. americanus" = "Juncus alpinoarticulatus subsp. americanus",
  "Spergularia media subsp. media" = "Spergularia media var. media",
  "Medicago sativa var. sativa" = "Medicago sativa subsp. sativa",
  "Caerx oligosperma" = "Carex oligosperma",
  "Crateagus flabellata" = "Crataegus flabellata",
  "Ranunculus sceleratus var. scelaratus" = "Ranunculus sceleratus var. sceleratus",
  "Silene flos-cuculi subsp. cuculi" = "Silene flos-cuculi subsp. flos-cuculi",
  "Huperzia haleakalae" = "Huperzia continentalis",  # Espèce exclue par VASCAN.
  "Botrychium lunaria" = "Botrychium neolunaria",  # Espèce exclue par VASCAN.
  "Nuphar rubrodisca" = "Nuphar ×rubrodisca",  # Espacement différent.
  "Populus ×rouleauiana" = "Populus ×rouleauiana",  # Espacement différent.
  "Salix ×fragilis                                    (alba × euxina)" = "Salix ×fragilis",  # Espacement différent.
  "Arnica angustifolia var, angustifolia" = "Arnica angustifolia subsp. angustifolia",
  "Epilobium ciliatum subsp. ciliatum" = "Epilobium ciliatum subsp. ciliatum var. ciliatum"  # VASCAN reconnaît deux variétés de la sous-espèce.
)

meades[originalNameSimple %in% names(corrections), 
       scientificNameSimple := unname(corrections[originalNameSimple])]

# 2.5. Préparer la table et effectuer une première jointure avec VASCAN.
meades.on <- meades |>
  select(scientificNameSimple, originalNameSimple, `Current Name (if different)`,
         `Notes on Range`, distribution)

meades.on.vascan1 <- left_join(
  meades.on,
  vascan.taxon.raw |> select(taxonID, scientificNameAuthorship, scientificName,
                             scientificNameSimple, accScientificName, taxonRank,
                             family, infraspecificEpithet),
  by = "scientificNameSimple"
)

# Nombre de lignes sans correspondance VASCAN après la première jointure.
nrow(meades.on.vascan1[is.na(scientificName), ])

# 2.6. Vérifier les noms non appariés à l'aide de gna_verifier().
noms_a_verifier <- meades.on.vascan1 |>
  filter(is.na(scientificName)) |>
  distinct(scientificNameSimple) |>
  pull(scientificNameSimple)

# Séparer les noms avec sous-espèce des autres noms pour la validation.
res1 <- gna_verifier(noms_a_verifier[grepl("subsp", noms_a_verifier, fixed = TRUE)], data_sources = c(1, 11))
res2 <- gna_verifier(noms_a_verifier[!grepl("subsp", noms_a_verifier, fixed = TRUE)], data_sources = c(1, 11))

res <- rbind(res1, res2)
res$matchedCanonicalFull <- gsub("× ", "×", res$matchedCanonicalFull, fixed = TRUE)

matching <- res |>
  select(submittedName, matchedCanonicalFull) |>
  dplyr::rename(scientificNameSimple = matchedCanonicalFull, originalNameSimple = submittedName) |>
  as.data.table()

# Réunir les noms normalisés par GNA et les informations de la liste originale.
meades.on2 <- rbind(
  meades.on[!originalNameSimple %in% noms_a_verifier, ],
  matching[, .(scientificNameSimple, originalNameSimple)] |>
    left_join(meades.on[originalNameSimple %in% noms_a_verifier, ] |> 
    select(originalNameSimple:distribution), by = "originalNameSimple"),
  fill = TRUE
)

#aucun duplicat avant de résoudre avec les noms de GNA verifier
meades.on%>%filter(duplicated(scientificNameSimple))%>%pull(scientificNameSimple)

#une deuxième ligne avec Leontodon saxatilis est ajouté, à supprimer
meades.on2%>%filter(scientificNameSimple%in%(meades.on2%>%filter(duplicated(scientificNameSimple))%>%pull(scientificNameSimple)))
meades.on2<-meades.on2[!(scientificNameSimple=='Leontodon saxatilis'&is.na(distribution)),]

# 2.7. Effectuer une deuxième jointure avec VASCAN après validation des noms dans GNA verifier
mead.vascan <- left_join(
  meades.on2 |> filter(!is.na(distribution)),
  vascan.taxon.raw |> select(taxonID, scientificNameAuthorship, scientificName, scientificNameSimple, accScientificName, taxonRank, family, infraspecificEpithet),
  by = "scientificNameSimple"
)

# Vérifier les noms qui demeurent sans correspondance.
nrow(mead.vascan[is.na(scientificName)])
mead.vascan[is.na(scientificName)]%>%select(scientificNameSimple, originalNameSimple, distribution)
meades.on3<-meades.on2 # nouvelle table pour recevoir la prochaine série de corrections

# Trois formes abrégées en "f." ne sont pas reconnues par VASCAN ; seul le nom de l'espèce est reconnu.
mead.unresolved.forms <- mead.vascan[is.na(scientificName) & scientificNameSimple %like% "f\\.", ] |>
  select(scientificNameSimple, originalNameSimple)
mead.unresolved.forms

meades.on3[scientificNameSimple %like% "f\\.", scientificNameSimple := apply(
  str_split(scientificNameSimple, " ", simplify = TRUE)[, 1:2, drop = FALSE],
  1, paste, collapse = " "
)]

# Pour les variétés et sous-espèces autonymes non reconnues, tenter une correspondance au niveau de l'espèce.
autonyme_var <- mead.vascan |> filter(is.na(scientificName) & scientificNameSimple %like% "var\\.") |> pull(scientificNameSimple)
meades.on3[scientificNameSimple %in% autonyme_var, scientificNameSimple := apply(
  str_split(scientificNameSimple, " ", simplify = TRUE)[, 1:2, drop = FALSE],
  1, paste, collapse = " "
)]

autonyme_subsp <- mead.vascan |> filter(is.na(scientificName) & scientificNameSimple %like% "subsp\\.") |> pull(scientificNameSimple)
meades.on3[scientificNameSimple %in% autonyme_subsp, scientificNameSimple := apply(
  str_split(scientificNameSimple, " ", simplify = TRUE)[, 1:2, drop = FALSE],
  1, paste, collapse = " "
)]

#c(autonyme_subsp, autonyme_var, mead.unresolved.forms$scientificNameSimple)

#à supprimer deuxième Epilobium ciliatum (erreur dans la base VASCAN - donc seulement pour cette version)
meades.on3%>%filter(scientificNameSimple%in%(meades.on3%>%filter(duplicated(scientificNameSimple))%>%pull(scientificNameSimple)))
meades.on3<-meades.on3[!(scientificNameSimple=='Epilobium ciliatum'&is.na(distribution)),]
meades.on3[is.na(distribution),]# devrait être vide, toutes les entrées devraient avoir une distribution

# 2.8. Effectuer une deuxième jointure avec VASCAN après les simplifications.
mead.vascan2 <- left_join(
  meades.on3 |> filter(!is.na(distribution)),
  vascan.taxon.raw |> select(taxonID, parentNameUsageID, scientificNameAuthorship, scientificName, scientificNameSimple, accScientificName, taxonRank, family, infraspecificEpithet),
  by = "scientificNameSimple"
)

mead.vascan2[is.na(accScientificName),]

# Cas particulier à documenter : erreur possible dans VASCAN pour
# Epilobium ciliatum subsp. ciliatum × Epilobium palustre, sous le taxonID 28156.
# Le taxon hybride semble manquer dans l'extraction :
# https://data.canadensys.net/vascan/taxon/6762


# ==============================================================================
# 3. Synthèse de la distribution géographique selon VASCAN
# ==============================================================================

# 3.1. Passer du format long au format large : une ligne par taxon et une colonne
# par territoire, avec la méthode d'établissement comme valeur.
ON_AT.table1 <- dcast(vascan.taxon.dist, taxonID ~ locality, value.var = "establishmentMeans") |>
  select(taxonID, Labrador, `New Brunswick`, Newfoundland, `Nova Scotia`, Ontario, `Prince Edward Island`, Quebec) |>
  left_join(vascan.taxon.raw |> select(taxonID, taxonRank, scientificNameSimple, genus, family), by = "taxonID")

ON_AT.table1[ON_AT.table1 == ""] <- NA

# Construire une seconde table à partir du statut de présence.
ON_AT.table1.alt <- dcast(vascan.taxon.dist, taxonID ~ locality, value.var = "occurrenceStatus") |>
  select(taxonID, Labrador, `New Brunswick`, Newfoundland, `Nova Scotia`, Ontario, `Prince Edward Island`, Quebec) |>
  left_join(vascan.taxon.raw |> select(taxonID, taxonRank, scientificNameSimple, genus, family), by = "taxonID")

# Repérer les taxons absents du Québec ou sans statut de présence au Québec,
# mais signalés comme absents dans au moins un territoire voisin.
ON_AT.table1.extirpated <- ON_AT.table1.alt[Quebec != "present" | is.na(Quebec), ] |>
  filter(Quebec == "absent" | `New Brunswick` == "absent" | Ontario == "absent" | Labrador == "absent")

nrow(ON_AT.table1.extirpated)#32 lignes

# Ajouter les taxons extirpés : VASCAN indique "absent", tandis que le portail
# en ligne utilise "extirpated". Ces enregistrements sont réintégrés à la table.
ON_AT.table1 <- rbind(ON_AT.table1[!taxonID %in% ON_AT.table1.extirpated$taxonID, ], ON_AT.table1.extirpated)
ON_AT.table1[ON_AT.table1 == ""] <- NA

# 3.2. Créer une classe de distribution spécifique au projet à partir des statuts provinciaux.
ON_AT.table1$dist <- ON_AT.table1$Quebec
ON_AT.table1[is.na(Quebec), dist := "hors-QC"]

ON_AT.table1[is.na(Quebec) & !is.na(`New Brunswick`), dist := "NB sans QC"]
ON_AT.table1[is.na(Quebec) & !is.na(Labrador), dist := "LB sans QC"]

ON_AT.table1[
  is.na(Quebec) & is.na(`New Brunswick`) & is.na(Labrador) &
    (!is.na(`Nova Scotia`) | !is.na(`Prince Edward Island`)),
  dist := "NE et IPE sans QC/LB/NB"
]

ON_AT.table1[
  is.na(Quebec) & is.na(`New Brunswick`) & is.na(Labrador) &
    is.na(`Nova Scotia`) & is.na(`Prince Edward Island`) & !is.na(Newfoundland),
  dist := "TN sans QC/LB/NE/IPE/NB"
]

ON_AT.table1[
  is.na(Quebec) & is.na(`New Brunswick`) & is.na(Labrador) & !is.na(Ontario),
  dist := "ON sans QC/LB/NB"
]

ON_AT.table1[
  is.na(Quebec) & is.na(Labrador) & is.na(`New Brunswick`) &
    is.na(`Nova Scotia`) & is.na(`Prince Edward Island`) & is.na(Newfoundland) & is.na(Ontario),
  dist := "excluded tous ou absent NE/IPE/TN"
]

# 3.3. Examiner les effectifs par classe de distribution.
table(ON_AT.table1$dist)

provinces_abréviations<-data.table(abréviation=c("QC", "LB", "NB", "NÉ", "IPÉ", "TN", "ON"),
                                    province=c("Québec", "Labrador", "Nouveau-Brunswick", "Nouvelle-Écosse", 
                                               "Île-du-Prince-Édouard", "Terre-Neuve", "Ontario"))
provinces_abréviations

dist_noms_complets<-data.table(
  dist=c("native", "introduced", "ON nord EMVS", "ON sans QC/LB/NB", 
  "excluded tous ou absent NE/IPE/TN", "ON nord S4 S5", "NB sans QC", 
  "NE et IPE sans QC/LB/NB", "TN sans QC/LB/NE/IPE/NB", 
  "LB sans QC", "absent", "ON nord absent"),
  
  dist_label=c("Indigène au QC", "Introduit au QC",
                             "Présent seulement en ON, répartition dans le Nord selon NHIC",
                             "Présent seulement en ON, répartition imprécise dans la province",
                              
                             "Exclu de toutes les provinces ou absent de la NÉ, de ÎPE et de TN",
                              "Présent seulement en ON, répartition dans le Nord selon Meades",
                              "Présent au NB, absent au QC", "Présent au NE et IPE, absent au QC, LB et NB", "Présent à TN, absent QC,LB,NB,IPE et NE",
                              "Présent au LB, absent au QC", "Extirpé du QC",
                              "Présent seulement en ON, absence d'équivalence dans les listes du Nord"))
dist_noms_complets

# Distinguer les taxons inclus dans le territoire de ceux omis ou à déterminer.
ON_AT.table1$source <- "autre"
ON_AT.table1[dist %in% c("introduced", "native", "absent", "LB sans QC", "NB sans QC"), source := "VASCAN inclu"]
ON_AT.table1[dist %in% c("NE et IPE sans QC/LB/NB", "TN sans QC/LB/NE/IPE/NB", "excluded tous ou absent NE/IPE/TN"), source := "VASCAN omis"]

table(ON_AT.table1$source)
# "autre" indique les espèces du Nord de l'Ontario (selon VASCAN) pour lesquelles il faut déterminer sud ou nord

# Objet produit : ON_AT.table1
# Table de distribution géographique au format large, enrichie d'informations
# taxonomiques et d'une classe synthétique (dist).
#
# Les valeurs manquantes et les
# statuts "absent" ne sont pas équivalents : absent dans le fichier téléchargé du IPT semble être associé à "extirpé"
# La classe "dist" repose en partie sur
# la présence ou l'absence de valeurs (absence de valeurs pour les absences avec aucune annotation particulière).


# ==============================================================================
# 4. Intégration des données EMVS du nord de l'Ontario
# ==============================================================================

# Les données de distribution ont été téléchargées depuis le portail Ontario GeoHub :
# https://geohub.lio.gov.on.ca/datasets/lio::provincially-tracked-species-1km-grid/about
# Une transformation dans QGIS a permis de soustraire le sud de l'Ontario.
sp_ontario_emvs <- terra::vect("C:/Users/Kellina/GeoFlora/2026+ Projet nouvelle flore du Québec/carto/PROVTRKG/prov_ontario_nord_sel.shp")
sp_ontario_emvs.df <- as.data.table(sp_ontario_emvs)

# 4.1. Associer les taxons EMVS aux noms de VASCAN.
# Utiliser la liste taxonomique complète du Canada, car certains parents peuvent
# être absents de la liste VASCAN restreinte aux distributions retenues.
list_ontario_emvs.df <- sp_ontario_emvs.df[!duplicated(sSCIENTIFI), ] |>
  filter(sELEMENT_T != "WILDLIFE CONCENTRATION AREA") |>
  filter(sSCIENTIFI %in% vascan.taxon.raw.can$scientificNameSimple) |>
  left_join(vascan.taxon.raw.can |> select(scientificNameSimple, accScientificName), by = c("sSCIENTIFI" = "scientificNameSimple"))

# Résultat initial : 636 taxons (espèces et taxons infraspécifiques) associés
# aux noms de VASCAN ; ce nombre est recalculé avec chaque mise à jour des données.
vascan.taxon.raw.can[scientificNameSimple %in% list_ontario_emvs.df$sSCIENTIFI & taxonomicStatus == "synonym", ] # 28 avec synonymes
vascan.taxon.raw.can[scientificNameSimple %in% list_ontario_emvs.df$sSCIENTIFI & taxonomicStatus == "accepted", ] # 608 avec noms acceptés

# 4.2. Associer les taxons EMVS à la classe de distribution de VASCAN.
list_ontario_emvs.df.dist <- left_join(
  list_ontario_emvs.df,
  ON_AT.table1 |> select(scientificNameSimple, dist),
  by = c("accScientificName" = "scientificNameSimple")
)

#si le nom est reconnu au niveau de la sous-espèce dans le QC/LB/NB mais à l'espèce dans NHIC, alors aucune distribution ne serait associée à cette étape

list_ontario_emvs.df.dist$source <- "autre"
list_ontario_emvs.df.dist[dist == "ON sans QC/LB/NB", source := "ON NHIC taxon"]
list_ontario_emvs.df.dist[dist %in% c("introduced", "native", "absent", "LB sans QC", "NB sans QC"), source := "VASCAN inclu"]
list_ontario_emvs.df.dist[dist %in% c("NE et IPE sans QC/LB/NB", "TN sans QC/LB/NE/IPE/NB", "excluded tous ou absent NE/IPE/TN"), source := "VASCAN omis"]
list_ontario_emvs.df.dist[is.na(dist), source := "ON NHIC parent"]

table(list_ontario_emvs.df.dist$source)

# Résultats initiaux à documenter :
# - 301 taxons reconnus au niveau de la variété ou de la sous-espèce dans VASCAN.
# - 79 taxons supplémentaires dont le parent est présent dans la liste EMVS.
# - Euthamia caroliniana et Thaspium barbinode sont deux taxons EMVS du nord
#   de l'Ontario exclus de VASCAN dans cette province.

# 4.3. Reclasser les taxons signalés uniquement en Ontario dans la liste EMVS.
ON_AT.table2<-data.table::copy(ON_AT.table1)
ON_AT.table2[scientificNameSimple %in% list_ontario_emvs.df$accScientificName & dist == "ON sans QC/LB/NB", source := "ON NHIC taxon"]
ON_AT.table2[scientificNameSimple %in% list_ontario_emvs.df$accScientificName & dist == "ON sans QC/LB/NB", dist := "ON nord EMVS"]

table(ON_AT.table2$dist)

# Exemples à présenter dans le document R Markdown :
# - Juncus arcticus : exemple de taxon classé "ON NHIC parent", avec un échange
#   entre les niveaux espèce et sous-espèce associé à la synonymie.
list_ontario_emvs.df.dist[accScientificName %like% "Juncus arcticus", ] |>
  select(sELEMENT_T:source)

# - Carex bigelowii : l'EMVS indique l'espèce alors que VASCAN ne mentionne
#   qu'une sous-espèce.
list_ontario_emvs.df.dist[accScientificName %like% "Carex bigelowii", ] |>
  select(sELEMENT_T:source)

ON_AT.table2[scientificNameSimple %like% "Carex bigelowii", ]%>%
  select(taxonID, taxonRank, scientificNameSimple, genus, family, dist, source)

# - Vérifier les cas où l'espèce et une variété indigène sont toutes deux mentionnées par NHIC
list_ontario_emvs.df.dist[accScientificName %like% "Asplenium scolopendrium", ]
list_ontario_emvs.df.dist[accScientificName %like% "Bouteloua curtipendula", ]

#Maianthemum canadense subsp. interius n'est pas dans la liste des EMVS de l'Ontario
#(note : distribution GBIF semble coupée à la frontière du Québec, FNA ne reconnaît pas la sous-espèce)
list_ontario_emvs.df.dist[accScientificName %like% "Maianthemum canadense", ] |>
  select(sELEMENT_T:source)

# Examiner les taxons classés "ON NHIC parent".
list_ontario_emvs.df.dist[source == "ON NHIC parent", ] |>
  select(sELEMENT_T:source)%>%head()

# 4.4. Repérer les taxons infraspécifiques potentiellement rares, présents uniquement
# en Ontario selon VASCAN, dont le parent est identifié à l'espèce dans la liste EMVS.
# Les taxons introduits sont exclus de cette recherche.
ON_parents <- left_join(
  ON_AT.table2[dist %in% c("ON sans QC/LB/NB", "NE et IPE sans QC/LB/NB", "excluded tous ou absent NE/IPE/TN")
              & taxonRank %in% c("variety", "subspecies") & Ontario != "introduced", ],
  vascan.taxon.raw.can |> select(taxonID, parentNameUsageID, parentNameUsage)
) |>
  left_join(
    vascan.taxon.raw.can |> select(scientificNameSimple, taxonID) |> dplyr::rename(parentNameSimple = scientificNameSimple),
    by = c("parentNameUsageID" = "taxonID")
  )

all_parents <- left_join(
  ON_AT.table2[taxonRank %in% c("variety", "subspecies"), ],
  vascan.taxon.raw.can |> select(taxonID, parentNameUsageID, parentNameUsage)
) |>
  left_join(
    vascan.taxon.raw.can |> select(scientificNameSimple, taxonID) |> dplyr::rename(parentNameSimple = scientificNameSimple),
    by = c("parentNameUsageID" = "taxonID")
  )

# Examiner les taxons EMVS dont le parent ne possède pas de classe de distribution.
left_join(
  list_ontario_emvs.df.dist[source == "ON NHIC parent", ] |> select(-dist, -source),
  all_parents,
  by = c("accScientificName" = "parentNameSimple")
) |>
  filter(is.na(dist))

# Résultat initial : un taxon, Atriplex suckleyi, est présent dans la liste EMVS
# sans être mentionné par VASCAN comme présent en Ontario.

# 4.5. Associer la distribution des parents et éviter les doublons.
interim_selon_parent <- left_join(
  list_ontario_emvs.df.dist[source == "ON NHIC parent", ] |> select(!dist),
  all_parents%>%dplyr::rename("source_initiale"="source"),
  by = c("accScientificName" = "parentNameSimple")
) |>
  filter(!is.na(dist))
#interim selon parent inclus des taxons pour lesquels la distribution inclut déjà le Québec et/ou Nouveau-Brunswick
table(interim_selon_parent$dist)
#seulement 41 taxons pour lesquels le parent est utile pour classifier car 8 déjà classifiés comme EMVS Ontario et catégories déjà inclues (e.x. "native" ou "NB sans QC")

# Série initiale de huit taxons pour lesquels la liste EMVS mentionne le taxon
# infraspécifique et le parent au niveau de l'espèce, ce dernier pouvant inclure
# des taxons infraspécifiques non protégés.
exclure_parent <- interim_selon_parent |> filter(dist == "ON nord EMVS") |> pull(accScientificName)

tx_ON_nord_EMVS_selon_parent <- left_join(
  ON_parents[!parentNameSimple %in% exclure_parent, ]%>%select(-source),
  list_ontario_emvs.df.dist[source == "ON NHIC parent", ] |>
    select(sSCIENTIFI, accScientificName, source),
  by = c("parentNameSimple" = "accScientificName")
) |>
  filter(!is.na(sSCIENTIFI))

nrow(tx_ON_nord_EMVS_selon_parent)#38 taxons
#(moins que la liste de tous les parents car les parents déjà dans la liste du territoire sont écartés ici)
length(exclure_parent)#8 taxons


ON_AT.table2[scientificNameSimple %in% tx_ON_nord_EMVS_selon_parent$scientificNameSimple, dist := "ON nord EMVS"]
ON_AT.table2[scientificNameSimple %in% tx_ON_nord_EMVS_selon_parent$scientificNameSimple, source := "ON NHIC parent"]


# Résultat du 2026-10-23 : 683 taxons demeurent à déterminer pour leur distribution
# en Ontario après intégration des données EMVS. La liste compte 413 taxons EMVS
# du nord de l'Ontario, davantage que la somme de 301 + 79, car un taxon parent
# peut correspondre à plusieurs taxons infraspécifiques.
# Résultat du 2026-10-26 : 759 taxons demeurent à déterminer pour leur distribution
# en Ontario après intégration des données EMVS. La liste compte 337 taxons EMVS
# du nord de l'Ontario, davantage moins que la somme de 301 + 79, car un taxon parent
# peut correspondre à un taxon déjà dans la distribution du QC/LB/NB.
table(ON_AT.table2$dist)


# ==============================================================================
# 5. Intégration de la liste de S. Meades
# ==============================================================================
ON_AT.table3<-data.table::copy(ON_AT.table2)


# 5.1. Associer les taxons de Meades à la distribution de VASCAN.
mead.dist <- left_join(
  mead.vascan2 |> select(!family:infraspecificEpithet) |> select(-taxonRank, -taxonID),
  ON_AT.table3,
  by = join_by("accScientificName" == "scientificNameSimple")
)

table(mead.dist$distribution)

# Les taxons signalés dans le nord de l'Ontario par Meades, mais absents du
# territoire Québec–Labrador–Nouveau-Brunswick selon VASCAN, sont reclassés.
mead.dist[dist %in% c("ON sans QC/LB/NB") & distribution == "nord Ontario selon S. Meades", dist := "Ontario nord S4 S5"]
mead.dist[dist %in% c("ON sans QC/LB/NB") & distribution == "absent", dist := "Ontario nord absent"]

# Vérifier les taxons sans classes de distribution et ceux exclus ou absents.
mead.dist[is.na(dist) & distribution != "absent", ]
mead.dist[dist == "excluded ou absent NE/IPE/TN", ]

# 5.2. Répercuter les classes de Meades dans la table générale.
ON_AT.table3[scientificNameSimple %in% mead.dist[dist == "Ontario nord S4 S5", ]$scientificNameSimple, dist := "ON nord S4 S5"]
ON_AT.table3[scientificNameSimple %in% mead.dist[dist == "Ontario nord S4 S5", ]$scientificNameSimple, source := "ON Meades taxon"]

ON_AT.table3[scientificNameSimple %in% mead.dist[dist == "Ontario nord absent", ]$scientificNameSimple, dist := "ON nord absent"]
ON_AT.table3[scientificNameSimple %in% mead.dist[dist == "Ontario nord absent", ]$scientificNameSimple, source := "ON Meades taxon"]

table(ON_AT.table2$dist)
table(ON_AT.table3$dist)
table(mead.dist$dist)

# 5.3. Associer la distribution des parents aux taxons infraspécifiques de Meades.
# Cette étape vise les taxons dont le nom n'a pas été apparié directement à VASCAN.
parentNames <- mead.vascan2[is.na(scientificName)] |>
  select(scientificNameSimple, originalNameSimple, distribution)

# Reconstituer la table des parents après l'ajout des distributions EMVS.
all_parents <- left_join(
  ON_AT.table3[taxonRank %in% c("variety", "subspecies"), ],
  vascan.taxon.raw.can |> select(taxonID, parentNameUsageID, parentNameUsage)
) |>
  left_join(
    vascan.taxon.raw.can |> select(scientificNameSimple, taxonID) |> dplyr::rename(parentNameSimple = scientificNameSimple),
    by = c("parentNameUsageID" = "taxonID")
  )

# Vérifier les parents classés comme présents uniquement en Ontario selon VASCAN.
parents_unique_Ontario_VASCAN<-left_join(parentNames, 
          all_parents%>%dplyr::rename("scientificNameSimple_infra"="scientificNameSimple"),
          by = c("scientificNameSimple" = "parentNameSimple")) |>
  filter(dist == "ON sans QC/LB/NB")%>%
  select(scientificNameSimple, scientificNameSimple_infra, distribution, taxonID, dist, source, parentNameUsage)
head(parents_unique_Ontario_VASCAN)
table(parents_unique_Ontario_VASCAN$distribution)

# Résultat du 2026-10-22 : cinq taxons, soit un incertain, un absent et trois présents.
# Résultat du 2026-10-26 : dix-sept taxons, soit un incertain, quatre absents

# Grindelia squarrosa est un exemple de taxon exclu du fichier EMVS.
ON_AT.table3[scientificNameSimple %like% "Grindelia squarrosa", ]


# note du 2026-10-22 : Eutrochium maculatum est déjà traité dans les données EMVS.
# mais en 2026-10-26 : cette espèce n'est pas dans la table emvs de l'Ontario
ON_AT.table3[scientificNameSimple %like% "Eutrochium maculatum", ]
list_ontario_emvs.df[sSCIENTIFI%like%'Eutrochium',]
list_ontario_emvs.df[sSCIENTIFI%like%'Eupatorium',]
list_ontario_emvs.df[accScientificName%like%'Eutrochium',]
list_ontario_emvs.df[accScientificName%like%'Eupatorium',]


# Ajouter les distributions des parents à la table des taxons infraspécifiques.
ajout_parents <- inner_join(all_parents, #%>%dplyr::rename("scientificNameSimple_infra"="scientificNameSimple")
                            parentNames, by = c("parentNameSimple" = "scientificNameSimple"))
ajout_parents[scientificNameSimple %like% "Grindelia squarrosa", ]

ON_AT.table3[
  dist == "ON sans QC/LB/NB" &
    scientificNameSimple %in% ajout_parents[distribution == "nord Ontario selon S. Meades", ]$scientificNameSimple,
  source := "ON Meades parent"
]

ON_AT.table3[
  dist == "ON sans QC/LB/NB" &
    scientificNameSimple %in% ajout_parents[distribution == "nord Ontario selon S. Meades", ]$scientificNameSimple,
  dist := "ON nord S4 S5"
]

ON_AT.table3[
  dist == "ON sans QC/LB/NB" &
    scientificNameSimple %in% ajout_parents[distribution == "absent", ]$scientificNameSimple,
  source := "ON Meades parent"
]

ON_AT.table3[
  dist == "ON sans QC/LB/NB" &
    scientificNameSimple %in% ajout_parents[distribution == "absent", ]$scientificNameSimple,
  dist := "ON nord absent"
]

table(ON_AT.table3$dist)
table(ON_AT.table3$source)


# ==============================================================================
# 6. Classification finale des taxons et exportation
# ==============================================================================

# 6.1. Attribuer le territoire d'inclusion à chaque taxon.
table(ON_AT.table3$source)
ON_AT.table3[source=='autre', source:='absent des listes NHIC et Meades']
ON_AT.table3$source<-factor(ON_AT.table3$source,
                            levels=c("VASCAN inclu", "VASCAN omis", 
                                     "ON NHIC taxon", "ON NHIC parent",
                                     "ON Meades taxon", "ON Meades parent",
                                     "absent des listes NHIC et Meades"))
table(ON_AT.table3$source)

ON_AT.table3$territoire <- "autre"

ON_AT.table3[dist %in% c("introduced", "native", "absent", "LB sans QC", "NB sans QC"), territoire := "QC inclu"]
ON_AT.table3[dist %in% c("LB sans QC", "NB sans QC"), territoire := "ajout LB"]
ON_AT.table3[dist %in% c("NB sans QC"), territoire := "ajout NB"]

ON_AT.table3[dist %in% c("ON nord EMVS", "ON nord S4 S5"), territoire := "potentiel ON"]

ON_AT.table3[
  dist %in% c("NE et IPE sans QC/LB/NB", "TN sans QC/LB/NE/IPE/NB", "excluded tous ou absent NE/IPE/TN", "ON sans QC/LB/NB", "ON nord absent"),
  territoire := "omettre"
]

ON_AT.table3$territoire<-factor(ON_AT.table3$territoire, levels=c("QC inclu", "ajout LB", "ajout NB", "potentiel ON", "omettre"))
table(ON_AT.table3$territoire)

ON_AT.table3[territoire=='autre',]

# 6.2. Préparer les fichiers d'exportation.
# L'export VASCAN est destiné à être mis à jour lors du push initial ; il ne doit
# pas être régénéré systématiquement sans vérifier les changements de sélection.
vascan.export <- vascan.taxon.raw[acceptedNameUsageID %in% ON_AT.table3[territoire != "omettre"]$taxonID, ]%>%
  select(-references, -bibliographicCitation, -nameAccordingTo, -nameAccordingToID, -license)
ON_AT.table.export <- ON_AT.table3 |> select(taxonID, dist:territoire)

# Exporter les données principales et une copie datée pour les sauvegardes.
#write.table(vascan.export, "output data/taxon_sel_VASCANv37_17.txt")
#write.table(ON_AT.table.export, "output data/distribution_taxons_sel.txt")

write.table(vascan.export, paste0("output data/taxon_sel_VASCANv37_17_", Sys.Date(), ".txt"))
write.table(ON_AT.table.export, paste0("output data/distribution_taxons_sel_", Sys.Date(), ".txt"))

# Objets produits :
# - vascan.export : données taxonomiques retenues pour l'exportation.
# - ON_AT.table.export : classe de distribution géographique et territoire d'inclusion retenus.


# ==============================================================================
# 7. Calcul des ajouts de taxons par genre
# ==============================================================================

# 7.1. Compter les taxons par genre et par territoire.
listajouts <- left_join(ON_AT.table.export[territoire != "omettre", ], vascan.export)

candidats_long <- listajouts[, list(nb.taxa = length(taxonID)), by = .(genus, territoire)] |>
  arrange(genus, territoire)

candidats <- dcast(candidats_long, genus ~ territoire, value.var = "nb.taxa")
candidats[is.na(candidats)] <- 0

# Calculer le nombre de taxons dans le territoire de base et les ajouts.
candidats$QC_LB_NB <- rowSums(candidats |> select(`QC inclu`, `ajout LB`, `ajout NB`))
candidats

# 7.2. Calculer la proportion de taxons potentiels du nord de l'Ontario.

candidats$pourcentage_ajout <- round(candidats$`potentiel ON` / candidats$QC_LB_NB * 100)
candidats[QC_LB_NB == 0, pourcentage_ajout := 100]

# 7.3. Classer les genres selon le nombre de taxons déjà inclus et le nombre d'ajouts potentiels.
candidats$cat <- "à déterminer"
candidats[QC_LB_NB == 0, cat := "nouveau genre"]
candidats[`potentiel ON` == 0, cat := "aucun"]

candidats[QC_LB_NB == 1 & `potentiel ON` == 1, cat := "plus 1 taxon dans monospécifique"]
candidats[QC_LB_NB == 2 & `potentiel ON` == 1, cat := "plus 1 taxon dans duospécifique"]
candidats[QC_LB_NB > 2 & `potentiel ON` == 1, cat := "plus 1 taxon dans 3 ou plus"]

candidats[QC_LB_NB <= 2 & `potentiel ON` == 2, cat := "plus 2 taxons dans mono ou duospécifique"]
candidats[QC_LB_NB > 2 & `potentiel ON` == 2, cat := "plus 2 taxons dans 3 ou plus"]

candidats[`potentiel ON` == 3, cat := "plus 3 taxons"]
candidats[`potentiel ON` == 4, cat := "plus 4 taxons"]
candidats[`potentiel ON` == 5, cat := "plus 5 taxons"]
candidats[`potentiel ON` > 5, cat := "6 ou plus taxons"]

# Repérer les genres dont la catégorie reste à déterminer.
candidats[cat == "à déterminer", ]
table(candidats$cat)

# Examiner les genres qui comptent plus de cinq taxons potentiels.
candidats[`potentiel ON` > 5, ]

# Fin du script de détermination de la couverture géographique du projet.


#rmarkdown::render(
# "couverture_geographique_projet.Rmd", output_format = "word_document",
# output_file = paste0("documents et rapports/Couverture_geographique_establishmentMeans_ProjetNouvelleFlore", Sys.Date(), ".docx"),
# clean = FALSE, quiet = FALSE
#   )
