# Attribution des groupes taxonomiques aux rédacteurs - phase Décembre 2026
# ====================================================================================
# Sections préalables:
#   1. Chargement et préparation des données VASCAN
#   2. Regroupement des taxons par groupe taxonomique
#   3. Intégration des statuts provinciaux EMVS
#   4. Exportation des taxons par regroupement_cle pour les rédacteurs
####DÉBUT script particulier#####
#   A. Importation des proposition de taxons par les rédacteurs - et approbation
#   B. Exportation de la sélection des taxons pour un rédacteur
# ====================================================================================

library(tidyr)
library(dplyr)
library(data.table)
library(taxize)
library(stringr)
library(openxlsx)


# ==============================================================================
# 1. Chargement et préparation des données VASCAN, distribution, attribution actuelle#####
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
names(distribution_sel)

attribution_fichiers <- list.files("output data",
                                    pattern = "^attribution_regroupements", full.names = TRUE)
attribution_fichiers_date <- as.Date(stringr::str_extract(attribution_fichiers, "\\d{4}-\\d{2}-\\d{2}"))
attribution_fichier_recent <- attribution_fichiers[which.max(attribution_fichiers_date)]
attribution_sel <- as.data.table(read.table(attribution_fichier_recent))
names(attribution_sel)

# Associer les données taxonomiques à la distribution en excluant les taxons "omettre"
# dont le territoire est classé "omettre".
taxons_prj <- left_join(
  distribution_sel[territoire != "omettre", ],
  taxons_tous |> 
    select(
      #-bibliographicCitation, -license, -references,-nameAccordingToID, -nameAccordingTo,
      -taxonomicStatus
    ))|>
  left_join(attribution_sel)

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
# A. objet pour recueillir choix####
# ==============================================================================

attribution_sel_temp<-left_join(attribution_sel, taxons_prj%>%filter(!duplicated(groupe_taxo))%>%
                                  select(order, family, groupe_taxo))%>%
                                      mutate(Approbation_Comite_Date=as.Date(Approbation_Comite_Date),
                                             mise_a_jour_date=as.Date(mise_a_jour_date))


# ==============================================================================
# C. Ajouts - sélections par rédacteurs (phase décembre 2026) ####
# ==============================================================================


###exemples ici à effacert####
attribution_sel_temp[groupe_taxo%in%c('Shepherdia'),`:=`(Interet_Redacteur='Mélanie Lapointe',
                                                         état='approbation - phase 1',
                                                         Approbation_Comite_Date=as.Date('2026-11-13'),
                                                         mise_a_jour_date=as.Date('2026-11-13'))]
attribution_sel_temp[groupe_taxo%in%c('Corylus', 'Carya', 'Aronia'),`:=`(Interet_Redacteur='Mélanie Lapointe',
                                                                         état='attente sélection taxons horticoles',
                                                                         Approbation_Comite_Date=as.Date('2026-11-13'),
                                                                         mise_a_jour_date=as.Date('2026-11-13'))]

attribution_sel_temp[groupe_taxo%in%c('Sagittaria', 'Potamogeton', 'Myriophyllum'),`:=`(Interet_Redacteur='Étienne Léveillé-Bourret',
                                                                                        mise_a_jour_date=as.Date('2026-11-24'))]

attribution_sel_temp[groupe_taxo%in%c('Senega'),`:=`(Interet_Redacteur='Patrick Gravel',
                                                     état='approbation - phase 2',
                                                     Approbation_Comite_Date=as.Date('2026-11-22'),
                                                     mise_a_jour_date=as.Date('2026-11-22'))]

attribution_sel_temp[groupe_taxo%in%c('Ribes', 'Alisma'),`:=`(Interet_Redacteur='Séléna Bergeron',
                                                              état='approbation - continuation',
                                                              Approbation_Comite_Date=as.Date('2026-11-22'),
                                                              mise_a_jour_date=as.Date('2026-11-22'))]
attribution_sel_temp[groupe_taxo%in%c('Hydrophyllum'),`:=`(Interet_Redacteur='Séléna Bergeron',
                                                           état='en attente FNA',
                                                           Approbation_Comite_Date=as.Date('2026-11-22'),
                                                           mise_a_jour_date=as.Date('2026-11-22'))]


attribution_sel_temp[Interet_Redacteur%like%'Séléna',]

attribution_sel_temp[groupe_taxo%in%c('Apocynum'),`:=`(Interet_Redacteur='Étienne Lacroix-Carignan',
                                                              état='rédaction complétée',
                                                              Approbation_Comite_Date=as.Date('2026-11-22'),
                                                              mise_a_jour_date=as.Date('2026-11-22'))]

attribution_sel_temp[Interet_Redacteur%like%'Lacroix',]

# ==============================================================================
# 4. Regroupement de genres pour les clés ####
# ==============================================================================


##exemples à effacer####
attribution_sel_temp[groupe_taxo%in%c('Schoenoplectiella', 'Schoenoplectus'), regroupement_cle:='Schoenoplectus_sensu_lato']
attribution_sel_temp[groupe_taxo%in%c('Pyrola', 'Moneses', 'Orthilia'), regroupement_cle:='Pyrola_sensu_lato']
#créer les regroupements pour la clé
attribution_sel_temp[groupe_taxo=='Calystegia'|groupe_taxo=='Convolvulus', regroupement_cle:='Calystegia_Convolvulus']
attribution_sel_temp[groupe_taxo=='Onoclea',] #déjà assigné
attribution_sel_temp[groupe_taxo%like%'Osmund',] #déjà assigné
#créer les regroupements pour la clé
attribution_sel_temp[groupe_taxo%in%c('Hypopitys', 'Monotropa'), regroupement_cle:='Monotropa_Hypopitys']

attribution_sel_temp[,c("groupe_taxo", "groupe_principal", "sous_groupe", "regroupement_cle", 
  "Priorisation", "Proposition_Responsable", "Interet_Redacteur", 
  "Approbation_Comite_Date", "état", "mise_a_jour_date")]

write.table(attribution_sel_temp%>%
              select(groupe_taxo, groupe_principal, sous_groupe, regroupement_cle, Priorisation,
                     Proposition_Responsable, Interet_Redacteur, Approbation_Comite_Date, `état`, `mise_a_jour_date`),
            paste0("output data/attribution_regroupements_check_", Sys.Date(), ".txt"))

write.table(attribution_sel_temp%>%
              select(groupe_taxo, groupe_principal, sous_groupe, regroupement_cle, Priorisation,
                     Proposition_Responsable, Interet_Redacteur, Approbation_Comite_Date, `état`, `mise_a_jour_date`),
            paste0("output data/attribution_regroupements_", Sys.Date(), ".txt"))

# ==============================================================================
# 4. Exportation en format convivial pour le publique ####
# ==============================================================================

#format pour le publique - lecteur convivial
taxons_export<-taxons_prj%>%select(groupe_taxo, regroupement_cle, scientificNameSimple, scientificName,taxonID, dist, territoire, source, 
                                 parentNameUsage, higherClassification, class:taxonRank)%>%#, `état`, mise_a_jour_date
                                  arrange(class, order, family, regroupement_cle, groupe_taxo, genus, territoire, scientificNameSimple)%>%
                left_join(attribution_sel_temp%>%select(groupe_taxo, regroupement_cle, Interet_Redacteur, Approbation_Comite_Date, `état`, `mise_a_jour_date`))

attribution_export<-attribution_sel_temp%>%left_join(gr_taxo%>%select(groupe_taxo, nb.taxa))%>%
  select(groupe_principal, sous_groupe, order, family, regroupement_cle, groupe_taxo, nb.taxa, Priorisation,
         Proposition_Responsable, Interet_Redacteur, Approbation_Comite_Date, `état`, `mise_a_jour_date`)

wb_tax <- createWorkbook()
largeur_colonnes <- function(df, max_width = 30) {
  pmin(max_width, pmax(10, sapply(df, \(x) max(nchar(as.character(x)), na.rm = TRUE)) + 2))
}
addWorksheet(wb_tax, "groupes_taxonomiques")
writeData(wb_tax, "groupes_taxonomiques", attribution_export)

addWorksheet(wb_tax, "taxons_projet")
writeData(wb_tax, "taxons_projet", taxons_export)

# Ajuster automatiquement les largeurs (maximum 30)
setColWidths(wb_tax, "groupes_taxonomiques",
             cols = 1:ncol(attribution_export),
             widths = largeur_colonnes(attribution_export))

setColWidths(wb_tax, "taxons_projet",
             cols = 1:ncol(taxons_export),
             widths = largeur_colonnes(taxons_export))

# Exporter
saveWorkbook(
  wb_tax,
  paste0("output data/Nouvelle_Flore_groupes_taxo_selection_redacteurs_", Sys.Date(), ".xlsx"),
  overwrite = TRUE
)

# ==============================================================================
# 5. Exportation de la sélection des taxons pour un rédacteur - exemples####
# ==============================================================================

redacteur_family = 'Osmundaceae'
redacteur_genus = 'Cicuta'
redacteur_family = 'Araceae'
redacteur_family = 'Fabaceae'

taxons_prj[family==redacteur_family&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple)

taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple)


