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
library(readODS)

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
names(distribution_sel)

attribution_fichiers <- list.files("output data",
                                   pattern = "^attribution_regroupements_", full.names = TRUE)
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
# 1. Chargement et préparation des données de vocabulaire contrôlé#####
# ==============================================================================


structures.import<-readODS::read_ods("documents et rapports/Formulaire_FVQ_v017.ods",
                  sheet="Structures")
names(structures.import)
structures<-structures.import%>%select(ID:`Libellé (auto)`)%>%
            rename("section"="Paragraphe (sections)", "structure"="Libellé (auto)")

caracteres.import<-structures.import<-readxl::read_xlsx("documents et rapports/Formulaire_FVQ_v017_import.xlsx",
                                                        sheet="Caractères")
caracteres.list<-caracteres.import%>%select(`Caractère`:`Définition et exemples`)%>%
                  rename("caractere"="Caractère", "lien_ref"="Lien avec la référence", "adjectif_m"="Adjectif m. (ratios)",
                         "adjectif_f"="Adjectif f. (ratios)", "def"="Définition et exemples")

qualificatif.list<-caracteres.import%>%select(`Qualificatif (m. sg.)`:`f. pl.`)%>%
                  rename("qualificatif"="Qualificatif (m. sg.)", "qualificatif_f_sg"="f. sg.",
                         "qualificatif_m_pl"="m. pl.", "qualificatif_f_pl"="f. pl.")

unites.list<-caracteres.import%>%select(`Unité`)%>%
                  rename("unite"="Unité")

statistique.list<-caracteres.import%>%select(Statistique)%>%
          rename("statistique"="Statistique")

genre.list<-caracteres.import%>%select(Genre)%>%
          rename("genre"="Genre")

nombre.usage.list<-caracteres.import%>%select(`Nombre d'usage`)%>%
          rename("nombre_usage"="Nombre d'usage")

type.choix.list<-caracteres.import%>%select(`Type (choix)`)%>%
          rename("type_choix"="Type (choix)")

traits.import<-read_ods("documents et rapports/Formulaire_FVQ_v017.ods",
                                             sheet="Entrée de données",decimal = ",")
traits.names<-as.vector(t(traits.import[1,2:ncol(traits.import)]))
traits.gr<-traits.import[2:nrow(traits.import),2:ncol(traits.import)]
names(traits.gr)<-traits.names


# ==============================================================================
# 2. Importation des données de traitements#####
# ==============================================================================

#test avec Gaultheria
fichier.traitement<-"input data/traitements formulaire v17/Gaultheria_JB_v1_1.ods"
fichier.traitement<-"input data/Formulaire_Uvularia_MLapointe.xlsx"
traits.import<-read_ods(fichier.traitement,
                                 sheet="Entrée de données")

traits.names<-as.vector(t(traits.import[1,2:ncol(traits.import)]))
ncols_keep<-length(traits.names[!is.na(traits.names)])
traits.gr<-traits.import[2:nrow(traits.import),2:(ncols_keep+1)]
names(traits.gr)<-traits.names[1:ncols_keep]
traits.gr<-traits.gr%>%rename("structure"="Structure",
                              "caractere"="Caractère",
                              "statistique"="Statistique",
                              "unite"="Unité")


structures.import.gr<-readODS::read_ods("input data/Gaultheria_JB.xlsx",
                                     sheet="Structures")
structures.gr<-structures.import.gr%>%select(ID:`Libellé (auto)`)%>%
  rename("section"="Paragraphe (sections)", "structure"="Libellé (auto)")

structures.add<-structures.gr[!structures.gr$structure%in%structures$structure,]

structures<-rbind(structures,
                  structures.add)
#validation que vide
traits.gr[!traits.gr$structure%in%structures$structure,]

traits.gr[traits.gr$structure%in%structures$structure,]


