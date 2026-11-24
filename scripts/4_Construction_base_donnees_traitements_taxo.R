# Compilation des données de traitements taxonomiques
# A : à partir du formulaire pour la structure
# B : à partir des données des rédacteurs pour les valeurs

# Télécharger tous les scripts et les fichiers à partir du GitHub
# Conserver dans la même la racine du projet, car les chemins sont relatifs.
# Actualiser la liste des formulaires avant chaque lot.
# ====================================================================================
# Sections :
#   1. Chargement et préparation des données VASCAN, de distribution et d'attribution
#   2. Importation des vocabulaires contrôlés du formulaire maître
#   3. Importation, validation et compilation des traitements taxonomiques; export des résultats
#
# ====================================================================================

library(tidyr)
library(dplyr)
library(data.table)
library(taxize)
library(stringr)
library(readODS)

# Dépendances : installer les paquets chargés ci-dessus si nécessaire. readODS lit les fichiers
# ODS du formulaire maître et des formulaires de traitements.

# ==============================================================================
# 1. Chargement et préparation des données VASCAN#####
# ==============================================================================


# Charger les données taxonomiques sélectionnées et leur distribution géographique.
# Les fichiers les plus récents sont retenus selon la date AAAA-MM-JJ incluse dans leur nom.
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

# Associer les données taxonomiques à la distribution en excluant les taxons
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

# Le formulaire maître v017 fournit les vocabulaires de référence.
# En cas de mise à jour, vérifier le nom du fichier, celui des feuilles et les colonnes renommées ci-dessous.
# Si les colonnes demeurent à la même place, le script est flexible pour accomoder des modifications entre les colonnes nommées

#toutes les structures normées
structures.import<-readODS::read_ods("documents et rapports/Formulaire_FVQ_v017.ods",
                  sheet="Structures")
names(structures.import)
structures<-structures.import%>%select(ID:`Libellé (auto)`)%>%
            rename("section"="Paragraphe (sections)", "structure"="Libellé (auto)")

#tous les caractères, qualificatifs, unites, statistiques, genre et nombre.usage
# valeures acceptées dans les tables
caracteres.import<-structures.import<-readODS::read_ods("documents et rapports/Formulaire_FVQ_v017.ods",
                                                        sheet="Caractères")
caractere.list<-caracteres.import%>%select(`Caractère`:`Définition et exemples`)%>%
                  rename("caractere"="Caractère", "lien_ref"="Lien avec la référence", "adjectif_m"="Adjectif m. (ratios)",
                         "adjectif_f"="Adjectif f. (ratios)", "def"="Définition et exemples")

qualificatif.list<-caracteres.import%>%select(`Qualificatif (m. sg.)`:`f. pl.`)%>%
                  rename("qualificatif"="Qualificatif (m. sg.)", "qualificatif_f_sg"="f. sg.",
                         "qualificatif_m_pl"="m. pl.", "qualificatif_f_pl"="f. pl.")

unite.list<-caracteres.import%>%select(`Unité`)%>%
                  rename("unite"="Unité")

statistique.list<-caracteres.import%>%select(Statistique)%>%
          rename("statistique"="Statistique")

genre.list<-caracteres.import%>%select(Genre)%>%
          rename("genre"="Genre")

nombre.usage.list<-caracteres.import%>%select(`Nombre d'usage`)%>%
          rename("nombre_usage"="Nombre d'usage")

type.choix.list<-caracteres.import%>%select(`Type (choix)`)%>%
          rename("type_choix"="Type (choix)")

#valeurs vides du fichier maître d'Entrée de données
traits.import<-read_ods("documents et rapports/Formulaire_FVQ_v017.ods",
                                             sheet="Entrée de données")
traits.names<-as.vector(t(traits.import[1,2:ncol(traits.import)]))
traits.gr<-traits.import[2:nrow(traits.import),2:ncol(traits.import)]
names(traits.gr)<-traits.names


# ==============================================================================
# 2. Importation des données de traitements#####
# ==============================================================================


#Mettre à jour cette liste pour choisir les formulaires à compiler.

fichiers.traitement<-c(
  "traitement data/traitements formulaire v17/Gaultheria_JB_v1_1.ods",
  "traitement data/traitements formulaire v17/Uvularia_MLA_v1_3.ods",
  "traitement data/traitements formulaire v17/Apocynum_ELC_v1_0.ods",
  "traitement data/traitements formulaire v17/Crocanthemum_PG_v1_1.ods"
)

# Chemins relatifs à la racine du projet.
# Chaque classeur doit contenir les feuilles
# « Structures », « Entrée de données » et « Versions ».

# ==============================================================================
# 2b. Validation des structures ajoutés par les rédacteurs et compilation des ajouts#####
# ==============================================================================


#structures à ajouter à la base de données selon les fichiers importés
structures.fichiers<-lapply(fichiers.traitement, \(fichier) {
  
  structures.import.gr<-read_ods(fichier, sheet="Structures")%>%
    select(ID:`Libellé (auto)`)%>%
    rename("section"="Paragraphe (sections)",
           "structure"="Libellé (auto)", "qualificatif"="Qualificatif")
  
  qualificatif.invalides <- structures.import.gr%>%
    dplyr::filter(!qualificatif %in% qualificatif.list$qualificatif)
  
  if (nrow(qualificatif.invalides) > 0) {
    cat(
      "Erreur : qualificatif non reconnues : ",
      paste(unique(qualificatif.invalides$qualificatif), collapse = ", ")
    )}
  structures.import.gr
  
})%>%
  bind_rows()%>%
  distinct()


# Vérifier les nouvelles structures ajoutées dans structures.add
# Modifier le fichier d'entrée (dans la feuille Structure et la feuille Entrée de données)
# En cas de termes ajoutés incorrectement (visualisation ici utile pour détecter erreurs)
structures.add<-structures.fichiers%>%
  filter(!structure%in%structures$structure)
View(structures.add)

#nouvelle liste de structures complètes pour la validation
structures<-bind_rows(structures, structures.add)


# ==============================================================================
# 2c. Compile les valeurs entrées pour les taxons#####
# ==============================================================================

# Pour chaque classeur, transforme les colonnes de taxons en lignes, vérifie les vocabulaires,
# puis ajoute les rédacteurs, réviseurs, date et numéro de version.
traits.serie <- lapply(fichiers.traitement, function(fichier) {
  cat(fichier)
  traits.import <- readODS::read_ods(fichier,
    sheet = "Entrée de données")
  
traits.names<-as.vector(t(traits.import[1,2:ncol(traits.import)]))
ncols_keep<-length(traits.names[!is.na(traits.names)])
traits.gr<-traits.import[2:nrow(traits.import),2:(ncols_keep+1)]
names(traits.gr)<-traits.names[1:ncols_keep]
traits.gr<-traits.gr%>%rename("structure"="Structure",
                              "caractere"="Caractère",
                              "statistique"="Statistique",
                              "unite"="Unité")
#convert to long
traits.gr.long.incomplet <- traits.gr %>%
  tidyr::pivot_longer(
    cols = -dplyr::all_of(
      traits.gr %>%
        dplyr::select(structure:`Commentaire (non publié)`) %>%
        names()),
    names_to = "scientificNameSimple",
    values_to = "valeur")%>%
  select(scientificNameSimple, structure, caractere, statistique, valeur, `Structure de référence`:`Commentaire (non publié)`)%>%
  filter(!is.na(structure), !is.na(valeur))

#vérifie les vocabulaires
structures.invalides <- traits.gr.long.incomplet %>%
  dplyr::filter(!structure %in% structures$structure)

if (nrow(structures.invalides) > 0) {
  cat(
    "Erreur : structures non reconnues : ",
    paste(unique(structures.invalides$structure), collapse = ", ")
  )}
 

unite.invalides <- traits.gr.long.incomplet %>%
      dplyr::filter(!unite %in% unite.list$unite)
    
    if (nrow(unite.invalides) > 0) {
      cat(
        "Erreur : unite non reconnues : ",
        paste(unique(unite.invalides$unite), collapse = ", ")
      )}

caractere.invalides <- traits.gr.long.incomplet %>%
        dplyr::filter(!caractere %in% caractere.list$caractere)
      
      if (nrow(caractere.invalides) > 0) {
        cat(
          "Erreur : caractere non reconnues : ",
          paste(unique(caractere.invalides$caractere), collapse = ", ")
        )}      
  
statistique.invalides <- traits.gr.long.incomplet %>%
  dplyr::filter(!statistique %in% statistique.list$statistique)

if (nrow(statistique.invalides) > 0) {
  cat(
    "Erreur : statistique non reconnues : ",
    paste(unique(statistique.invalides$statistique), collapse = ", ")
  )}



# La dernière ligne de Versions fournit la date et le numéro de version associés à toutes
# les observations du formulaire.
version <- readODS::read_ods(fichier,sheet = "Versions") 

#tous les rédacteurs
redacteurs<-version[version$Rôle%like%'Rédacteur',]$Responsable%>%unique()%>%
            paste0(collapse=", ")

#tous les réviseurs
reviseurs<-if(nrow(version[version$Rôle%like%'Réviseur',])==0){'Aucune révision'}else{
  version[version$Rôle%like%'Réviseurs',]$Responsable%>%unique()%>%
  paste0(collapse=", ")}

#  dplyr::slice_tail(n = 1)
# Ajouter les métadonnées à chaque ligne
traits.gr.long <- traits.gr.long.incomplet%>%
  dplyr::mutate(
    redacteurs = redacteurs, reviseurs=reviseurs,
    date_version = version%>%dplyr::slice_tail(n=1)%>%pull(Date)%>%as.Date(),
    version = version%>%dplyr::slice_tail(n=1)%>%pull(Version)
  )


})

#Compilation de toutes les valeurs ensembles
traits.compile<- dplyr::bind_rows(traits.serie)%>%data.table()

# Correction spécifique aux fichiers: les lignes avec structure « feuille » et caractère « forme »
# sont renommées en structure « limbe » dans la compilation finale.
traits.compile[structure=='feuille'&caractere=='forme',structure:='limbe']

View(traits.compile)

#valide que tous les taxons sont dans le fichier VASCAN : ajouts potentiels

traits.compile[!scientificNameSimple%in%taxons_prj$scientificNameSimple,]


# ==============================================================================
# 3. Exportation en fichiers CSV#####
# ==============================================================================

# Exporte deux fichiers CSV datés dans output data. Les contrôles de vocabulaire ci-dessus
# affichent les valeurs inconnues dans la console sans interrompre automatiquement l'exécution.
write.csv(structures.add,
          paste0("output data/Structures_ajoutes_manuellement", Sys.Date(), ".csv"))

write.csv(traits.compile,
          paste0("output data/Traits_compilation", Sys.Date(), ".csv"))


