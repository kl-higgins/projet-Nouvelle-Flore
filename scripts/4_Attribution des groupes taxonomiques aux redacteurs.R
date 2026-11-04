# Attribution des groupes taxonomiques aux rédacteurs
# ====================================================================================
# Sections préalables:
#   1. Chargement et préparation des données VASCAN
#   2. Regroupement des taxons par groupe taxonomique
#   3. Intégration des statuts provinciaux EMVS
#   4. Exportation des taxons par regroupement_cle pour les rédacteurs
####DÉBUT script#####
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
# A. Importation des proposition de taxons par les rédacteurs - et approbation####
# ==============================================================================


choixInitial<-readxl::read_xlsx("input data/retraites redaction/RetraiteProjetNouvelleFloreNovembre 2026_Redacteurs.xlsx",
                                sheet="Form Responses 1")%>%as.data.table()%>%
  rename("redacteur"="Votre nom complet (nom, prénom)")

choixTous<-pivot_longer(choixInitial%>%select(-`Timestamp`,
                                              -`Votre courriel`,
                                              -`Affiliation (non-obligatoire)`,
                                              -`Proposition de genres d'intérêt (séparés par des virgules)`,
                                              -`Commentaires sur le choix des genres (ordre de préférence, partenaire de travail, ...)`,
                                              -`Proposition/Sélection finale`),
                        cols=matches("^(genre)\\d+$"),
                        names_to=NULL, values_to="genre")%>%filter(!is.na(genre))%>%as.data.table()
choixTous$assignation<-as.character()
doubles<-choixTous[,
                   list(binomes=length(redacteur)),
                   by=.(genre)]%>%filter(binomes>1)%>%
  pull(genre)

choixTous[genre%in%doubles,]%>%arrange(genre)


choixTous[redacteur=='Gravel Patrick'&genre=='Crocanthemum', assignation := 'approbation - étape 1']
choixTous[redacteur=='Gravel Patrick'&genre=='Ulmus', assignation := 'en attente approbation']
choixTous[redacteur=='Gravel Patrick'&genre%in%c('Aralia', 'Panax', 'Acer'), assignation := "refus - assigné à Séléna Bergeron"]
choixTous[redacteur=='Gravel Patrick'&genre%in%c('Acer'), assignation := "refus - assigné à Alexandre Bergeron"]

choixTous[redacteur=='Gravel Patrick'&genre%in%c('Elymnus'), genre := 'Elymus']#Agrimonium à corriger
choixTous[redacteur=='Gravel Patrick'&genre%in%c('Elymus'), assignation := "refus - assigné à François Rousseu"]
choixTous[redacteur=='Gravel Patrick',]

choixTous[redacteur=='Martine Lapointe'&genre%in%c('Trillium', 'Trillidium'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Martine Lapointe'&genre%in%c('Uvularia', 'Lilium'), assignation := 'approbation - étape 2'] # autre Liliacées
choixTous[redacteur=='Martine Lapointe'&genre%in%c('Erythronium', 'Medeola', 'Anticlea', 'Smilax', 'Veratrum'), assignation := 'approbation - étape 3'] # autre Liliacées
choixTous[redacteur=='Martine Lapointe'&genre%in%c('Smilax'), assignation := 'en attente - plusieurs taxons en Ontario'] # autre Liliacées

choixTous[redacteur=='Martine Lapointe'&genre%in%c('Allium'), assignation := 'refus - assigné à Séléna Bergeron'] # autres personnes
choixTous[redacteur=='Martine Lapointe'&genre%in%c('Polygonatum'), assignation := 'refus - assigné à Étienne Lacroix-Carignan'] # autres personnes
choixTous[redacteur=='Martine Lapointe'&genre%in%c('Clintonia'), assignation := 'refus - assigné à Kellina Higgins'] # autres personnes

choixTous[redacteur=='Martine Lapointe',]

choixTous[redacteur=='Lachance, Audrey'&genre=='Fraxinus', assignation := 'approbation - étape 1']

choixTous[redacteur=='Duvivier, Iris'&genre=='Kalmia', assignation := 'approbation - étape 1']
choixTous[redacteur=='Duvivier, Iris'&genre%in%c('Drosera', 'Pyrola', 'Iris'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Duvivier, Iris'&genre%in%c('Trichophorum', 'Rhynchospora', 'Scirpus', 'Rhododendron'), assignation := 'en attente approbation']
choixTous[redacteur=='Duvivier, Iris'&genre%in%c('Rhododendron'), assignation := 'refus - assigné à Alain Cuerrier']
choixTous[redacteur=='Duvivier, Iris'&genre%in%c('Vaccinium'), assignation := 'refus - genre très compliqué']
choixTous[redacteur=='Duvivier, Iris',]


choixTous[redacteur=='Étienne Lacroix-Carignan'&genre=='Polygonatum', assignation := 'approbation - étape 1']
choixTous[redacteur=='Étienne Lacroix-Carignan'&genre%in%c('Thymus'), assignation := 'en attente FNA']
choixTous[redacteur=='Étienne Lacroix-Carignan'&genre%in%c('Fraxinus'), assignation := 'refus - assigné à Andrey Lachance']
choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = rep("Étienne Lacroix-Carignan", 2),
    genre = c('Viola', 'Equisetum'),
    assignation = rep("approbation - étape 2", 2)
  ),
  fill = TRUE
)
choixTous[redacteur=='Étienne Lacroix-Carignan',]

regroupement_cle=c('Monotropa', 'Hypopitys')
choixTous[redacteur=='Frédéric Coursol'&genre%in%c('Monotropa', 'Hypopitys'), assignation:='approbation - étape 1']

choixTous[redacteur=='Frédéric Coursol'&genre%in%c('Pterospora'), assignation:='approbation - étape 2']
choixTous[redacteur=='Frédéric Coursol',]

choixTous[redacteur=='Bergeron, Alexandre',]
choixTous[redacteur=='Bergeron, Alexandre'&genre%in%c('Osmundaceae', 'Onocleaceae'), assignation := 'approbation - étape 1']

choixTous[redacteur=='Séléna Bergeron',]
choixTous[redacteur=='Séléna Bergeron'&genre%in%c('Claytonia', 'Panax', 'Allium'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Séléna Bergeron'&genre%in%c('Hydrophyllum'), assignation := 'en attente FNA']


choixTous[redacteur=='Villeneuve, Thomas',]
choixTous[redacteur=='Villeneuve, Thomas'&genre=='Cicuta', assignation := 'approbation - étape 1']
choixTous[redacteur=='Villeneuve, Thomas'&genre%in%c('Vincetoxicum', 'Apocynum', 'Reynoutria', 'Celastrus'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Villeneuve, Thomas'&genre%in%c('Vaccinium'), assignation := 'refus - genre très compliqué']
choixTous[redacteur=='Villeneuve, Thomas'&genre=='Lobelia', assignation := 'en attente FNA']

choixTous[redacteur=='Frédérick Létourneau',]
choixTous[redacteur=='Frédérick Létourneau'&genre%in%c('Galearis'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Frédérick Létourneau'&genre%in%c('Platanthera', 'Cypripedium', 'Spiranthes', 'Corallorhiza'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Frédérick Létourneau'&genre%in%c('Goodyera'), assignation := 'refus - assigné à Benoît Dorion']


choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = rep("Benoît Dorion"),
    genre = c('Goodyera'),
    assignation = rep("approbation - étape 1")
  ),
  fill = TRUE
)

choixTous[redacteur=='Boudreau, Julie',]
choixTous[redacteur=='Boudreau, Julie'&genre%in%c('Gaultheria'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Boudreau, Julie'&genre%in%c('Andromeda', 'Chamaedaphne', 'Epigaea'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Boudreau, Julie'&genre%in%c('Uvularia'), assignation := 'refus - assigné à Martine Lapointe']


choixTous[redacteur=='Bruneau, Anne',]
choixTous[redacteur=='Bruneau, Anne'&genre%in%c('Fabaceae'), assignation := 'approbation - étape 1']
choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = rep("Bruneau, Anne", 2),
    genre = c('Hedysarum', 'Melilotus'),
    assignation = rep("approbation - étape 1", 2)
  ),
  fill = TRUE
)
choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = rep("Bruneau, Anne", 2),
    genre = c('Lathyrus', 'Medicago'),
    assignation = rep("approbation - étape 2", 2)
  ),
  fill = TRUE
)

choixTous[redacteur=='François Rousseu',]
choixTous[redacteur=='François Rousseu'&genre%in%c('Cinna'), assignation := 'approbation - étape 1']
choixTous[redacteur=='François Rousseu'&genre%in%c('Alopecurus'), assignation := 'approbation - étape 2']
choixTous[redacteur=='François Rousseu'&genre%in%c('Sporobolus', 'Elymus',  'Eragrostis'), assignation := 'approbation - étape 3']

choixTous[redacteur=='Bailleul, Stéphane',]
choixTous[redacteur=='Bailleul, Stéphane'&genre%in%c('Agrimonium'), genre := 'Agrimonia']#Agrimonium à corriger
choixTous[redacteur=='Bailleul, Stéphane'&genre%in%c('Agrimonia', 'Sorbus'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Bailleul, Stéphane'&genre%in%c('Geum'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Bailleul, Stéphane'&genre%in%c('Rosa', 'Prunus'), assignation := 'approbation - étape 3']
choixTous[redacteur=='Bailleul, Stéphane'&genre%in%c('Rubus'), assignation := 'refus - à re-évaluer selon avancement']


choixTous[redacteur=='Cuerrier, Alain'&genre%in%c('Araceae'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Cuerrier, Alain'&genre%in%c('Primula', 'Rhododendron'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Cuerrier, Alain'&genre%in%c('Rhodiola'), assignation := 'approbation - étape 4']
choixTous[redacteur=='Cuerrier, Alain'&genre%in%c('Trientalis'), assignation := 'refus - assigné à Séléna Bergeron']
choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = rep("Cuerrier, Alain", 2),
    genre = c("Lemna", "Wolffia"),
    assignation = rep("approbation - étape 3", 2)
  ),
  fill = TRUE
)
choixTous <- rbind(
  choixTous,
  data.table(
    redacteur = "Cuerrier, Alain",
    genre = "Arisaema",
    assignation = "approbation - étape 1"
  ),
  fill = TRUE
)
choixTous[redacteur=='Cuerrier, Alain',]

choixTous[redacteur=='Pesant, Simon',]
choixTous[redacteur=='Pesant, Simon'&genre%in%c('Eriocaulon'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Pesant, Simon'&genre%in%c('Eleocharis'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Pesant, Simon'&genre%in%c('Schoenoplectiella',  'Zizania'), assignation := 'approbation - étape 3']
choixTous[redacteur=='Pesant, Simon'&genre%in%c('Lycopus'), assignation := 'en attente FNA']


redacteur_genus = 'Schoenoplectiella'
regroupement_cle_choix<-taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%pull(regroupement_cle)
taxons_prj[regroupement_cle%in%regroupement_cle_choix&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, Priorisation, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)

choixTous[redacteur=='Vallée, Marc-Aurèle',]
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Alnus'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Eleocharis', 'Viola'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Botrychium', 'Sceptrydium', 'Poa', 'Epilobium', 'Crataegus'), assignation := 'approbation - étape 3']#'Sceptrydium',
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Salix'), assignation := 'refus - Jacques Brisson à confirmer intérêt']


choixTous[redacteur=='Higgins, Kellina',]$assignation<-'soutien bioinformatique'
choixTous[redacteur=='Étienne Léveillé-Bourret',]$assignation<-'soutien technique'
choixTous[redacteur=='Dupont, Patrice',]$assignation<-'soutien bibliographique'

choixTous[redacteur=='Higgins, Kellina',]$genre<-'Persicaria'
choixTous[redacteur=='Dupont, Patrice',]$genre<-'soutien bibliographique'

View(choixTous)

# ==============================================================================
# B. Exportation de la sélection des taxons pour un rédacteur####
# ==============================================================================

redacteur_family = 'Osmundaceae'
redacteur_genus = 'Cicuta'
redacteur_family = 'Araceae'
redacteur_family = 'Fabaceae'

taxons_prj[family==redacteur_family&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)

redacteur_genus='Trillium'
taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)

redacteur_genus = 'Monotropa'
regroupement_cle_choix<-taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%pull(regroupement_cle)
taxons_prj[regroupement_cle%in%regroupement_cle_choix&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)


redacteur_genus = 'Pyrola'
regroupement_cle_choix<-taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%pull(regroupement_cle)
taxons_prj[regroupement_cle%in%regroupement_cle_choix&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, Priorisation, family, genus,
         scientificName,scientificNameSimple, `Statut provincial`)


genres_sel_retraite<-choixTous%>%filter(assignation%like%'approbation - étape', !genre%in%c('Araceae', 'Fabaceae'))%>%pull(genre)

taxons_prj[genus%in%genres_sel_retraite&is.na(regroupement_cle),regroupement_cle:=genus]
taxons_prj[genus%in%genres_sel_retraite,]%>%select(genus, regroupement_cle)%>%filter(!duplicated(genus))

taxons_sel_retraite<-left_join(taxons_prj[genus%in%genres_sel_retraite,]%>%
                                 select(taxonID, dist, territoire, family, regroupement_cle, genus,
                                        scientificNameSimple, scientificName),
                               choixTous%>%filter(assignation%like%'approbation - étape', !genre%in%c('Araceae', 'Fabaceae')), 
                               by=c("genus"="genre"))%>%arrange(redacteur, regroupement_cle, genus, dist, assignation)
taxons_sel_retraite<-taxons_sel_retraite%>%
  mutate(territoire=if_else(
    territoire=='QC inclu'&dist=='potentiel-QC',
        'QC potentiel',
       territoire))%>%select(-dist)%>%
        select(redacteur, assignation, taxonID:scientificName)
    

wb <- createWorkbook()

addWorksheet(wb, "choix_redacteur")
writeData(wb, "choix_redacteur", choixTous%>%arrange(redacteur, assignation))

addWorksheet(wb, "taxons_approbation")
writeData(wb, "taxons_approbation", taxons_sel_retraite)

# Ajuster automatiquement les largeurs
setColWidths(wb, "choix_redacteur", cols = 1:ncol(choixTous), widths = "auto")
setColWidths(wb, "taxons_approbation", cols = 1:ncol(taxons_sel_retraite), widths = "auto")

# Exporter
saveWorkbook(
  wb,
  "output data archivage/Selection_pour_redacteurs_Retraite2026nov.xlsx",
  overwrite = TRUE
)
