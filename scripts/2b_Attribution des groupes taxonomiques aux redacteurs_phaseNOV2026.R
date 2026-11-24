# Attribution des groupes taxonomiques aux rédacteurs - phase Novembre 2026
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

attribution_sel <- as.data.table(read.table("output data/attribution_regroupements_2026-10-22.txt"))
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
# A. Importation des proposition de taxons par les rédacteurs retraite de rédaction - et approbation####
# ==============================================================================

attribution_sel_temp<-left_join(attribution_sel, taxons_prj%>%filter(!duplicated(groupe_taxo))%>%
                                  select(order, family, groupe_taxo))%>%
                                      mutate(Approbation_Comite_Date=as.Date(Approbation_Comite_Date),
                                             mise_a_jour_date=as.Date(mise_a_jour_date))

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

#choixTous[genre%in%doubles,]%>%arrange(genre)

choixTous[redacteur=='Gravel Patrick'&genre=='Crocanthemum', assignation := 'approbation - étape 1']
choixTous[redacteur=='Gravel Patrick'&genre=='Ulmus', assignation := 'en attente approbation']
choixTous[redacteur=='Gravel Patrick'&genre%in%c('Aralia', 'Panax', 'Acer'), assignation := "refus - assigné à Séléna Bergeron"]
choixTous[redacteur=='Gravel Patrick'&genre%in%c('Acer'), assignation := "refus - assigné à Alexandre Bergeron"]

choixTous[redacteur=='Gravel Patrick'&genre%in%c('Elymnus'), genre := 'Elymus']#genre à corriger
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
         scientificName,scientificNameSimple)

choixTous[redacteur=='Vallée, Marc-Aurèle',]
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Alnus'), assignation := 'approbation - étape 1']
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Eleocharis', 'Viola'), assignation := 'approbation - étape 2']
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Botrychium', 'Sceptridium', 'Poa', 'Epilobium', 'Crataegus'), assignation := 'approbation - étape 3']#'Sceptrydium',
choixTous[redacteur=='Vallée, Marc-Aurèle'&genre%in%c('Salix'), assignation := 'refus - Jacques Brisson à confirmer intérêt']


choixTous[redacteur=='Higgins, Kellina',]$assignation<-'soutien bioinformatique'
choixTous[redacteur=='Étienne Léveillé-Bourret',]$assignation<-'soutien technique'
choixTous[redacteur=='Dupont, Patrice',]$assignation<-'soutien bibliographique'

choixTous[redacteur=='Higgins, Kellina',]$genre<-'Persicaria'
choixTous[redacteur=='Dupont, Patrice',]$genre<-'soutien bibliographique'


# ==============================================================================
# B. Exportation de la sélection des taxons pour un rédacteur####
# ==============================================================================

redacteur_family = 'Osmundaceae'
redacteur_genus = 'Cicuta'
redacteur_family = 'Araceae'
redacteur_family = 'Fabaceae'

taxons_prj[family==redacteur_family&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple)

redacteur_genus='Trillium'
taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple)

redacteur_genus = 'Monotropa'
regroupement_cle_choix<-taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%pull(regroupement_cle)
taxons_prj[regroupement_cle%in%regroupement_cle_choix&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, family, genus,
         scientificName,scientificNameSimple)


redacteur_genus = 'Pyrola'
regroupement_cle_choix<-taxons_prj[genus==redacteur_genus&territoire!='omettre',]%>%pull(regroupement_cle)
taxons_prj[regroupement_cle%in%regroupement_cle_choix&territoire!='omettre',]%>%
  select(taxonID, dist, source, territoire, Priorisation, regroupement_cle, Priorisation, family, genus,
         scientificName,scientificNameSimple)


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
#saveWorkbook(
#  wb,
#  "output data archivage/Selection_pour_redacteurs_Retraite2026nov.xlsx",
#  overwrite = TRUE
#)

# ==============================================================================
# C. Mise à jour fichier attribution des groupes taxonomiques - retraite rédaction Nov2026 + autres mises à jour ####
# ==============================================================================

choixRedacteursRetraiteNov<-choixTous%>%filter(assignation%like%'approbation - étape', !genre%in%c('Araceae', 'Fabaceae'))%>%
  rename("groupe_taxo"="genre", "Interet_Redacteur"="redacteur", "état"="assignation")

choixRedacteursRetraiteNov[duplicated(groupe_taxo),]
choixRedacteursRetraiteNov[groupe_taxo=='Eleocharis', Interet_Redacteur:='Pesant, Simon et Vallée, Marc-Aurèle']
choixRedacteursRetraiteNov[groupe_taxo=='Viola', Interet_Redacteur:='Étienne Lacroix-Carignan et Vallée, Marc-Aurèle']
choixRedacteursRetraiteNov[groupe_taxo=='Crataegus', Interet_Redacteur:='Marc-Aurèle Vallée, Étienne Léveillé-Bourret et Étienne Lacroix-Carignan']
choixRedacteursRetraiteNov<-choixRedacteursRetraiteNov[!duplicated(groupe_taxo)]
choixRedacteursRetraiteNov$Approbation_Comite_Date<-as.Date('2026-11-05')
choixRedacteursRetraiteNov$mise_a_jour_date<-as.Date('2026-11-05')


#attribution_sel[groupe_taxo%in%choixRedacteursRetraiteNov$groupe_taxo,]# OK - pas de perte d'information

  #attribution_sel_ajout_auteurs_Retraite <- copy(attribution_sel_temp)

attribution_sel_temp<-rows_update(
  attribution_sel_temp,
  choixRedacteursRetraiteNov,
  by = "groupe_taxo",
  unmatched = "ignore"
)

#Arold Lavoie - échanges par courriel
attribution_sel_temp[attribution_sel_temp[["order"]] %in% 'Ranunculales', `:=`(Interet_Redacteur='Arold Lavoie',
                                                                               Approbation_Comite_Date=as.Date('2026-11-10'),
                                                                               mise_a_jour_date=as.Date('2026-11-10'))]

attribution_sel_temp[family %like% 'Berberidaceae', état:='approbation - étape 1']
attribution_sel_temp[family %like% 'Papaveraceae', état:='approbation - étape 2']
attribution_sel_temp[family %like% 'Ranunculaceae', état:='approbation - étape 3']
attribution_sel_temp[family %like% 'Menispermaceae', état:='approbation - étape 1']
attribution_sel_temp[family %like% 'Menispermaceae', Interet_Redacteur:='Arold Lavoie et Kellina Higgins']


#Simon Joly - discussion verbale
attribution_sel_temp[groupe_taxo=='Rosa', `:=`(Interet_Redacteur='Simon Joly et Stéphane Bailleul',
                                         état='approbation - étape 2',
                                         Approbation_Comite_Date=as.Date('2026-11-10'),
                                         mise_a_jour_date=as.Date('2026-11-10'))]

attribution_sel_temp[groupe_taxo=='Impatiens', `:=`(Interet_Redacteur='Simon Joly',
                                         état='approbation - étape 1',
                                         Approbation_Comite_Date=as.Date('2026-11-10'),
                                         mise_a_jour_date=as.Date('2026-11-10'))]

#Anne Bruneau
attribution_sel_temp[family=='Fabaceae'&is.na(Interet_Redacteur), `:=`(Interet_Redacteur='Bruneau, Anne',
                                                    état='approbation - étape 3',
                                                    Approbation_Comite_Date=as.Date('2026-11-05'),
                                                    mise_a_jour_date=as.Date('2026-11-05'))]

#Alain Cuerrier
attribution_sel_temp[family=='Araceae'&is.na(Interet_Redacteur), `:=`(Interet_Redacteur='Cuerrier, Alain',
                                                                       état='approbation - étape 5',
                                                                       Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                       mise_a_jour_date=as.Date('2026-11-05'))]
#Luc Brouillet
attribution_sel_temp[groupe_taxo=='Oclemena', `:=`(Interet_Redacteur='Luc Brouillet',
                                                                      état='approbation - étape 1',
                                                                      Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                      mise_a_jour_date=as.Date('2026-11-05'))]

#autres proches de Schoenoplectiella
#attribution_sel_temp[groupe_taxo%in%c('Bolboschoenus', 'Blysmus', 'Trichophorum'), `:=`(Interet_Redacteur='Pesant, Simon',
#                                                   état='approbation - étape 4',
#                                                   Approbation_Comite_Date=as.Date('2026-11-05'),
#                                                   mise_a_jour_date=as.Date('2026-11-05'))]

#traitements en cours à Séléna
attribution_sel_temp[groupe_taxo%in%c('Calystegia', 'Convolvulus', 'Claytonia'), `:=`(Interet_Redacteur='Séléna Bergeron',
                                                   état='rédaction en cours',
                                                   Approbation_Comite_Date=as.Date('2026-10-15'),
                                                   mise_a_jour_date=as.Date('2026-11-11'))]

attribution_sel_temp[groupe_taxo%in%c('Lysimachia', 'Aralia'), `:=`(Interet_Redacteur='Séléna Bergeron',
                                                                                      #état='rédaction complétée',
                                                                                      Approbation_Comite_Date=as.Date('2026-10-15'))]

attribution_sel_temp[groupe_taxo%in%c('Panax', 'Allium'), `:=`(Interet_Redacteur='Séléna Bergeron',
                                                                    état='rédaction en cours',
                                                                    Approbation_Comite_Date=as.Date('2026-10-15'),
                                                                    mise_a_jour_date=as.Date('2026-11-09'))]


attribution_sel_temp[groupe_taxo%in%c('Maianthemum', 'Streptopus'), `:=`(Interet_Redacteur='Kellina Higgins',
                                                               #état='rédaction complétée',
                                                               Approbation_Comite_Date=as.Date('2026-07-15'),
                                                               mise_a_jour_date=as.Date('2026-08-15'))]

attribution_sel_temp[groupe_taxo%in%c('Clintonia'), `:=`(Interet_Redacteur='Martine Lapointe et Kellina Higgins',
                                                               état='rédaction complétée',
                                                               Approbation_Comite_Date=as.Date('2026-07-15'),
                                                               mise_a_jour_date=as.Date('2026-11-09'))]


attribution_sel_temp[groupe_taxo%in%c("Asparagus", "Convallaria", 
                                      "Hosta", "Hyacinthoides", "Ornithogalum", 
                                      "Scilla"), `:=`(Proposition_Responsable='Martine Lapointe et Julie Boudreau',
                                                      #état='rédaction complétée',
                                                      #Approbation_Comite_Date=as.Date('2026-07-15'),
                                                      mise_a_jour_date=as.Date('2026-11-11'))]

#Alexandre Bergeron - séléction 
attribution_sel_temp[Interet_Redacteur=='Alexandre Bergeron'&sous_groupe=='Fougères', `:=`(Approbation_Comite_Date=as.Date('2026-07-15'),
                                                                                        mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[Interet_Redacteur=='Alexandre Bergeron'&family=='Anacardiaceae', `:=`(Approbation_Comite_Date=as.Date('2026-07-15'),
                                                                                           mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[family=='Rhamnaceae', `:=`(Proposition_Responsable='Alexandre Bergeron',
                                                                                           mise_a_jour_date=as.Date('2026-11-09'))]


attribution_sel_temp[family=='Ophioglossaceae'&is.na(Interet_Redacteur), `:=`(Interet_Redacteur='Marc-Aurèle Vallée',
                                                                          état='approbation - rédaction éventuelle',
                                                                          Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                          mise_a_jour_date=as.Date('2026-11-05'))]

attribution_sel_temp[family=='Orchidaceae'&is.na(Interet_Redacteur), `:=`(Interet_Redacteur='Benoît Dorion et Frédérick Létourneau',
                                                                          état='approbation - rédaction éventuelle',
                                                                          Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                          mise_a_jour_date=as.Date('2026-11-05'))]


attribution_sel_temp[groupe_taxo=='Pyrola',                               `:=`(Interet_Redacteur='Iris Duvivier',
                                                                                   état='approbation - étape 2',
                                                                                   Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                                   mise_a_jour_date=as.Date('2026-11-05'))]

attribution_sel_temp[groupe_taxo=='Gaylussacia',                               `:=`(Interet_Redacteur='Iris Duvivier',
                                                                               état='approbation - étape 3',
                                                                               Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                               mise_a_jour_date=as.Date('2026-11-05'))]

attribution_sel_temp[sous_groupe=='Ericaceae sensu lato'&
                       is.na(Interet_Redacteur)&!Priorisation%like%'omplexe', `:=`(Interet_Redacteur='Iris Duvivier et Julie Boudreau',
                                                                              état='approbation - rédaction éventuelle - vérifie si complexes',
                                                                              Approbation_Comite_Date=as.Date('2026-11-05'),
                                                                              mise_a_jour_date=as.Date('2026-11-05'))]


attribution_sel_temp[groupe_taxo%in%c('Amelanchier', 'Hypericum'), `:=`(Interet_Redacteur='Étienne Lacroix-Carignan',
                                                                        état='approbation - rédaction éventuelle',
                                                                        Approbation_Comite_Date=as.Date('2026-07-15'),
                                                                        mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[groupe_taxo%in%c('Abies', 'Cephalanthus',
                                      'Selaginella', 'Thymus', 'Vinca'),`:=`(Interet_Redacteur='Étienne Lacroix-Carignan',
                                                    état='approbation - rédaction dans site FQ',
                                                    Approbation_Comite_Date=as.Date('2026-10-15'),
                                                    mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[groupe_taxo%in%c('Antennaria', 'Cornus', 'Diphasiastrum'),`:=`(Interet_Redacteur='Étienne Lacroix-Carignan',
                                                                             état='discuter avec comité éditorial - rédaction partielle dans site FQ',
                                                                             #Approbation_Comite_Date=as.Date('2026-10-15'),
                                                                             mise_a_jour_date=as.Date('2026-11-09'))]
attribution_sel_temp[groupe_taxo%in%c('Celtis', 'Fagus', 'Larix', 'Pinus',
                                      'Taxus', 'Thuja','Tsuga', 'Picea'),`:=`(Interet_Redacteur='Anne-Marie Blancette',
                                                    état='approbation - rédaction dans site FQ',
                                                    Approbation_Comite_Date=as.Date('2026-10-15'),
                                                    mise_a_jour_date=as.Date('2026-11-09'))]
attribution_sel_temp[groupe_taxo%in%c('Populus'),`:=`(Interet_Redacteur='Anne-Marie Blancette',
                                                      état='discuter avec comité éditorial - rédaction dans site FQ',
                                                   #Approbation_Comite_Date=as.Date('2026-07-15'),
                                                    mise_a_jour_date=as.Date('2026-11-09'))]
attribution_sel_temp[groupe_taxo%in%c('Carex sect. Phacocystis', 
                                      'Dactylis',  'Sphenopholis'),`:=`(Interet_Redacteur='François Rousseu',
                                                                              état='approbation - rédaction dans site FQ',
                                                                              Approbation_Comite_Date=as.Date('2026-10-15'),
                                                                              mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[groupe_taxo%in%c('Hydrogalium', 'Trichogalium'), `:=`(Interet_Redacteur='Andrée Thériault',
                                                                        état='approbation - rédaction éventuelle',
                                                                        Approbation_Comite_Date=as.Date('2026-07-15'),
                                                                        Priorisation='en attente FNA',
                                                                        mise_a_jour_date=as.Date('2026-11-09'))]

attribution_sel_temp[!is.na(Proposition_Responsable)&is.na(Approbation_Comite_Date)&!Proposition_Responsable%like%'Léveillé'&!Proposition_Responsable%like%'Alexandre Bergeron',]
attribution_sel_temp[is.na(Proposition_Responsable)&is.na(Interet_Redacteur)&is.na(Approbation_Comite_Date)&
                       groupe_principal%like%'Angiospermes'&
                       !Priorisation%like%'Complexe'&!Priorisation%like%'attente FNA',]%>%select(groupe_taxo:Priorisation, order, family)

angiospermes_responsables<-attribution_sel_temp[is.na(Proposition_Responsable)&is.na(Interet_Redacteur)&is.na(Approbation_Comite_Date)&
                       groupe_principal%like%'Angiospermes'&
                       !Priorisation%like%'Complexe'&!Priorisation%like%'attente FNA',]%>%select(groupe_taxo:Priorisation, order, family)

#correction dans les groupe_principal et sous_groupe
attribution_sel_temp[groupe_taxo%in%c('Aronia', 'Physocarpus', 'Spiraea'), sous_groupe:="Arbres et arbustes feuillus"]
attribution_sel_temp[groupe_taxo%in%c('Parnassia'), `:=`(sous_groupe="Autres dicotylédones herbacées terrestres",
                                                         groupe_principal="Dicotylédones herbacées terrestres")]

write.csv(left_join(angiospermes_responsables, gr_taxo%>%select(groupe_taxo, nb.taxa))%>%filter(nb.taxa>1&nb.taxa<6)%>%arrange(family),
          "output data archivage/Angiospermes_sans_redacteurs_2_a_5_taxa.csv")


# ==============================================================================
# 4. Regroupement de genres pour les clés ####
# ==============================================================================

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
            paste0("output data/attribution_regroupements_2026-11-11.txt"))

# ==============================================================================
# D. Ajouts - sélections par rédacteurs (phase mi à fin novembre) ####
# ==============================================================================

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


write.table(attribution_sel_temp%>%
              select(groupe_taxo, groupe_principal, sous_groupe, regroupement_cle, Priorisation,
                     Proposition_Responsable, Interet_Redacteur, Approbation_Comite_Date, `état`, `mise_a_jour_date`),
            paste0("output data/attribution_regroupements_2026-11-24.txt"))



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

