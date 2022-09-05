#######################################################################################
##################################RESAMPLING PROCEDURE#################################
####################SOURCE CODE FOR RESAMPLING SEMILANDMARKS IN ONE SPECIMEN###########
#######################################################################################

setwd("/Users/philosauria/Desktop/rev_turtles/CSV_files_landmarks") #set directory
library(rgl)
library(geomorph) 
library(Morpho)

#plot it in 3D to control the specimen
plot3d(turt1[,6], turt1[,7], turt1[,8], aspect="iso")

#isolate landmarks first
turtland <- turt1[1:10,] #first ten lines are landmarks

#isolate semilandmarks
turtsemiland <- turt1[11:nrow(turt1),] #semilandmarks for resampling

#write new documents with landmarks and semilandmarks
write.csv(turtsemiland, file = "turtsemiland.csv")

#open semilandmarks file
turt3 <- read.table(file = "turtsemiland.csv", header = F, sep=",", skip = 1)[1:nrow(turtsemiland),]

#split the different curves by names
splitsemi <- split(turt3, factor(turt3$V3, levels = unique(turt3$V3))) #delete duplicates
length(splitsemi) #control nb of curves

#give a list of the semilandmarks curves. In this dataset, we expect 12 of them
ls(splitsemi)

#REARRANGE SEMILANDMARKS#

#we erase the names of the colonnes
(unname(turtland, force = FALSE))

#creation of a matrix
turtland<- turtland[,6:8]
turtland <- as.matrix(turtland)

######RESAMPLING 12 CURVES########

#Curve1_ Left marginals
landm1<-data.frame(splitsemi$L_marginals[7:9]) #we isolate this curve
land1<-matrix(unlist(landm1), nrow(splitsemi$L_marginals), 3) 
semiland1<-digit.curves(land1[1,], land1, nPoints=38, closed = FALSE) #resampling of the curve
semiland1 #to control the number of semilandmarks

#Curve2_ Right marginals
landm2<-data.frame(splitsemi$R_marginals[7:9])
land2<-matrix(unlist(landm2), nrow(splitsemi$R_marginals), 3)
semiland2<-digit.curves(land2[1,], land2, nPoints=38, closed = FALSE)
semiland2

#Curve3_ Vertebrals
landm3<-data.frame(splitsemi$Vertebrals[7:9])
land3<-matrix(unlist(landm3), nrow(splitsemi$Vertebrals), 3)
semiland3<-digit.curves(land3[1,], land3, nPoints=28, closed = FALSE)
semiland3

#Curve4_ Left epi-hyoplastron
landm4<-data.frame(splitsemi$L_epi_hyo[7:9])
land4<-matrix(unlist(landm4), nrow(splitsemi$L_epi_hyo), 3)
semiland4<-digit.curves(land4[1,], land4, nPoints=13, closed = FALSE)
semiland4

#Curve5_ Left hyo-hypoplastron
landm5<-data.frame(splitsemi$L_hyo_hypo[7:9])
land5<-matrix(unlist(landm5), nrow(splitsemi$L_hyo_hypo), 3)
semiland5<-digit.curves(land5[1,], land5, nPoints=8, closed = FALSE)
semiland5

#Curve6_ Left hypo-xiphiplastron
landm6<-data.frame(splitsemi$L_hypo_xiphi[7:9])
land6<-matrix(unlist(landm6), nrow(splitsemi$L_hypo_xiphi), 3)
semiland6<-digit.curves(land6[1,], land6, nPoints=8, closed = FALSE)
semiland6

#Curve7_ Right epi-hyoplastron
landm7<-data.frame(splitsemi$R_hypo_xiphi[7:9])
land7<-matrix(unlist(landm7), nrow(splitsemi$R_hypo_xiphi), 3)
semiland7<-digit.curves(land7[1,], land7, nPoints=8, closed = FALSE)
semiland7

#Curve8_ Right hyo-hypoplastron
landm8<-data.frame(splitsemi$R_hyo_hypo[7:9])
land8<-matrix(unlist(landm8), nrow(splitsemi$R_hyo_hypo), 3)
semiland8<-digit.curves(land8[1,], land8, nPoints=8, closed = FALSE)
semiland8

#Curve9_ Right hypo-xiphiplastron
landm9<-data.frame(splitsemi$R_epi_hyo[7:9])
land9<-matrix(unlist(landm9), nrow(splitsemi$R_epi_hyo), 3)
semiland9<-digit.curves(land9[1,], land9, nPoints=13, closed = FALSE)
semiland9

#Curve10_ Length plastron
landm10<-data.frame(splitsemi$longueur_plastron[7:9])
land10<-matrix(unlist(landm10), nrow(splitsemi$longueur_plastron), 3)
semiland10<-digit.curves(land10[1,], land10, nPoints=28, closed = FALSE)
semiland10

#Curve11_ Width plastron
landm11<-data.frame(splitsemi$largeur_plastron[7:9])
land11<-matrix(unlist(landm11), nrow(splitsemi$largeur_plastron), 3)
semiland11<-digit.curves(land11[1,], land11, nPoints=13, closed = FALSE)
semiland11

#Curve12_ Width shell
landm12<-data.frame(splitsemi$largeur_shell[7:9])
land12<-matrix(unlist(landm12), nrow(splitsemi$largeur_shell), 3)
semiland12<-digit.curves(land12[1,], land12, nPoints=28, closed = FALSE)
semiland12

#plotting to control the curves
plot3d(semiland1[,1], semiland1[,2], semiland1[,3], aspect="iso")
plot3d(semiland2[,1], semiland2[,2], semiland2[,3], aspect="iso", add=TRUE)
plot3d(semiland3[,1], semiland3[,2], semiland3[,3], aspect="iso", add=TRUE)
plot3d(semiland4[,1], semiland4[,2], semiland4[,3], aspect="iso", add=TRUE)
plot3d(semiland5[,1], semiland5[,2], semiland5[,3], aspect="iso", add=TRUE)
plot3d(semiland6[,1], semiland6[,2], semiland6[,3], aspect="iso", add=TRUE)
plot3d(semiland7[,1], semiland7[,2], semiland7[,3], aspect="iso", add=TRUE)
plot3d(semiland8[,1], semiland8[,2], semiland8[,3], aspect="iso", add=TRUE)
plot3d(semiland9[,1], semiland9[,2], semiland9[,3], aspect="iso", add=TRUE)
plot3d(semiland10[,1], semiland10[,2], semiland10[,3], aspect="iso", add=TRUE)
plot3d(semiland11[,1], semiland11[,2], semiland11[,3], aspect="iso", add=TRUE)
plot3d(semiland12[,1], semiland12[,2], semiland12[,3], aspect="iso", add=TRUE)

#create a list with all the curves
finish <- bindArr(semiland1, semiland2,semiland3, semiland4,semiland5,semiland6,semiland7,semiland8,semiland9,semiland10,semiland11,semiland12, along=1)



######################################################################################
##################################GPA AND PCA ANALYSIS################################
######################################################################################
#call packages
library(rgl)
library(geomorph)
library(RRPP)
library(stats)
library(RColorBrewer)

#set directory
setwd("/Users/philosauria/Desktop/rev_turtles/PCA")

#call resampled landmarks
tt2d <-read.table(file = "turtles_SET1_resampled.csv", header = T, sep=",", skip = 0, fill = TRUE, row.names=1)#open the document with the 3D array composed of all the landmarks and semilandmarks data
tt3d <- arrayspecs(tt2d, 265, 3)


#open sliders file
cm<-as.matrix(read.table(file = "curves_slider_matrix.csv", header = F, sep=";", skip = 0, fill = TRUE))
	cm<- cm[-256:-340,-4:-8]

#open species list (in the same order as the dataset)
speclist<-c("Dermatemys_mawi", "Staurotypus_triporcatus", "Claudius_angustatus", "Sacalia_quadriocellata", "Mauremys_reevesii", "Apalone_mutica", "Pelusios_situatus", "Elseya_novaeguineae", "Chelydra_serpentina", "Carettochelys_insculpta", "Emys_orbicularis", "Pelomedusa_subrufa", "Psammobates_tentorius", "Homopus_femoralis", "Kinixys_belliana", "Rafetus_euphraticus", "Platysternon_megacephalum", "Pelodiscus_sinensis", "Stigmochelys_pardalis", "Dermochelys_coriacea", "Chelonoidis_carbonaria", "Rhinoclemmys_annulata", "Emydura_macquarii", "Astrochelys_radiata", "Pyxis_arachnoides", "Phrynops_tuberosus", "Podocnemis_vogli", "Lissemys_punctata", "Chelodina_oblonga", "Mesoclemmys_dahli", "Chersina_angulata", "Gopherus_polyphemus", "Sternotherus_odoratus", "Graptemys_geographica", "Clemmys_guttata", "Deirochelys_reticularia", "Malaclemys_terrapin", "Kinosternon_baurii", "Emydoidea_blandingii", "Eretmochelys_imbricata", "Geochelone_elegans", "Chelonia_mydas", "Actinemys_marmorata", "Terrapene_carolina", "Testudo_graeca", "Hydromedusa_tectifera", "Orlitia_borneensis", "Cuora_amboinensis", "Notochelys_platynota", "Cyclemys_dentata", "Batagur_dhongoka", "Vijayachelys_silvatica", "Melanochelys_trijuga", "Caretta_caretta", "Dogania_subplana", "Heosemys_spinosa", "Chelus_fimbriata", "Macrochelys_temminckii", "Malacochersus_tornieri", "Malayemys_subtrijuga", "Indotestudo_elongata", "Pangshura_tentoria", "Morenia_petersi", "Geoemyda_spengleri", "Manouria_impressa", "Platemys_platycephala", "Trachemys_scripta", "Glyptemys_insculpta", "Chrysemys_picta", "Proganochelys_quenstedti", "Proterochersis_robusta", "Plesiochelys_bigleri")

#open ecology list (in the same order as the dataset)
eco <- read.table(file = "ecology_web.csv", header = F, sep=";", skip = 0, fill = TRUE)
	eco <- unlist(eco, use.names=FALSE) #this step transform the dataset into "factors"


#creation of the dataset, composed of the landmarks files, curves sliders, and ecology categories
data.tt <- list("land" = tt3d, "curves" = cm, "ecology" = eco)
	str(data.tt) #check that everything is in order



		#if any, in which specimen data
		apply(is.na(data.tt$land), 3, which)

		#if any missing data: estimation of missing data using TPS or Reg methods
		data.tt$land <-estimate.missing(data.tt$land, method = c("TPS","Reg"))

######################################################################################
############################GENERALIZED PROCRUSTES ANALYSIS###########################
######################################################################################

#Generalized Procrustes WITH CURVES#

# Using bending energy for sliding
gpa.Y <-gpagen(data.tt$land, curves=data.tt$curves, Proj = TRUE, ProcD = FALSE)

pdf('outliers_SET1.pdf')
	plotOutliers(gpa.Y$coords) #check for extreme specimen and errors of data acquisition
dev.off()

summary(gpa.Y) #consensus shape


########Check specimens with problems######
#Plot meanshape (reference)
#ref<-mshape(gpa.Y$coords)
#plot reference to target and curves to compare
#plotRefToTarget(ref,gpa.Y$coords[,,2],method="points", links=cm) #compare one specimen to the mean shape


###################################################################################
###############################PRINCIPAL COMPONENT ANALYSIS########################
###################################################################################

#plot PCA results

		gp <- data.tt$ecology #grouping of the turtles by ecology categories
		col.gp <- brewer.pal(length(levels(gp)), "Set1")
		col.gp[6] <- "black"
   		names(col.gp) <- levels(gp)
	col.gp <- col.gp[match(gp, names(col.gp))] # col.gp must NOT be a factor

pca.lands <- gm.prcomp(gpa.Y$coords) #plot PCA and warpgrids

pdf('pca_SET1.pdf')
plot(pca.lands, pch = c(19, 17, 23, 15, 25, 20)[as.numeric(eco)], cex = 1.1, col = col.gp, bg=col.gp)

dev.off()	

pdf('pca_SET1_numbers.pdf')
		gp <- data.tt$ecology #grouping of the turtles by ecology categories
		col.gp <- brewer.pal(length(levels(gp)), "Set1")
		col.gp[6] <- "black"
   		names(col.gp) <- levels(gp)
	col.gp <- col.gp[match(gp, names(col.gp))] # col.gp must NOT be a factor

pca.lands <- gm.prcomp(gpa.Y$coords) #plot PCA and warpgrids


plot(pca.lands, pch = c(19, 17, 23, 15, 25, 22)[as.numeric(eco)], cex = 1.1, col = col.gp, bg=col.gp)
text(pca.lands$x[,1], pca.lands$x[,2],cex = 0.7, pos = 4, col = col.gp)

dev.off()	

#PC1min
plot3d(pca.lands$shapes$shapes.comp1$min, aspect="iso")
#PC1max
plot3d(pca.lands$shapes$shapes.comp1$max, aspect="iso")
#PC2min
plot3d(pca.lands$shapes$shapes.comp2$min, aspect="iso")
#PC2max
plot3d(pca.lands$shapes$shapes.comp2$max, aspect="iso")

######################################################################################
##################################HOW MUCH PCA DO WE KEEP ?###########################
######################################################################################

#http://rfunctions.blogspot.com/2015/01/pca-principal-component-analysis.html

pca.lands
summary(pca.lands)
loadings <-pca.lands$rotation	# The loadings (or eigenvectors) are simply our rotation values.

ev <- pca.lands$d #eigenvalues

####SOURCE :
# Plot eigenvalues and percentages of variation of an ordination object
# Kaiser rule and broken stick model
# Usage:
# evplot(ev)
# where ev is a vector of eigenvalues
 
# License: GPL-2 
# Author: Francois Gillet, 25 August 2012
 
evplot <- function(ev)
{
	# Broken stick model (MacArthur 1957)
	n <- length(ev)
	bsm <- data.frame(j=seq(1:n), p=0)
	bsm$p[1] <- 1/n
	for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
	bsm$p <- 100*bsm$p/n
	# Plot eigenvalues and % of variation for each axis
	op <- par(mfrow=c(2,1))
	barplot(ev, main="Eigenvalues", col="bisque", las=2)
	abline(h=mean(ev), col="red")
	legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
	barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
		main="% variation", col=c("bisque",2), las=2)
	legend("topright", c("% eigenvalue", "Broken stick model"), 
		pch=15, col=c("bisque",2), bty="n")
	par(op)
}

#export brokenstick plot
pdf('evplot_SET1.pdf')
evplot(ev)
dev.off()

#save the PC Data
write.csv(pca.lands$x, file = "pca_SET1.csv")
write.csv(pca.lands$d, file = "standard_dev_SET1.csv")

######################################################################################
############################LINEAR DISCRIMINANT ANALYSIS##############################
######################################################################################


setwd("/Users/philosauria/Desktop/rev_turtles/LDA")

#call packages
library(ape)
library(class)
library(geiger)
library(lattice)
library(mda)
library(nnet)
library(MASS)
library(RColorBrewer)

#open species list (in the same order as the dataset)
speclist<-c("Dermatemys_mawi", "Staurotypus_triporcatus", "Claudius_angustatus", "Sacalia_quadriocellata", "Mauremys_reevesii", "Apalone_mutica", "Pelusios_situatus", "Elseya_novaeguineae", "Chelydra_serpentina", "Carettochelys_insculpta", "Emys_orbicularis", "Pelomedusa_subrufa", "Psammobates_tentorius", "Homopus_femoralis", "Kinixys_belliana", "Rafetus_euphraticus", "Platysternon_megacephalum", "Pelodiscus_sinensis", "Stigmochelys_pardalis", "Dermochelys_coriacea", "Chelonoidis_carbonaria", "Rhinoclemmys_annulata", "Emydura_macquarii", "Astrochelys_radiata", "Pyxis_arachnoides", "Phrynops_tuberosus", "Podocnemis_vogli", "Lissemys_punctata", "Chelodina_oblonga", "Mesoclemmys_dahli", "Chersina_angulata", "Gopherus_polyphemus", "Sternotherus_odoratus", "Graptemys_geographica", "Clemmys_guttata", "Deirochelys_reticularia", "Malaclemys_terrapin", "Kinosternon_baurii", "Emydoidea_blandingii", "Eretmochelys_imbricata", "Geochelone_elegans", "Chelonia_mydas", "Actinemys_marmorata", "Terrapene_carolina", "Testudo_graeca", "Hydromedusa_tectifera", "Orlitia_borneensis", "Cuora_amboinensis", "Notochelys_platynota", "Cyclemys_dentata", "Batagur_dhongoka", "Vijayachelys_silvatica", "Melanochelys_trijuga", "Caretta_caretta", "Dogania_subplana", "Heosemys_spinosa", "Chelus_fimbriata", "Macrochelys_temminckii", "Malacochersus_tornieri", "Malayemys_subtrijuga", "Indotestudo_elongata", "Pangshura_tentoria", "Morenia_petersi", "Geoemyda_spengleri", "Manouria_impressa", "Platemys_platycephala", "Trachemys_scripta", "Glyptemys_insculpta", "Chrysemys_picta", "Proganochelys_quenstedti", "Proterochersis_robusta", "Plesiochelys_bigleri")


#read PC Data and ecology categories
#COR<-read.csv("pca_allturtles_alldata_MODIFIED.csv", header=TRUE, sep=",", row.names=1) #3CAT
COR<-read.csv("pca_SET1.csv", header=TRUE, sep=";",row.names=1)#5cat
rownames(COR) <- speclist

eco <- read.table(file = "ecology_web.csv", header = F, sep=";", skip = 0, fill = TRUE)

CORN <- row.names(COR)
names (CORN) <- row.names(COR)

gA <- eco 	#Grouping

############################
XA <- COR[,1:9] #PCA Data

############################
############################
#for FOSSIL data

testtaxa <- rownames(COR[gA=="UKN",]) # UNKNOWN = FOSSIL DATA
testtaxan <- row(COR)[gA=="UKN",1]
trainingtaxa <- rownames(COR[-testtaxan,])
X <- XA[-testtaxan,]
sf <- COR[-testtaxan,]
g <- gA[-testtaxan,]
g <- droplevels(g)
X2 <- XA[testtaxa,]


#TRAINING DATA

gp <- g #grouping of the turtles by ecology categories
		col.gp <- brewer.pal(length(levels(gp)), "Set1")
   		names(col.gp) <- levels(gp)
	col.gp <- col.gp[match(gp, names(col.gp))] # col.gp must NOT be a factor


t.lda <- lda(g ~ ., X)
t.lda.values  <- predict(t.lda)
ldahist(t.lda.values$x[,1], g = g)
confusion <- table(g, t.lda.values$class, dnn = c('Actual Group','Predicted Group'))


t.lda_CV <- lda(g ~ .,CV=TRUE, X) #using cross validation model to compare
confusion_CV <- table(g, t.lda_CV$class, dnn = c('Actual Group','Predicted Group'))


diag(prop.table(confusion_CV, 1))
diag(prop.table(confusion, 1)) #Rates should be the same to use the first model. 

#plot extant turtles

plot(t.lda.values$x[,1], t.lda.values$x[,2], pch = c(19, 17, 23, 15, 25)[as.numeric(g)], cex=1, bg=col.gp, col=col.gp)


#predictions on fossils

plda <- predict(object = t.lda, newdata = X2)

plda$class

lda_data <- rbind(t.lda.values$x, plda$x)

x<-as.data.frame(t.lda.values$class)
x[,1] <- trainingtaxa
x[,2] <- g
x[,3] <- as.data.frame(t.lda.values$class)
mat <- cbind(x, t.lda.values$x)
colnames(mat) <- c("species", "true class", "predicted class", "LD1", "LD2", "LD3", "LD4")
row.names(mat) <- NULL


# write tables
write.csv(lda_data, "SET1_LDA_LDA_VALUES_EXT_FOSS.csv")
write.csv(confusion, "SET1_LDA_Cross-classification-confusion matrix.csv")
write.csv(confusion_CV, "SET1_CV_LDA_Cross-classification-confusion_matrix.csv")
write.csv(plda$class, "SET1_LDA_prediction_matrix.csv")
write.csv(mat, "SET1_LDA_VALUES_LD_MATRIX.csv")

aa <- cbind(plda$class, plda$posterior, plda$x)
write.csv(aa, "SET1_PREDICTIONS_FOSSILS.csv")

pdf('LDA_SET1_EXTANT.pdf')
plot(t.lda.values$x[,1], t.lda.values$x[,2], pch = c(19, 17, 23, 15, 25)[as.numeric(g)], cex=1, bg=col.gp, col=col.gp)
dev.off()


g2 <- as.factor(c(g, "6", "6", "6"))
lda_data2 <- cbind(lda_data, g2)

col.gp2 <- brewer.pal(length(levels(g2)), "Set1")
col.gp2[6] <- "black"
   names(col.gp2) <- levels(g2)
col.gp2 <- col.gp2[match(g2, names(col.gp2))] # col.gp must NOT be a factor


pdf('LDA_SET1_fossils_numbers.pdf')
	plot(lda_data2[,1], lda_data2[,2], pch = c(21, 24, 23, 22, 25, 20)[as.numeric(g2)], cex= 1.5, bg=col.gp2, col="black")
	text(lda_data2[,1], lda_data2[,2], cex = 0.7, pos = 4, col = col.gp2)
dev.off()


pdf('LDA_SET1_fossils.pdf')
	plot(lda_data2[,1], lda_data2[,2], pch = c(21, 24, 23, 22, 25, 20)[as.numeric(g2)], cex= 1.5, bg=col.gp2, col="black")
	
dev.off()


#############################################
##########MEAN SHAPES#########################
#############################################
#############################################

eco <- read.table(file = "ecology_web.csv", header = F, sep=";", skip = 0, fill = TRUE)
d2_tt <- two.d.array(gpa.Y$coords)
dd_tt <- cbind (eco, d2_tt)
split_tt <- split(dd_tt, dd_tt[,1]) #split categories
length(split_tt) #control nb of curves


TER <- split_tt$TER
SET <- split_tt$SET
SEA <- split_tt$SEA
FUA <- split_tt$FUA
FME <- split_tt$FME
UKN <- split_tt$UKN

dTER <- arrayspecs(split_tt$TER[,2:796], 265, 3)
dSET <- arrayspecs(split_tt$SET[,2:796], 265, 3)
dSEA <- arrayspecs(split_tt$SEA[,2:796], 265, 3)
dFUA <- arrayspecs(split_tt$FUA[,2:796], 265, 3)
dFME <- arrayspecs(split_tt$FME[,2:796], 265, 3)
dUKN <- arrayspecs(split_tt$UKN[,2:796], 265, 3)


mTER <- mshape(dTER, na.action=1)
mSET <- mshape(dSET, na.action=3)
mSEA <- mshape(dSEA, na.action=3)
mFUA <- mshape(dFUA, na.action=3)
mFME <- mshape(dFME, na.action=3)

mfrow3d(3, 2)
plot3d(mTER[,1],mTER[,2],mTER[,3], asp=FALSE)
plot3d(mSET[,1],mSET[,2],mSET[,3], asp=FALSE)
plot3d(mSEA[,1],mSEA[,2],mSEA[,3], asp=FALSE)
plot3d(mFUA[,1],mFUA[,2],mFUA[,3], asp=FALSE)
plot3d(mFME[,1],mFME[,2],mFME[,3], asp=FALSE)




######################################################################################
##################pFDA  - Code adapted from MONTANI and SCHMITZ 2011##################
########################doi:10.1111/j.1558-5646.2011.01271.x##########################
######################################################################################

#call packages
library(ape)
library(class)
library(geiger)
library(lattice)
library(mda)
library(nnet)
library(RColorBrewer)

#set directory

setwd("/Users/philosauria/Desktop/rev_turtles/pFDA")

source("phylo.fda.v0.2.R")

speclist<-c("Dermatemys_mawi","Staurotypus_triporcatus","Claudius_angustatus","Sacalia_quadriocellata","Mauremys_reevesii","Apalone_mutica","Pelusios_sinuatus","Elseya_novaeguineae","Chelydra_serpentina","Carettochelys_insculpta","Emys_orbicularis","Pelomedusa_subrufa","Psammobates_tentorius","Homopus_femoralis","Kinixys_belliana","Rafetus_euphraticus","Platysternon_megacephalum","Pelodiscus_sinensis","Stigmochelys_pardalis","Dermochelys_coriacea","Chelonoidis_carbonaria","Rhinoclemmys_annulata","Emydura_macquarii","Astrochelys_radiata","Pyxis_arachnoides","Phrynops_tuberosus","Podocnemis_vogli","Lissemys_punctata","Chelodina_oblonga","Mesoclemmys_dahli","Chersina_angulata","Gopherus_polyphemus","Sternotherus_odoratus","Graptemys_geographica","Clemmys_guttata","Deirochelys_reticularia","Malaclemys_terrapin","Kinosternon_baurii","Emydoidea_blandingii","Eretmochelys_imbricata","Geochelone_elegans","Chelonia_mydas","Actinemys_marmorata","Terrapene_carolina","Testudo_graeca","Hydromedusa_tectifera","Orlitia_borneensis","Cuora_amboinensis","Notochelys_platynota","Cyclemys_dentata","Batagur_dhongoka","Vijayachelys_silvatica","Melanochelys_trijuga","Caretta_caretta","Dogania_subplana","Heosemys_spinosa","Chelus_fimbriata","Macrochelys_temminckii","Malacochersus_tornieri","Malayemys_subtrijuga","Indotestudo_elongata","Pangshura_tentoria","Morenia_petersi","Geoemyda_spengleri","Manouria_impressa","Platemys_platycephala","Trachemys_scripta","Glyptemys_insculpta","Chrysemys_picta","Proganochelys_quenstedti","Proterochersis_robusta","Plesiochelys_bigleri")

#read consensus tree
treA <- read.tree("trees_age_topo_turtles5_fossils.tre")

treA$tip.label
if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
is.ultrametric(treA) #should be FALSE

##########################
#read PC Data and ecology categories
COR<-read.csv("pca_SET1.csv", header=T, sep=";")
CORN <- COR$species
names (CORN) <- COR$species



CORorder<-COR[match(treA$tip.label, names(CORN)),]

ddA <- CORorder
taxaA <- ddA$species
rownames(ddA) <- taxaA

############################
gA <- ddA$ecology 	#Grouping



############################
XA <- ddA[,3:11] #PCA Data

############################
############################
#for FOSSIL data

testtaxa <- rownames(ddA[gA=="UKN",]) # UNKNOWN = FOSSIL DATA
testtaxan <- row(ddA)[gA=="UKN",1]
trainingtaxa <- rownames(ddA[-testtaxan,])
X <- XA[-testtaxan,]
dd <- ddA[-testtaxan,]
g <- gA[-testtaxan]
tre <- drop.tip(treA, testtaxa) 
is.ultrametric(tre)





############IF NOT BUT ALMOST###################
force.ultrametric<-function(tree,method=c("nnls","extend")){
    method<-method[1]
    if(method=="nnls") tree<-nnls.tree(cophenetic(tree),tree,
        rooted=TRUE,trace=0)
    else if(method=="extend"){
        h<-diag(vcv(tree))
        d<-max(h)-h
        ii<-sapply(1:Ntip(tree),function(x,y) which(y==x),
            y=tree$edge[,2])
        tree$edge.length[ii]<-tree$edge.length[ii]+d
    } else 
        cat("method not recognized: returning input tree\n\n")
    tree
}

library(phangorn)
tre<-force.ultrametric(tre) ## default method
is.ultrametric(tre) #should be TRUE

##########################
########LAMBDA############
##########################
filename_stem <- "HABITAT.optLambda"
ol1 <- optLambda(X,g,tre,idc=filename_stem) #UNKNOWN TAXA

ol1$optlambda	#Lambda value

######################################################################################
###############################PHYLOGENETIC DISCRIMINANT ANALYSIS#########################
######################################################################################

#SIMPLE
optl <- 0	#!!!!!!!!Replace with the optimal lambda value from above!!!!!!
pfda <- phylo.fda.pred(XA,gA,taxaA,treA,testtaxan,val=optl)

pfda$confusion

pfda$testprediction

#Save table
write.csv(pfda$confusion, "SET1_Cross-classification-confusion matrix.csv")
write.csv(pfda$testprediction, "SET1_prediction_matrix.csv")

################################
test.class <- as.character(predict(pfda, pfda$DATAtest, type="class"))
test.variates <- predict(pfda, pfda$DATAtest, type="variates")
test.prob <- predict(pfda, pfda$DATAtest, type="posterior")
test.results <- cbind(test.class, test.prob, test.variates)

#colnames(test.results)<- c("predicted class", "P(fins)", "P(notwebbed)", "P(redwebbed)", "P(paddle)","P(webbed)","DA1","DA2","DA3","DA4")

colnames(test.results)<- c("predicted class","FME", "FUA", "SEA", "SET", "TER","DA1","DA2","DA3","DA4")

rownames(test.results) <- testtaxa
head(test.results)

write.csv(test.results, "SET1_PREDICTIONS_VALUES.csv")

##################################
training.class <- as.character(predict(pfda, pfda$DATA, type="class"))
training.variates <- predict(pfda, pfda$DATA, type="variates")
training.prob <- predict(pfda, pfda$DATA, type="posterior")
training.results <- cbind(as.character(g), training.class, training.prob, training.variates)
colnames(training.results) <- c("true class", "predicted class", "P(fins)", "P(webbed)", "P(semiwebbed)","P(semiterrestrial)","P(terrestrial)","DA1","DA2","DA3","DA4")
rownames(training.results) <- trainingtaxa

#Save table
write.csv(training.results, "SET1_TURTLE_PROBABILITY_HABITAT.csv")

####################################
###PLOT
########
training <- as.data.frame(cbind(training.variates, as.character(g)))
colnames(training) <- c("DA1","DA2","DA3","DA4","groups")
rownames(training) <- rownames(dd)

####################################
unknown <- as.character(rep("UKN", times=length(testtaxan)))
test <- as.data.frame(cbind(test.variates, unknown))
colnames(test) <- c("DA1","DA2","DA3","DA4","groups")
rownames(test) <- testtaxa

##################################
#############################
scatter <- rbind(training, test) #REAL unknown taxa
write.csv(scatter,"scatter_SET1.csv")

scatter[, 1:2] <- lapply(scatter[,1:2], as.character)
scatter[, 1:2] <- lapply(scatter[,1:2], as.numeric)
######################

######################
GR1 <- scatter[scatter$groups=="TER",]
GR2 <- scatter[scatter$groups=="SET",]
GR3 <- scatter[scatter$groups=="SEA",]
GR4 <- scatter[scatter$groups=="FUA",]
GR5 <- scatter[scatter$groups=="FME",]
GR6 <- scatter[scatter$groups=="UKN",]

############################
pdf('SET1_pDFA_text.pdf')
plot(GR1$DA1, GR1$DA2,							#terrestrial
      asp=1,
      xlim=range(scatter[,1]),
      ylim=range(scatter[,2]),
      pch=25, col="black", bg="#ff7f00",
      cex=1.5, cex.lab=1.5,
      xlab="DA 1", ylab="DA 2"); 					

points(GR2$DA1, GR2$DA2, pch=22, cex=1.5, col="black", bg="#984ea3"); 	#semi-terrestrial
points(GR3$DA1, GR3$DA2, pch=23, cex=1.5, col="black", bg="#4daf4a");	#semi-aquatic
points(GR4$DA1, GR4$DA2, pch=24, cex=1.5, col="black", bg="#377eb8");	#fully aquatic
points(GR5$DA1, GR5$DA2, pch=21, cex=1.5, col="black", bg="#e41a1c");	#full marine environment
points(GR6$DA1, GR6$DA2, pch=20, cex=1.5, col="black", bg="black"); 	#fossils
box(lwd=2); axis(1, lwd=2, lwd.ticks=2); axis(2, lwd=2, lwd.ticks=2)



#Text
#text(GR1$DA1, GR1$DA2, labels=rownames(GR1), cex=0.9, font=2, pos=4)
#text(GR2$DA1, GR2$DA2, labels=rownames(GR2), cex=0.9, font=2, pos=4)
#text(GR3$DA1, GR3$DA2, labels=rownames(GR3), cex=0.9, font=2, pos=4)
#text(GR4$DA1, GR4$DA2, labels=rownames(GR4), cex=0.9, font=2, pos=4)
#text(GR5$DA1, GR5$DA2, labels=rownames(GR5), cex=0.9, font=2, pos=4)
text(GR6$DA1, GR6$DA2, labels=rownames(GR6), cex=0.9, font=2, pos=4)
dev.off()

pdf('SET1_pDFA.pdf')
plot(GR1$DA1, GR1$DA2,							#terrestrial
      asp=1,
      xlim=range(scatter[,1]),
      ylim=range(scatter[,2]),
      pch=25, col="black", bg="#ff7f00",
      cex=1.5, cex.lab=1.5,
      xlab="DA 1", ylab="DA 2"); 					

points(GR2$DA1, GR2$DA2, pch=22, cex=1.5, col="black", bg="#984ea3"); 	#semi-terrestrial
points(GR3$DA1, GR3$DA2, pch=23, cex=1.5, col="black", bg="#4daf4a");	#semi-aquatic
points(GR4$DA1, GR4$DA2, pch=24, cex=1.5, col="black", bg="#377eb8");	#fully aquatic
points(GR5$DA1, GR5$DA2, pch=21, cex=1.5, col="black", bg="#e41a1c");	#full marine environment
points(GR6$DA1, GR6$DA2, pch=20, cex=1.5, col="black", bg="black"); 	#fossils
box(lwd=2); axis(1, lwd=2, lwd.ticks=2); axis(2, lwd=2, lwd.ticks=2)



#Text
#text(GR1$DA1, GR1$DA2, labels=rownames(GR1), cex=0.9, font=2, pos=4)
#text(GR2$DA1, GR2$DA2, labels=rownames(GR2), cex=0.9, font=2, pos=4)
#text(GR3$DA1, GR3$DA2, labels=rownames(GR3), cex=0.9, font=2, pos=4)
#text(GR4$DA1, GR4$DA2, labels=rownames(GR4), cex=0.9, font=2, pos=4)
#text(GR5$DA1, GR5$DA2, labels=rownames(GR5), cex=0.9, font=2, pos=4)
#text(GR6$DA1, GR6$DA2, labels=rownames(GR6), cex=0.9, font=2, pos=4)

dev.off()
