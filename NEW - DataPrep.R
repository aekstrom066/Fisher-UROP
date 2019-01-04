#################################################################################################################
#
#     Preparing 3D landmark data for Geometric Morphometric Analysis
#     Aaron's UROP project on fisher skull morphology
#
#     By: Michael Joyce and Aaron Ekstrom
#     January 2018
#
#################################################################################################################

### Overview
#     Aaron digitized 2D stereo image pairs and used them to create a 3D model of 42 landmarks
#     This script file will take 3D landmark data for individual specimens and re-format the data
#     so that we have one file for analysis that has 3D landmarks for all specimens.

### Read Packages #####
library(StereoMorph)
library(rgl)



### Establish workflow for one specimen #####
# Read in 3D landmark text file
dat.3d<-readShapes('./Session 02/Unified Landmarks/6272/Shapes 3D/1.txt')

# Pull off Landmarks into data frame
dat.lm<-dat.3d$landmarks
colnames(dat.lm)<-c("X", "Y", "Z")



### Found a few issues: 3D points look off for a few and some are missing landmarks
### Aaron will go through each specimen and identify which have missing landmarks (and then correct those issues)
### Aaron will also go through and look at the 3D points for each specimen to see which appear to have erroneous points

## Code to read in 1.txt files and export landmark data as CSV files for Aaron to review for missing landmarks
folder.dat<-read.csv('Session_Specimen.csv') #read in data with session folder and specimen identifiers
folder.dat<-folder.dat[folder.dat$Specimen!=7142,]

for(i in 1:nrow(folder.dat)){
  #read in 1.txt file
  dat.i<-readShapes(paste('./', folder.dat[i,1], '/Unified Landmarks/', folder.dat[i,2], '/Shapes 3D/1.txt', sep=""))
  
  lm.i<-dat.i$landmarks
  colnames(lm.i)<-c("X", "Y", "Z")
  
  write.csv(lm.i, paste("./Specimen_landmarks/", folder.dat[i,2], ".csv", sep=""), na="")
}




## Code for viewing landmarks in 3D

# View landmarks
library(rgl)
dat.ex<-read.csv('./Specimen_landmarks/6051.csv')
plot3d(dat.ex$X, dat.ex$Y, dat.ex$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'red', size = 10)

dat.ex$col<-seq(1,nrow(dat.ex),1)
plot3d(dat.ex$X, dat.ex$Y, dat.ex$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'red', size = 4)
with(dat.ex,text3d(dat.ex$X, dat.ex$Y, dat.ex$Z,col))


dat2<-as.data.frame(dat.lm)
plot3d(dat2$X, dat2$Y, dat2$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'red', size = 10)

dat3<-as.data.frame(dat.lm.2)
plot3d(dat3$X, dat3$Y, dat3$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'blue', size = 10, add=T)



########################### Starting over with improved 3D landmark data ###########################


####################################################################################################

### Establish workflow for one specimen #####
# Read in vector of specimens with 3D data
numberOfSamples<-125

spec<-read.csv('./NEW Specimens_3D.csv')


# Read in 3D landmark text file
i<-1
dat.3d<-readShapes(paste('./UROP 3D Data/', spec[i,], '/Shapes 3D/1.txt', sep=""))

# Pull off Landmarks into data frame
dat.lm<-dat.3d$landmarks
colnames(dat.lm)<-c("X", "Y", "Z")

### Dont need these lines, same as "dat.lm" ###

#i<-3
#dat.3d.2<-readShapes(paste('./UROP 3D Data/', spec[i,], '/Shapes 3D/1.txt', sep=""))

# Pull off Landmarks into data frame
#dat.lm.2<-dat.3d.2$landmarks
#colnames(dat.lm.2)<-c("X", "Y", "Z")

# Code to read in 1.txt files and export landmark data as CSV files
for(i in 1:nrow(spec)){
  #read in 1.txt file
  dat.i<-readShapes(paste('./UROP 3D Data/', spec[i,], '/Shapes 3D/1.txt', sep=""))
  
  lm.i<-dat.i$landmarks
  colnames(lm.i)<-c("X", "Y", "Z")
  
  write.csv(lm.i, paste("./Specimen_landmarks/", spec[i,], ".csv", sep=""), na="")
}


# Re-organize landmark data
# Read in 3D landmark text file
spec<-as.vector(spec[,1])
i<-1
dat.3d<-readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))

# Pull off Landmarks into data frame
dat.lm<-dat.3d$landmarks
colnames(dat.lm)<-c("X", "Y", "Z")

#Create new data object to store reorganized data
dat.lm2<-matrix(NA, numberOfSamples, 126)  #create empty matrix to store data
nms<-read.csv('colnames_lm.csv', header=F)
nms<-as.vector(nms[,1])

colnames(dat.lm2)<-nms

#re-arrange landmark data to a single row
hilow<-matrix(NA,42,2)
hilow[,1]<-seq(1,124,3)
hilow[,2]<-seq(3,126,3)

for(i in 1:41){
  dat.lm2[1,hilow[i,1]:hilow[i,2]] <- dat.lm[i,]
}

###########################################################################################

### Develop Loop to process mutliple specimens at a time #####

#Re-make matrix to hold ID, sex, Age info

###ORIGINAL CODE####
# for(i in 1:length(spec)){
#  dat.3d<-readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))
#  
#  # Pull off Landmarks into data frame
#  dat.lm<-dat.3d$landmarks
#
#  for(j in 1:42){
#    dat.lm2[i,hilow[j,1]:hilow[j,2]]<-dat.lm[j,]
#  }
#  
#}
#####################

#### Aaron Edited Code ###
 for(i in 1:numberOfSamples){
  dat.3d <- readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))
  
  # Pull off Landmarks into data frame
  dat.lm <- dat.3d$landmarks

  for(j in 1:42){
    dat.lm2[i,hilow[j,1]:hilow[j,2]] <- dat.lm[j,]
  }
}

### Export TXT file of landmark data
write.csv(dat.lm2, "NEW Fisher_ontogeny_set2.csv", row.names=F)

### looked through csv file to identify potental issues with landmark number, coordinate system orientation, or order


## there are some alignment issues from different sessions
## we will try to use the function 'findOptimalPointAlignmen' in StereoMorph to fix these issues

#Try out function using 6080 and 6092
# Read in 3D landmark text file
#i<-1
#d1<-readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))

#i<-2
#d2<-readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))

#View before transformation
#dat1<-d1$landmarks
#colnames(dat1)<-c("X", "Y", "Z"); dat1<-as.data.frame(dat1)

#dat2<-d2$landmarks
#colnames(dat2)<-c("X", "Y", "Z"); dat2<-as.data.frame(dat2)

#plot3d(dat1$X, dat1$Y, dat1$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'red', size = 10, aspect = F)
#plot3d(dat2$X, dat2$Y, dat2$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'blue', size = 10, add=T)


#transform/align
#d3<-findOptimalPointAlignment(d1$landmarks, d2$landmarks)
## just wants matrix of landmarks x coorinates, not a list

#view after transformation
#dat3<-d3
#colnames(dat3)<-c("X", "Y", "Z"); dat3<-as.data.frame(dat3)

#plot3d(dat1$X, dat1$Y, dat1$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'red', size = 10, aspect = F)
#plot3d(dat3$X, dat3$Y, dat3$Z, xlab = 'X coord', ylab = 'Y coord', zlab = 'Z coord', axes = T, col = 'blue', size = 10, add=T)


#### Align all the datasets to 6052

# Step 1: read in reference data set (6052)
d1<-readShapes('./UROP 3D Data/6052/Shapes 3D/1.txt')
d1<-d1$landmarks  #save landmark data and drop other list elements
d1p<-as.data.frame(d1) #to use for plotting alignments
colnames(d1p)<-c("x", "y", "z")

# Step 2: Loop to re-align and export JPEGs of alignment for review
for(i in 2:numberOfSamples){
  d2<-readShapes(paste('./UROP 3D Data/', spec[i], '/Shapes 3D/1.txt', sep=""))
  d2<-d2$landmarks
  
  #re-align
  #d2b<-findOptimalPointAlignment(d1, d2)
  
  #Use Try Catch to find what file is causing errors
  d2b <- tryCatch({
    findOptimalPointAlignment(d1, d2)
  }, error = function(error_condition){
    print(i)
    next
  })
  
  #Export re-aligned landmarks
  write.csv(d2b, paste("./Specimen_landmarks/fixed/", spec[i], "_realigned.csv", sep=""), na="")
  
  #Export Images of alignment for diagnostics
  d2p<-as.data.frame(d2b)
  colnames(d2p)<-c("x", "y", "z")
  
  jpeg(paste("./Specimen_landmarks/fixed/verify_alignment/", spec[i], "_plots.jpg", sep=""),
       width=8.5, height=11, units="in", res=600)
  par(mfrow=c(3,1))
  plot(d1p$y ~ d1p$x, xlab="x", ylab="y", main="XY align", pch=16, col="red")
  points(d2p$y ~ d2p$x, pch=16, col="blue")
  plot(d1p$z ~ d1p$x, xlab="x", ylab="z", main="XZ align", pch=16, col="red")
  points(d2p$z ~ d2p$x, pch=16, col="blue")
  plot(d1p$y ~ d1p$z, xlab="z", ylab="y", main="ZY align", pch=16, col="red")
  points(d2p$y ~ d2p$z, pch=16, col="blue")
  dev.off()
}




### Re-organize landmark data using re-aligned data #####
# Read in re-aligned 3D landmark csv files
#i<-1
#dat.lm<-read.csv(paste('./Specimen_landmarks/fixed/', spec[i], '_realigned.csv', sep=""))
#dat.lm<-dat.lm[,2:4]
#dat.lm<-as.matrix(dat.lm)


#Create new data object to store reorganized data
#dat.lm2<-matrix(NA, numberOfSamples, 126)  #create empty matrix to store data
#nms<-read.csv('colnames_lm.csv', header=F)
#nms<-as.vector(nms[,1])

#colnames(dat.lm2)<-nms

#re-arrange landmark data to a single row
#hilow<-matrix(NA,42,2)
#hilow[,1]<-seq(1,124,3)
#hilow[,2]<-seq(3,126,3)

#for(i in 1:42){
#  dat.lm2[1,hilow[i,1]:hilow[i,2]]<-dat.lm[i,]
#}

###########################################################################################
###########################################################################################

#####           Develop Loop to process mutliple specimens at a time                  #####

###########################################################################################
###########################################################################################
#Re-make matrix to hold ID, sex, Age info **DIDN'T DO THIS - INSTEAD WILL CBIND THOSE FIELDS ONTO THIS MATRIX WHEN READY


for(i in 1:numberOfSamples){
  dat.lm<-read.csv(paste('./Specimen_landmarks/fixed/', spec[i], '_realigned.csv', sep=""))
  dat.lm<-dat.lm[,2:4]
  dat.lm<-as.matrix(dat.lm)
  
  for(j in 1:42){
    dat.lm2[i,hilow[j,1]:hilow[j,2]]<-dat.lm[j,]
  }
  
}


dat.lm2<-as.data.frame(dat.lm2)

## Add specimen, age, age class, and sex
agesex<-read.csv('Specimens_3D_agesex.csv')
dat.lm3<-cbind(agesex, dat.lm2)

## Get rid of row for 7343
#dat.lm3<-dat.lm3[-57,]

## export data
write.csv(dat.lm3, "Fisher_ontogeny_set1_realigned.csv", row.names=F)



### Procrustes transformation and analysis
library(geomorph)
data<-read.csv('Fisher_ontogeny_set1_realigned.csv')

#Separate the factors from the shape data
ages<-as.factor(data[,3])#age classes in third column
Y<-as.matrix(data[,-(1:4)])#remove those two columns, call the shape file "Y"

#Make the shape data a 3D array with each landmark in a row, each coordinate in a column (and each individual in a "sheet")
Y.shape<-arrayspecs(Y,42,3) #individuals separated, each row is a landmark, each value is a coordinate
#Superimpose it
Y.super<-gpagen(Y.shape)

Y.shape=Y.super$coords
Y.dat=two.d.array(Y.shape) #2d array: each individual in a row and each variable in a column

#adapt this for procrustes ANOVA:
# but note that procD.lm can use 2D matrix (from 'two.d.array' function), 3D array (created above using 'arrayspecs'), or geomorphic data frame ('geomorph.data.frame')
# could use: x <- geomorph.data.frame(y, sex=..., age=...) where y = procrustes transformed coords form gpagen

dat.full<-data
dat.full$ord.ac<-as.factor(ifelse(dat.full$AgeClass=="J", "A", ifelse(dat.full$AgeClass=="Y", "B", "C")))  #ordinal age class


pp<-geomorph.data.frame(Y.shape, sex=dat.full$Sex, age=dat.full$ord.ac)

#from ASM workshop:
#Do geographic poulations of tree squirrels differ in jaw shape?
#####################################################
#Procrustes anova of mean shapes
########################################
sexes<-dat.full$Sex
ageclass<-dat.full$ord.ac

pp.aov<-procD.lm(Y.shape~ageclass*sexes)  #can customize # iterations and many other things for procD.lm function -- procrustes distance linear model
pp.aov


### Using age as a continuous variable
#for each age, add 0.67 years to get reasonable estimate of true age (e.g., juveniles = 0 years but should be 0.67 yrs)
dat.full$age.yr<-dat.full$Age+0.67
dat.full$ln.age.yr<-log(dat.full$age.yr+1)
ln.age<-dat.full$ln.age.yr

#repeat procrustes ANOVA using log-age
pp.aov2<-procD.lm(Y.shape~ln.age*sexes)  
pp.aov2


#####################################################
#Pairwise comparisons of means
###########################################

#Using the advanced.procD.lm function in geomorph
#make geomorph data frame
f.gdf=geomorph.data.frame(Y.shape=Y.shape,sex=dat.full$Sex, ac=dat.full$ord.ac, la=ln.age)  

#permutation test for sex from procrustes anova usig age class
f.as1<-advanced.procD.lm(Y.shape~1,~sex,groups=~sex,data=f.gdf)   #Null model is slope only; comparison is geog.groups
f.as1

#could use the following to manually plot results:
f.as1$LS.means
#could store the LS.means and then plot them for male/female separately and draw vector lines

#permutation test for ln.age from procrustes ANOVA using ln.age
# if age and sex significant, use advanced.procD.lm(Y.shape~1,~age + sex,groups=~age + sex,data=f.gdf)



#########################################
#
#  Fisher growth analysis
#
#########################################

# ANCOVA model to determine whether growth rates significantly different among males and females
csize<-Y.super$Csize

gr.1<-lm(csize ~ sexes*ln.age)  #growth ancova: effect of sex/age
summary(gr.1)
plot(gr.1) #diagnostic plots look good

##Plot of results
newdat<-data.frame(ln.age=seq(0, 2.2, 0.01))
newdat$F.csize<-(gr.1$coefficients[1]+gr.1$coefficients[3]*newdat$ln.age)
newdat$M.csize<-(gr.1$coefficients[1]+gr.1$coefficients[2]+gr.1$coefficients[3]*newdat$ln.age)
newdat$M.csize2<-(gr.1$coefficients[1]+gr.1$coefficients[2]+(gr.1$coefficients[3]+gr.1$coefficients[4])*newdat$ln.age)

#plot with sex-specific intercepts only
plot(csize ~ jitter(ln.age), col=sexes, pch=16, xlab="ln(Age + 1)", ylab="Centroid Size", xlim=c(0,2.2))
points(newdat$F.csize ~ newdat$ln.age, col="black", lwd=2, type="l")
points(newdat$M.csize ~ newdat$ln.age, col="red", lwd=2, type="l")
###NOTE: there appear to be 2 females that are listed as males (both juveniles)

#plot with sex-specific slopes and intercepts
plot(csize ~ jitter(ln.age), col=sexes, pch=16, xlab="ln(Age + 1)", ylab="Centroid Size", xlim=c(0,2.2))
points(newdat$F.csize ~ newdat$ln.age, col="black", lwd=2, type="l")
points(newdat$M.csize2 ~ newdat$ln.age, col="red", lwd=2, type="l")
###NOTE: there appear to be 2 females that are listed as males (both juveniles)

#back-transform age
newdat$age<-exp(newdat$ln.age)-1
ages2<-dat.full$age.yr

#plot with sex-specific intercepts only
plot(csize ~ jitter(ages2, amount=0.1), col=sexes, pch=16, xlab="Age (yrs)", ylab="Centroid Size", xlim=c(0,5))
points(newdat$F.csize ~ newdat$age, col="black", lwd=2, type="l")
points(newdat$M.csize ~ newdat$age, col="red", lwd=2, type="l")
###NOTE: there appear to be 2 females that are listed as males (both juveniles)

#plot with sex-specific slopes and intercepts
plot(csize ~ jitter(ages2, amount=0.1), col=sexes, pch=16, xlab="Age (yrs)", ylab="Centroid Size", xlim=c(0,5))
points(newdat$F.csize ~ newdat$age, col="black", lwd=2, type="l")
points(newdat$M.csize2 ~ newdat$age, col="red", lwd=2, type="l")
###NOTE: there appear to be 2 females that are listed as males (both juveniles)




#####################################################################
#Ontogenetic allometry in rat cranial shape
###################################################################
#Do rat crania change shape over ontogeny?
###################################################################
data(rats)
ratland<-rats$x
rats<-gpagen(ratland)
rats.shape<-rats$coords
rats.size<-rats$Csize

rat.allo=procD.lm(rats.shape~log(rats.size))
rat.allo

rats.allo2=procD.allometry(rats.shape~rats.size)
rats.allo2
doPlot<-plot(rats.allo2,method="RegScore",shapes=TRUE)
plotRefToTarget(doPlot$min.shape,doPlot$max.shape)
title("Ontogenetic change in rat cranium")

doPlot<-plot(rats.allo2,method="CAC",shapes=TRUE)
doPlot<-plot(rats.allo2,method="PredLine",shapes=TRUE)

#################################################################################
#From simple to complex: size, sexual dimorphism and 
#interspecific differences in shape, allometry and sexual dimorphism
################################################################################
data(apes)
#There is something wrong with specimen #60 so we'll leave it out
apes.dat=apes$x[,,-60]
#First we will superimpose the data and plot it
apes.proc=gpagen(apes.dat)
apes.shape=apes.proc$coords
apes.size=apes.proc$Csize
plotAllSpecimens(apes.shape)
#They're upside down (and backwards)
apes.shape=-1*apes.shape
plotAllSpecimens(apes.shape)

apes.groups=as.factor(apes$group[-60])
apes.groups
apes.species=as.factor(c(rep("gor",59),rep("chimp",53),rep("orang",54)))
apes.sex=as.factor(c(rep("female",30),rep("male",29),rep("female",25),rep("male",28),rep("female",24),rep("male",30)))

##################################################
#Do species differ in their mean shapes?
####################################################
gdf=geomorph.data.frame(apes.shape=apes.shape,apes.species=apes.species,apes.sex=apes.sex,apes.groups=apes.groups)
ape.specDiff=procD.lm(apes.shape~apes.species,data=gdf)
ape.specDiff
#Pairwise comparisons
ape.pairwise=advanced.procD.lm(apes.shape~1,~apes.species,groups=~apes.species,data=gdf)
ape.pairwise
#Pairwise comparisons
Pairwise.differences<-permudist(apes.shape, groups=apes.species,which=1:3, rounds=10000)  #could put in Holm's adjustment for P-values
Pairwise.differences

##################################################
#Is the difference due solely to size?
##############################################
gdf=geomorph.data.frame(apes.shape=apes.shape,apes.size=apes.size,apes.species=apes.species,apes.sex=apes.sex,apes.groups=apes.groups)
allometry<-procD.lm(apes.shape~apes.size+apes.species,data=gdf)
allometry
allo.apes<-advanced.procD.lm(apes.shape~apes.size+apes.species,~apes.size*apes.species,groups=~apes.species, slopes=~apes.size)
allo.apes
#################################################
#Do sexes, on average, differ in their mean shapes?
#####################################################
ape.sexDiff=procD.lm(apes.shape~apes.sex,data=gdf)
ape.sexDiff

#Do sexes differ, taking species' differences into account
#####################################################
ape.speciesSex=procD.lm(apes.shape~apes.species+apes.sex,data=gdf)
ape.speciesSex

##########################################################################
#But can we treat those as independent effects:
#Do species differ in sexual dimorphism?
#If so, do they differ in the direction or extent of sexual dimorphism?
########################################################################
TA=trajectory.analysis(apes.shape~apes.species*apes.sex)
TA
names(TA)
TA$aov
TA$trajectory.angle.deg
TA$P.angle

TA$means
apes.mean.shapes=arrayspecs(TA$means,8,2)

############################################
#Plot the difference between the sexes
###################################################
GP1=gridPar(pt.bg="red",pt.size=1,tar.pt.bg="black",tar.pt.size=1,n.col.cell=40)
m1=apes.mean.shapes[,,1]
m2=apes.mean.shapes[,,2]
joinline=t(matrix(c(1,6,6,7,7,8,2,8,2,3,3,4,4,5,1,5),2,8))
#joinline2=define.links(apes.mean.shapes[,,1])#This will let you make the links file interactively -- click and then click back to console and type Y for yes to continue selecting stuff

mag=3
mag <- (mag - 1)
m2.new<-m2+(m2-m1)*mag
m2.new

plot.new()
plotRefToTarget(m1,m2,useRefPts=TRUE,method="TPS",gridPar=GP1,mag=3,links=joinline)
arrows(m1[,1],m1[,2],m2.new[,1],m2.new[,2],lwd=2,length=0.1,cex=1)
title("chipmanzee sexual dimorphism")

m3=apes.mean.shapes[,,3]
m4=apes.mean.shapes[,,4]

mag=3
mag <- (mag - 1)
m4.new<-m4+(m4-m3)*mag
m4.new

plot.new()
plotRefToTarget(apes.mean.shapes[,,3],apes.mean.shapes[,,4],mag=3,gridPar=GP1,method="TPS",useRefPts=TRUE,links=joinline2)
arrows(m3[,1],m3[,2],m4.new[,1],m4.new[,2],lwd=2,length=0.1,cex=1)
title("gorilla sexual dimorphism")

m5=apes.mean.shapes[,,5]
m6=apes.mean.shapes[,,6]

mag=3
mag <- (mag - 1)
m6.new<-m6+(m6-m5)*mag
m6.new

plot.new()
plotRefToTarget(apes.mean.shapes[,,5],apes.mean.shapes[,,6],mag=3,gridPar=GP1,method="TPS",useRefPts=TRUE,links=joinline2)
arrows(m5[,1],m5[,2],m6.new[,1],m6.new[,2],lwd=2,length=0.1,cex=1)
title("orang sexual dimorphism")
#############################################




