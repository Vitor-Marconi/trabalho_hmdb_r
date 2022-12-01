library(readxl)
structures<- read_csv("structures (1).csv")
#Import the libraries

install.packages('dslabs')

install.packages('dyplr')

install.packages('readr')

install.packages('tidyverse')

install.packages('esquisse')


library(dslabs)

library(readr)

library(tidyverse)

library(dplyr)

library(ggplot2)

library(esquisse)

library(tidyr)

#loading the database summum

summary(structures)

# check if there is any Na in the database

ind <-is.na(structures)
ind
#Creates a variable for each column of the database

#Way of representing chemical structures using ASCII characters

smiles <- structures$Smiles

#Indicate the simple numbers of each type of atom in a molecule

MolecularFormula <- structures$Molecular_Formula

#Number of atoms in the molecule

NumAtoms <- structures$NumAtoms

#The exact molecular weight of the molecule

ExactMolWt <- structures$ExactMolWt

#Number of Rotatable Bonds

NumRotableBonds <- structures$NumRotatableBonds

#Atom-based calculation of LogP

MolLogp <- structures$MolLogP

#The exact ringcount of the molecule

RingCount <- structures$Ring_Count

#Number of Hydrogen Bond Acceptors

NumHAcceptors <- structures$NumHAcceptors

#Topological polar surface area

TPSA <- structures$TPSA

#Number of Hydrogen Bond Donors

NumHDonors <- structures$NumHDonors

"------------------------------------------------------------------------
-------------------------------------------------------------------------"
#creating histograms 

hist(RingCount, col = "darkblue", xlab="Ring Count", main = "Histogram of ring count", xlim = c(0,10))

hist(NumAtoms, col = "purple", xlab = "NumAtoms", main = "histogram of NumAtoms", xlim = c(0,150))

hist(NumHDonors, col= "red", xlab ="NumHDonors",  xlim = c(0,10))

hist(NumHAcceptors, col = "orange", xlab = "NumHAcceptors")

hist(ExactMolWt, col = "blue", xlab = "ExactMolWt", xlim = c(0,2000))

"--------------------------------------------------
---------------------------------------------------"


#crate graphics with the fuction plot
plot(ExactMolWt,NumAtoms, col = "purple")
plot(TPSA, NumAtoms, col= "blue")
plot(TPSA, ExactMolWt, xlab ="Topological polar surface area", ylab = "ExactMolWt")
"--------------------------------------------------
---------------------------------------------------"

#creating graphics with fuction ggplot

#Plot 'ExactMolWt x NumAtoms', colour = RingCount
ggplot(structures) +
  aes(x = NumRotableBonds, y = NumAtoms, colour = RingCount) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(title = "NumRotableBonds x NumAtoms") +
  theme_bw()


#Plot RingCount x NumHAcceptors

ggplot(structures) +
  aes(x = RingCount, y = NumHAcceptors, colour = NumAtoms) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(title = "RingCount x NumHAcceptors") +
  theme_bw()

#Plot ExactmolWt x NumHAcceptors 

ggplot(structures) +
  aes(x = ExactMolWt, y = NumHAcceptors, colour = NumHDonors) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(title = "ExactMolWt x NumHAcceptors") +
  theme_bw()


#plot NumrotableBonds x NumHAcceptors, colors = NumHDonors

ggplot(structures) +
  aes(
    x = NumRotableBonds,
    y = NumHAcceptors,
    colour = NumHDonors
  ) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(title = "NumRotableBonds x NumHAcceptors") +
  theme_minimal()

#Plot NumHAcceptors x NumAtoms, colour = ExactMolWt

ggplot(structures) +
  aes(x = NumHAcceptors, y = NumAtoms, colour = ExactMolWt) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(title = "NumHAcceptors x NumAtoms") +
  theme_bw()
"------------------------------------------------------
-------------------------------------------------------"


#Max, Mean, min and median of each descriptor.

Max_Min_Mean_Median <- data.frame(Descriptors = c("NumAtoms", "ExactMolWt", "NumRotableBonds","RingCount","NumHAcceptors","NumHDonors","TPSA"),
                                  Max = c(max(NumAtoms),
                                          max(ExactMolWt),
                                          max(NumRotableBonds),
                                          max(RingCount),
                                          max(NumHAcceptors),
                                          max(NumHDonors),
                                          max(TPSA)),
                                  Min = c(min(NumAtoms),
                                          min(ExactMolWt),
                                          min(NumRotableBonds),
                                          min(RingCount),
                                          min(NumHAcceptors),
                                          min(NumHDonors),
                                          min(TPSA)),
                                  Mean = c(mean(NumAtoms),
                                           mean(ExactMolWt),
                                           mean(NumRotableBonds),
                                           mean(RingCount),
                                           mean(NumHAcceptors),
                                           mean(NumHDonors),
                                           mean(TPSA)),
                                  median = c(median(NumAtoms),
                                             median(ExactMolWt),
                                             median(NumRotableBonds),
                                             median(RingCount),
                                             median(NumHAcceptors),
                                             median(NumHDonors),
                                             median(TPSA)),
                                  stringsAsFactors = FALSE)
Max_Min_Mean_Median
