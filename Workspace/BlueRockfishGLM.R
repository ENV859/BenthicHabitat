## ------------------------------------------------------------------------
#Load the needed libraries
library(foreign)
library(arm)
library(MASS)

## ------------------------------------------------------------------------
#Set the input and output file names
#Inputs
presence_dbf <- 'Presence.dbf' #SAMPLE product of known occurrenced 
random_dbf <-'Random.dbf'      #SAMPLE product of random points

#Outputs
mdl3var_file <- 'mdlvar.csv'
mdlaic_file <- 'mdkaic.csv'

## ------------------------------------------------------------------------
#Read in the two arcGIS SAMPLE results into R data frame objects
present <- read.dbf(presence_dbf)
random <- read.dbf(random_dbf)

## ------------------------------------------------------------------------
#Drop the first three columns from each (ID, X, and Y fields...)
present <- present[,c(-1:-3)]
random <- random[,c(-1:-3)]

## ------------------------------------------------------------------------
#Add a "species" column to each, listing presence (1) or absence (0)
present['species'] = rep(1, nrow(present))
random['species'] = rep(0, nrow(random))

## ------------------------------------------------------------------------
#Bind the two tables into a single table
sp.pa = rbind(present, random)

## ------------------------------------------------------------------------
#Change missing data (-9999) to NA
sp.pa[sp.pa == -9999] <- 0

## ------------------------------------------------------------------------
#Show the first 10 rows of data
sp.pa[1:10,]    

## ------------------------------------------------------------------------
#Take a look at the column names:
colnames(sp.pa)

## ------------------------------------------------------------------------
#Take a look at the column types with the `str` ("structure") command
str(sp.pa)

## ------------------------------------------------------------------------
#Show a summary of the data
summary(sp.pa)

## ------------------------------------------------------------------------
#Use the `class` function to display the type
class(sp.pa$habras10)

## ------------------------------------------------------------------------
#Change habras10 to 'factor' (categorical)
sp.pa$habras10 = factor(sp.pa$habras10)

## ------------------------------------------------------------------------
#Now see that its class is defined as `factor`
class(sp.pa$habras10)	

## ------------------------------------------------------------------------
#See the levels contained in the category
levels(sp.pa$habras10)    

## ----pairplot------------------------------------------------------------
#Look at pairwise plots of variables to examine correlations
pairs(sp.pa)	

## ------------------------------------------------------------------------
#Run the GLM
mdlglm <- glm(formula = species ~ bath10_8w + botc10_8ws + brkd10_8w + klpd10_8w + habras10, family = binomial(link = "logit"), data = sp.pa)

## ------------------------------------------------------------------------
#
summary(mdlglm)

## ------------------------------------------------------------------------
mdlbayes <- bayesglm(formula = species ~ bath10_8w + botc10_8ws + brkd10_8w + klpd10_8w + habras10, family = binomial(link = "logit"), data = sp.pa)
mdlbayes

## ------------------------------------------------------------------------
summary(mdlbayes)

## ------------------------------------------------------------------------
mdlaic <- stepAIC(mdlbayes, trace=T)
mdlaic

## ------------------------------------------------------------------------
summary(mdlaic)

## ------------------------------------------------------------------------
mdl3var <- bayesglm(formula = species ~ bath10_8w + botc10_8ws + brkd10_8w, family = binomial(link = "logit"), data = sp.pa)
mdl3var

## ------------------------------------------------------------------------
summary(mdl3var)

## ------------------------------------------------------------------------
write.csv(summary(mdl3var)$coefficients, file=mdl3var_file)
write.csv(summary(mdlaic)$coefficients, file=mdlaic_file)

