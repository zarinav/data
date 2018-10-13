###############################################
# This file contains code for illustrating main examples in                   
# Vakhitova & Alston-Knox
# "Non-significant p-values? Strategies to understand and better  
# determine the importance of effects and interactions in 
# logistic regression
# PlosOne                                                             
################################################
# Load necessary libraries
library(BMA)          # Section 5.4: Bayesian model averaging 
library(MCMCpack)     # Section 5.3: Bayesian GLM
library(MASS)         # Section 5.3: To plot histograms using 
# truehist()
library(psych)        # Section 5.3: To create design matrix X

##### Example: Guardianship in Cyberspace Data

# Load data: Cyberabuse Newspaper Reports data

# DFI = read.csv( "CyberabuseCMC.csv", header = T )


###### Section 5.1.1: Single factor GLM analysis

# Make a single 4 level factor
DFI$RelMotive=DFI$Motivation*10+ DFI$Relationship
DFI$RelMotive[ DFI$RelMotive == 0 ] = "NoMotive_NoRel"
DFI$RelMotive[ DFI$RelMotive == 1 ] = "NoMotive_Rel"
DFI$RelMotive[ DFI$RelMotive == 10 ] = "Motive_NoRel"
DFI$RelMotive[ DFI$RelMotive == 11 ] = "Motive_Rel"

DFI$RelMotive = factor( DFI$RelMotive )
# Make No motivation + No relationship the base comparison
DFI$RelMotive = relevel( DFI$RelMotive, ref = "NoMotive_NoRel" ) 

# Logistic regression model
# Using Wald (p-values), only having both a prior relationship 
# and expressive motivation significantly differs from 
# the base level.

Model1 = glm( formula = Method ~ RelMotive, 
              family = "binomial", data = DFI )
summary( Model1 ) 

###############################################
###### Section 5.2: Results using the Wald test

# Make Motivation and Relationship a factor variable

DFI$Motivation = factor(DFI$Motivation)
DFI$Relationship = factor(DFI$Relationship)

# Step 1: GLM with main effects and interaction
# No significant p-values (Wald test)
Model2 = glm( formula = Method ~ Relationship * Motivation, 
              family = "binomial", data = DFI )
summary( Model2 )


# Step 2: Drop interaction term. Only main effects
# Both main effects now significant using Wald test
Model3 = glm( formula = Method ~ Relationship + Motivation, 
              family = "binomial", data = DFI )
summary( Model3 )

################################################
##### Section 5.3: Results using Change in Deviance (Table 6)

# Full model vs main effects compared using change in deviance
anova( Model2, Model3, test = "Chisq" )  

# Table 6: Removing interaction does not result in 
# significantly worse fit

Model4 = glm( formula = Method ~ Relationship, 
              family = "binomial", data = DFI )
summary( Model4 )

anova( Model3, Model4, test = "Chisq" )  # Main effects vs 
# Relationship only

Model5 = glm( formula = Method ~ Motivation, 
              family = "binomial", data = DFI )
summary( Model5 )

anova( Model3, Model5, test = "Chisq" )  # Main effects vs 
# Motivation only

###############################################
#Section 5.4: Bayesian GLM  with vaguely informative priors

Model1.Bayesian = MCMClogit( Method ~ Relationship * Motivation, 
                             data = DFI,
                             burnin = 10000, 
                             mcmc = 110000, 
                             thin = 100, 
                             tune = 1.1,
                             b0 = c( -1, 0.1, 0.1, 1.1 ), 
                             B0 = c( 0.5, 2, 2, 0.5 ), 
                             verbose = 1000 )
summary( Model1.Bayesian )

### Figure 4: Posterior distribution histograms
## Create design matrix for predictions


# Use only unique values of Relationship and Motivation
Unique.X = unique( DFI[ , 1:2 ] ) 
Xmat = dfOrder( Unique.X )

Pred.Fit = Xmat%*%t( Model1.Bayesian )
Prob.Fit = matrix( 0, nrow = dim( Pred.Fit )[ 1 ], 
                   ncol = dim( Pred.Fit )[ 2 ] )
par( mfrow = c( 2, 2 ) )
for( i in 1:4 ){
  Prob.Fit[ i,] = exp( Pred.Fit[ i, ] ) / 
    ( 1 + exp( Pred.Fit[ i, ] ) )
  truehist( Prob.Fit[ i, ] )
}

par( mfrow = c( 4,3 ), oma = c( 6,4,4,1 ), 
     mar = c( 0, 0, 0, 0 ) )
for( i in 1:4 ){
  truehist( Model1.Bayesian[ , i ], 
            xlim = c( -3, 3 ), axes = F )
  box()
  if( i == 4 ){
    axis( 1, at = seq( -2.5, 2.5, by = 0.5 ), labels = FALSE)
    axis( 1, at = seq( -2,2, by = 1 ), labels = TRUE, 
          tick = FALSE)
    text( -2.4, 0.65, expression( beta[ 3 ] ) )
  }
  if( i == 1 ){
    text( -2.4, 0.85, expression( beta[ 0 ] ) )
  }
  if( i == 2 ){
    text( -2.4, 0.65, expression( beta[ 1 ] ) )
  }
  if( i == 3 ){
    text( -2.4, 0.75, expression( beta[ 2 ] ) )
  }
  truehist( Pred.Fit[ i, ], xlim = c( -2.5, 2 ), axes = F)
  box()
  if( i == 4 ){
    axis( 1, at = seq( -2, 1.5, by = 0.5 ), labels = FALSE )
    axis( 1, at = seq( -2, 1, by = 1 ), labels = TRUE, 
          tick = FALSE )
    text( -2, 1.4, expression( paste( beta[ 0 ], "+", 
                                      beta[ 1 ], "+", 
                                      beta[ 2 ], "+", beta[ 3 ] ) ) )
  }
  if( i == 1 ){
    text( -2.4, 0.85, expression( beta[ 0 ] ) )
  }
  if( i == 2 ){
    text( -2.25, 0.65, expression( paste( beta[ 0 ], "+", 
                                          beta[ 1 ] ) ) )
  }
  if( i == 3 ){
    text( -2.25, 0.75, expression( paste( beta[ 0 ], "+", 
                                          beta[ 2 ] ) ) )
  }
  truehist( Prob.Fit[ i, ], xlim = c( 0, 1.05 ), axes = F )
  box()
  if( i == 4 ){
    axis( 1, at = seq( 0, 1, by = 0.1 ), labels = FALSE )
    axis( 1, at = seq( 0.1, 0.9, by = 0.2 ), labels = TRUE, 
          tick = FALSE )
    text( 0.88, 6.0, "Relationship & " )
    text( 0.88, 5.3, "Expressive Motivation" )
  }
  
  if( i == 1 ){
    text( 0.88, 5.0, "No Relationship &" )
    text( 0.88, 4.3, "Instrumental Motivation" )
  }
  if( i == 2 ){
    text( 0.88, 3.8, "Relationship &" )
    text( 0.88, 3.2, "Instrumental Motivation" )
  }
  if( i == 3 ){
    text( 0.88, 5.0, "No Relationship &" )
    text( 0.88, 4.3, "Expressive Motivation" )
  }
}
mtext( "Density", side = 2, line = 49, at = 15, cex = 1.5 )
mtext( "Predictive Posterior Fit", side = 1, line = 4, 
       at = -0.6, cex = 1.5 )
mtext( "Posterior distribution: Coefficients", side = 3, 
       line = 26, 
       at = -1.70, cex = 1 )
mtext( "Posterior distribution: Linear predictor", side = 3, 
       line = 26, 
       at = -0.6, cex = 1 )
mtext( "Posterior distribution: Response", side = 3, line = 26, 
       at = 0.5, cex = 1)

par( mfrow = c( 1, 1 ), oma = c( 2, 2, 1, 1 ), 
     mar = c( 4, 4, 1, 1 ) )
#############################################
## Section 5.5: Bayesian Model Averaging

DFBMA = DFI[!is.na( DFI$Method), ]  # removed missing data

form = Method ~ Relationship * Motivation
Model.BMA = bic.glm( f = form, data = DFBMA, 
                     glm.family = binomial(), 
                     strict = FALSE, factor.type = TRUE)
## Summary provides most likely model 
## and inclusion probabilities of variables
summary( Model.BMA )

## Figure 5: Image plot
imageplot.bma( Model.BMA, cex.lab = 1, cex.axis = 1.5, 
               color = c( "#616A6B", "black", "#CCD1D1" ) )