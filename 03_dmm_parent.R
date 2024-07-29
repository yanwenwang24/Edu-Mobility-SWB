#===============================================================================
# 29/07/2024
# Diagonal Mobility Model on Parents
# Educational Assortative Mating and Subjective Well-Being
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================

######################################################################
# Loading packages
######################################################################
library(tidyverse)
library(haven)
library(gnm)
library(lmtest)
library(stargazer)
library(jtools)

######################################################################
# Loading data
######################################################################
load("to-be-filled")


# DMM ---------------------------------------------------------------------

# * Baseline --------------------------------------------------------------

set.seed(123)
mod.base <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + CGENDER + 
                  Dref(PEDUA, CEDUA),
                data = parents)

print(summary(mod.base), digits = 4)
print(DrefWeights(mod.base), digits = 4)


# * + Gender Weight Interaction -------------------------------------------

mod.genderweight <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + CGENDER + 
                          Dref(PEDUA, CEDUA, delta = ~ 1 + RGENDER),
                        data = parents)

print(summary(mod.genderweight), digits = 4)
print(DrefWeights(mod.genderweight), digits = 4)

lrtest(mod.genderweight, mod.base)


# * + Mobility Indicators -------------------------------------------------

# Main model with upward/downward mobility
mod.mob <- gnm(RSATISFY ~ -1 + upa + downa + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + CGENDER + 
                 Dref(PEDUA, CEDUA),
               data = parents)

print(summary(mod.mob), digits = 4)
print(DrefWeights(mod.mob), digits = 4)

lrtest(mod.mob, mod.base)

# Model with interaction with parents' gender
mod.mob.rgender <- gnm(RSATISFY ~ -1 + RGENDER*upa + RGENDER*downa + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + CGENDER +
                         Dref(PEDUA, CEDUA),
                       data = parents)

print(summary(mod.mob.rgender), digits = 4)
print(DrefWeights(mod.mob.rgender), digits = 4)

lrtest(mod.mob.rgender, mod.base)

# Model with interaction with child's gender
mod.mob.cgender <- gnm(RSATISFY ~ -1 + CGENDER*upa + CGENDER*downa + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + 
                         Dref(PEDUA, CEDUA),
                       data = parents)

print(summary(mod.mob.cgender), digits = 4)
print(DrefWeights(mod.mob.cgender), digits = 4)

lrtest(mod.mob.cgender, mod.base)

# Model with three-way interaction (parent's & child's gender)

mod.mob.rcgender <- gnm(RSATISFY ~ -1 + RGENDER*CGENDER*upa + RGENDER*CGENDER*downa + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + CRESIDE + 
                          Dref(PEDUA, CEDUA),
                        data = parents)

print(summary(mod.mob.rcgender), digits = 4)
print(DrefWeights(mod.mob.rcgender), digits = 4)

lrtest(mod.mob.rcgender, mod.base)