#===============================================================================
# 29/07/2024
# Diagonal Mobility Model on Children
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



# DMM All Children --------------------------------------------------------

# * Baseline --------------------------------------------------------------

set.seed(123)
mod.base <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE + RSINGLE +
                  Dref(PEDUA, REDUA),
                data = child)

print(summary(mod.base), digits = 4)
print(DrefWeights(mod.base), digits = 4)


# * + Single-Child Weight Interaction --------------------------------------

mod.singleweight <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE + RSINGLE +
                          Dref(PEDUA, REDUA,
                               delta = ~ 1 + RSINGLE),
                        data = child)

print(summary(mod.singleweight), digits = 4)
print(DrefWeights(mod.singleweight), digits = 4)

lrtest(mod.singleweight, mod.base)


# * + Mobility Indicators -------------------------------------------------

# Main model with upward/downward mobility
mod.mob <- gnm(RSATISFY ~ -1 + upa + downa + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE + RSINGLE + 
                      Dref(PEDUA, REDUA),
                    data = child)

print(summary(mod.mob), digits = 4)
print(DrefWeights(mod.mob), digits = 4)

lrtest(mod.mob, mod.base)

# Model with interaction with only-child status
mod.mob.single <- gnm(RSATISFY ~ -1 +  RSINGLE*upa + RSINGLE*downa + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE +
                        Dref(PEDUA, REDUA),
                      data = child)

print(summary(mod.mob.single), digits = 4)
print(DrefWeights(mod.mob.single), digits = 4)

lrtest(mod.mob.single, mod.base)


# DMM Only Children -------------------------------------------------------

child <- child %>% 
  filter(RSINGLE == 1)

# * Baseline --------------------------------------------------------------

set.seed(123)
mod.base <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE +
                  Dref(PEDUA, REDUA),
                data = child)

print(summary(mod.base), digits = 4)
print(DrefWeights(mod.base), digits = 4)


# * + Gender Weight Interaction -------------------------------------------

mod.genderweight <- gnm(RSATISFY ~ -1 + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE +
                          Dref(PEDUA, REDUA,
                               delta = ~ 1 + RGENDER),
                        data = child)

print(summary(mod.genderweight), digits = 4)
print(DrefWeights(mod.genderweight), digits = 4)

lrtest(mod.genderweight, mod.base)


# * + Mobility Indicators -------------------------------------------------

# Main model with upward/downward mobility
mod.mob <- gnm(RSATISFY ~ -1 + upa + downa + RGENDER + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE + 
                 Dref(PEDUA, REDUA),
               data = child)

print(summary(mod.mob), digits = 4)
print(DrefWeights(mod.mob), digits = 4)

lrtest(mod.mob, mod.base)

# Model with gender interaction
mod.mob.gender <- gnm(RSATISFY ~ -1 + RGENDER*upa + RGENDER*downa + RAGE + RMSTAT + RPARTY + RHUKOU + RMINORITY + RRESIDE + 
                        Dref(PEDUA, REDUA),
                      data = child)

print(summary(mod.mob.gender), digits = 4)
print(DrefWeights(mod.mob.gender), digits = 4)

lrtest(mod.mob.gender, mod.base)