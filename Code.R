#Import data
data<- read.csv("CombinedCSV.csv", header= TRUE, stringsAsFactors=FALSE)

#View data
str(data)

head(Variables)

#Upload CSV with selected columns
Variables<- read.csv("SelectedColumns.csv", header=TRUE, stringsAsFactors = FALSE)

#View data
str(Variables)

# remove the 'unknowns',"both" and "head"
Variables <- subset(Variables, subset=!(Variables$Modifier..2 %in% c("unknown", "head", "both paws"))) # adding the ! in front of %in% means 'remove all elements that are on this list'
unique(Variables$Modifier..2)
# "both paws" "right paw" "left paw"

# Currently there are rows where left or right lever are not specified:
unique(Variables$Modifier..1)
# "right lever" "left lever"  "None" 

# we remove the ones with "none"
Variables <- subset(Variables, subset = Variables$Modifier..1!="None")
unique(Variables$Modifier..1)
# now it's just left and right lever

# Extract some numbers here:
# How many solves did we have in total?

# How many were with right, left or both paws?
length(Variables$Modifier..2[Variables$Modifier..2=="right paw"]) #592
length(Variables$Modifier..2[Variables$Modifier..2=="left paw"]) #612
length(Variables$Modifier..2[Variables$Modifier..2=="both paws"]) #0
#SW: sorry I had meant to remove both paws but had made a typo - there should be none now

# we are now running the model with just two outcome categories (left and right), meaning you can use the lme4 package

# much of the model specification remains the same - you can now use the 'binomial' model family

Variables$Modifier..1<- factor(Variables$Modifier..1)
# we do the same for modifier 2
Variables$Modifier..2<- factor(Variables$Modifier..2)


# 1) Run the binomial generalized linear mixed effect model ---------------



library(lme4)

model.lat <- glmer(Modifier..2 ~ Modifier..1 + (1|Subject), data=Variables, family = binomial)

summary(model.lat)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: Modifier..2 ~ Modifier..1 + (1 | Subject)
# Data: Variables
# 
# AIC      BIC   logLik deviance df.resid 
# 1285.1   1300.4   -639.6   1279.1     1201 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8006 -0.6618 -0.4344  0.5064  2.3022 
# 
# Random effects:
#   Groups  Name        Variance Std.Dev.
# Subject (Intercept) 0.1344   0.3666  
# Number of obs: 1204, groups:  Subject, 36
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.1166     0.1377  -8.112 4.97e-16 ***
#   Modifier..1right lever   2.5931     0.1627  15.934  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Mdfr..1rghl -0.442


# how to interpret this: 
# the lever side (right/left) predicts which paw the squirrels use (right/left) - the effect is significant



# 2) Repeatability --------------------------------------------------------



# Calculate repeatability:


# Extract random effect variance (Subject)
var_Subject <- as.numeric(lme4::VarCorr(model.lat)$Subject[1])

# Residual variance for logistic regression
var_Residual <- pi^2 / 3

# Calculate repeatability
repeatability <- var_Subject / (var_Subject + var_Residual)

# Print the repeatability
print(paste("Repeatability (R):", round(repeatability, 3)))

# repeatabilies can range from 0-1. Our result of 0.039 indicates very low repeatability. 
