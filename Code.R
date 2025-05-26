#Import data
data<- read.csv("CombinedCSV.csv", header= TRUE, stringsAsFactors=FALSE)

#View data
str(data)


#Upload CSV with selected columns
Variables<- read.csv("SelectedColumns.csv", header=TRUE, stringsAsFactors = FALSE)

# look at the first few lines
head(Variables)


#View data
str(Variables)

# remove the 'unknowns',"both" and "head"
Variables <- subset(Variables, subset=!(Variables$Modifier..2 %in% c("unknown", "head", "both paws"))) # adding the ! in front of %in% means 'remove all elements that are on this list'
unique(Variables$Modifier..2)
# "right paw" "left paw"

# Currently there are rows where left or right lever are not specified:
unique(Variables$Modifier..1)
# "right lever" "left lever"

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


#LT: How many solves for each lever side?
length(Variables$Modifier..1[Variables$Modifier..1=="right lever"])#471
length(Variables$Modifier..1[Variables$Modifier..1=="left lever"])#733


# we are now running the model with just two outcome categories (left and right), meaning you can use the lme4 package

# much of the model specification remains the same - you can now use the 'binomial' model family

Variables$Modifier..1<- factor(Variables$Modifier..1)
# we do the same for modifier 2
Variables$Modifier..2<- factor(Variables$Modifier..2)


# 1) Run the binomial generalized linear mixed effect model ---------------



library(lme4)

# SW: I have made one small change to the model - I instead of (1|subject), I included ( Modifier 1| subject), which allows individuals to have paw preferences for a given lever rather than overall. (random slope model)

model.lat <- glmer(Modifier..2 ~ Modifier..1 + (Modifier..1|Subject), data=Variables, family = binomial)

summary(model.lat)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: Modifier..2 ~ Modifier..1 + (Modifier..1 | Subject)
# Data: Variables
# 
# AIC      BIC   logLik deviance df.resid 
# 1266.7   1292.2   -628.4   1256.7     1199 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.8726 -0.6381 -0.4797  0.5136  2.0846 
# 
# Random effects:
#   Groups  Name                   Variance Std.Dev. Corr 
# Subject (Intercept)            0.1088   0.3299        
# Modifier..1right lever 0.8255   0.9086   -0.51
# Number of obs: 1204, groups:  Subject, 36
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.1355     0.1556  -7.298 2.93e-13 ***
#   Modifier..1right lever   2.6982     0.3071   8.785  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# Mdfr..1rghl -0.563


# how to interpret this: 

# FIXED EFFECTS:
# the intercept is the log odds of pushing right when the left lever is used (reference category)
# Modifier..1right lever is the effect of using the right paw on the probability of pushing the right lever

# RANDOM EFFECTS:
# Subject (Intercept): some individuals show preferences for the left or the right lever regardless of paw preference
# Slope variance (Modifier..1right lever): Some individuals may have a stronger or weaker tendency to push in a certain direction based on their paw preference
# Correlation between intercept and slope (-0.51): A negative correlation suggests that individuals who generally push left more often (low intercept) tend to have a weaker effect of paw preference on push direction.



#Converting estimates into probabilities-------------------------------- --------------------------------------------------------------------

#converting intercept

log_odds.int <- -1.1355

probability.int <- exp(log_odds.int)/ (1 + exp(log_odds.int))

print(probability.int)

# SW: perfect - you could also use the function 'plogis' as a shorter version
plogis(fixef(model.lat)[1])
# 0.2431525 -> 24.3% probability of using the right paw when pushing left
# SW: what is the probability of using the left paw when pushing left?

1-plogis(fixef(model.lat)[1])


#converting Modifier..1

log_odds.mod1 <- 2.698249 

probability.mod1<- exp(log_odds.mod1)/ (1+ exp(log_odds.mod1))

print(probability.mod1)

plogis(fixef(model.lat)[2])
# 0.9369233 -> squirrels have a 93.7% probability of using the right paw when pushing the right lever
# SW: what is their probability of using the left paw when using the right lever?


#Calculate the confidence interval-------------------------------------- ------------------------------------------------------------------

# Z-score for 95% confidence interval
z_score <- 1.96

#Std errors
std.error.int <- 0.1556
std.error.mod1 <- 0.3071

# SW: this is almost right - you need to take the log odds rather than the estimates because the standard error is also on the log odd scale


# Calculate the confidence intervals for intercept

lower.bounds.int <- fixef(model.lat)[1] - z_score * std.error.int
upper.bounds.int <- fixef(model.lat)[1] + z_score * std.error.int

# SW: and THEN convert to probabilities:
plogis(lower.bounds.int)
#  0.1914759
plogis(upper.bounds.int)
# 0.30354


# Combine results into a data frame for better visualization
ci.int <- data.frame(
  Probability = probability.int,
  SE = std.error.int,
  Lower_Bound = plogis(lower.bounds.int),
  Upper_Bound = plogis(upper.bounds.int)
)

print(ci.int)
# Probability     SE Lower_Bound Upper_Bound
# (Intercept)   0.2431475 0.1556   0.1914759     0.30354

# SW - these are now the probabilities of using the right paw when pushing left (reference category)


# Calculate the confidence intervals for Modifer..1
lower.bounds.mod1 <- fixef(model.lat)[2] - z_score * std.error.mod1
upper.bounds.mod1 <- fixef(model.lat)[2] + z_score * std.error.mod1

# Combine results into a data frame for better visualization
ci.mod1 <- data.frame(
  Probability = probability.mod1,
  SE = std.error.mod1,
  Lower_Bound = plogis(lower.bounds.mod1),
  Upper_Bound = plogis(upper.bounds.mod1)
)

print(ci.mod1)
# 
# Probability     SE Lower_Bound Upper_Bound
# Modifier..1right lever   0.9369232 0.3071   0.8905463   0.9644345

#SW: probability of using the right paw when pushing right

## SW: Hmmm not quite - we already have extracted the probability of using the right paw on the right lever. (93.6% with CI 89.1-96.4%) - you can use exactly these numbers in the abstract. 

## Now we also need the left paw on the left lever. Above, we have the probability of using the right paw on the left lever as 

#             Probability     SE Lower_Bound Upper_Bound
# (Intercept)   0.2431475 0.1556   0.1914759     0.30354

# = 24.3%. To calculate the probability of using the left paw of the left lever, we simply substract this number from 100%. 

1-0.2431475=0.7568525 # = 75.7%
# The same goes for the confidence intervals - simply 1-lower bound and upper bound.

1-0.30354= 0.69646 # = 69.6% for upper bound
1-0.1914759 = 0.8085241 # = 80.9% for upper bound



## SW: I commented this out - there is no need for the *0.5





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

# repeatabilies can range from 0-1. Our result of 0.032 indicates very low repeatability. 

# looking at individua-level repeatability

# SW - ignore these parts for now! I need to look at it again

# Extract random effects
ranef_subj <- ranef(model.lat)$Subject


# Summarize random effects for each individual
head(ranef_subj)

# probabilty of 
hist(plogis(ranef_subj$`(Intercept)`))
hist(plogis(ranef_subj$`Modifier..1right lever`))

# 3) plotting graphs --------------------------------------------------- ---

library(ggplot2)

# Build the data frame
plot_data <- data.frame(
  Lever = rep(c("Left lever", "Right lever"), each = 2),
  Paw = rep(c("Right paw", "Left paw"), 2),
  Probability = c(0.2431, 0.7569, 0.9369, 0.0631),
  Lower = c(0.1915, 0.6965, 0.8905, 0.0356),
  Upper = c(0.3035, 0.8085, 0.9644, 0.1095)
)


#Graph

ggplot(plot_data, aes(x = Lever, y = Probability, fill = Paw)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("orange", "orange4")) +
  labs(title = "Predicted Probability of Paw Use by Lever Side",
       y = "Predicted Probability", x = "Lever Side") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
