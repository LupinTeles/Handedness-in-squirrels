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
Variables <- subset(Variables, subset=!(Variables$Modifier..2 %in% c("unknown", "head", "both"))) # adding the ! in front of %in% means 'remove all elements that are on this list'
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
length(Variables$Modifier..2[Variables$Modifier..2=="both paws"]) #760

# we are now running the model with just two outcome categories (left and right), meaning you can use the lme4 package

# much of the model specification remains the same - you can now use the 'binomial' model family

Variables$Modifier..1<- factor(Variables$Modifier..1)


Modifier..2 ~ Modifier..1 + (1|Subject)

model.lat <- glmer(Modifier..2 ~ Modifier..1 + (1|Subject), data=Variables, family = binomial)




# once it's run, we can look at repeatability. maybe you can google to see how far you get. but don't waste too much time - I can give you pointers :-)


