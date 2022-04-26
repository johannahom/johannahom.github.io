###############################
###    Install packages     ###
###############################
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(scales)
library(ordinal)
library(MASS)
library(rcompanion)
library(brant)
library(lattice)
library(car)
library(RVAideMemoire)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(splines)
library(ggeffects)

###############################
###     0 Preprocessing     ###
###############################

# 0. Read in files (List 1 and 2)
mosList1 <- read.csv(file = '')
mosList2 <- read.csv(file = '')

head(mosList1)
head(mosList2)
colnames(mosList2)
colnames(mosList2)
summary(mosList1)
summary(mosList2)


# 0a. Remove extraneous columns
mosList1 <- dplyr::select(mosList1, -c(StartDate, EndDate, IPAddress, RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Q2, Status, Q2.1, Q108, Q1))
mosList2 <- dplyr::select(mosList2, -c(StartDate, EndDate, IPAddress, RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Q2, Status, Q2.1, Q1.2, Q1))

colnames(mosList1)
colnames(mosList2)

# 0b. Remove extraneous rows with column titles from qualtrics
mosList1 <- mosList1[-c(1, 2), ] 
mosList2 <- mosList2[-c(1, 2), ] 


###############################
### 1 Check Participants    ###
###############################

# 1a Allow only completed experiments
mosList1 <- mosList1[mosList1$Progress == 100 & mosList1$Finished == 1, ]
# --> 1 removed
mosList2 <- mosList2[mosList2$Progress == 100 & mosList2$Finished == 1, ]
# --> 3 removed

# 1b Only allow those using headphones
mosList1 <- mosList1[mosList1$Q1.1 == 1, ]
# --> 1 removed
mosList2 <- mosList2[mosList2$Q1.1 == 1, ]
# --> 1 removed

# 1c Check for being too quick based on audio duration (300 seconds) 
mosList1$Duration..in.seconds. <- as.numeric(mosList1$Duration..in.seconds.)
mosList2$Duration..in.seconds. <- as.numeric(mosList2$Duration..in.seconds.)

mosList1 <- mosList1[mosList1$Duration..in.seconds. > 300, ]
mosList2 <- mosList2[mosList2$Duration..in.seconds. > 300, ]

# -> 0 removed


###############################
###   2 Reformat data       ###
###############################

# 2a Remove above columns
mosList1 <- dplyr::select(mosList1, -c(Q1.1, Progress, Finished, Duration..in.seconds.))
mosList2 <- dplyr::select(mosList2, -c(Q1.1, Progress, Finished, Duration..in.seconds.))


# 2b add participant codes
mosList1$participant <- 1:nrow(mosList1)
mosList1$participant <- sub("^", "PP1_", mosList1$participant)

mosList2$participant <- 1:nrow(mosList2)
mosList2$participant <- sub("^", "PP2_", mosList2$participant)

# 2c Convert participant to factor
mosList1$participant <- as.factor(mosList1$participant)
mosList2$participant <- as.factor(mosList2$participant)

# 2d Reformat tables
mosList1Long <- pivot_longer(mosList1, -c(participant), values_to = "MOSScore", names_to = "Stimulus")
mosList2Long <- pivot_longer(mosList2, -c(participant), values_to = "MOSScore", names_to = "Stimulus")

# 2e Add stimulus information

# Baseline/Datamix
mosList1Long <- mosList1Long %>%
  mutate(System = case_when(
    grepl(".*(baseline)", mosList1Long$Stimulus) ~ "Baseline",
    grepl(".*(datamix)", mosList1Long$Stimulus) ~ "Datamix"
  ))

mosList2Long <- mosList2Long %>%
  mutate(System = case_when(
    grepl(".*(baseline)", mosList2Long$Stimulus) ~ "Baseline",
    grepl(".*(datamix)", mosList2Long$Stimulus) ~ "Datamix"
  ))

# Question/Answer
mosList1Long <- mosList1Long %>%
  mutate(DAType = case_when(
    grepl(".*(question)", mosList1Long$Stimulus) ~ "Question",
    grepl(".*(answer)", mosList1Long$Stimulus) ~ "Answer"
  ))

mosList2Long <- mosList2Long %>%
  mutate(DAType = case_when(
    grepl(".*(question)", mosList2Long$Stimulus) ~ "Question",
    grepl(".*(answer)", mosList2Long$Stimulus) ~ "Answer"
  ))


# 2f Combine both lists into one dataframe
mosscores <- rbind(mosList1Long, mosList2Long)

# 2g Convert stimulus name into basename
mosscores$Stimulus <- gsub("_1$", "", mosscores$Stimulus)
mosscores$StimulusCode <- gsub("^(baseline|datamix).(question|answer)_", "", mosscores$Stimulus)
mosscores$StimulusCode <- gsub("^(baseline|datamix).", "", mosscores$Stimulus)


###############################
###   3. Recode variables   ###
###############################


# 3a Change variables to factors
mosscores$Stimulus <- as.factor(mosscores$Stimulus)
mosscores$System <- as.factor(mosscores$System)
mosscores$MOSScore <- as.numeric(mosscores$MOSScore)
mosscores$DAType <- as.factor(mosscores$DAType)
mosscores$participant <- as.factor(mosscores$participant)
mosscores$MOSScore.f = factor(mosscores$MOSScore,
                              ordered = TRUE) #converting MOS score to ordered factor


###############################
###     4. Basic Stats      ###
###############################

# 4a Check for N/A
apply(mosscores, 2, function(x) any(is.na(x)))
#--> all good

#4b Total descriptive stats per system
mosscores%>%
  group_by(System)%>% 
  summarise(Mean=mean(MOSScore), Max=max(MOSScore), Min=min(MOSScore), Median=median(MOSScore), Std=sd(MOSScore))

#4c Descriptive stats per combination of variables system x DAType
mosscores%>%
  group_by(System, DAType)%>% 
  summarise(Mean=mean(MOSScore), Max=max(MOSScore), Min=min(MOSScore), Median=median(MOSScore), Std=sd(MOSScore))

#4d Plot the MOS scores per DAType

#Make two dataframes for Question vs Answer 
questions <- mosscores[mosscores$DAType == 'Question', ]
answers <- mosscores[mosscores$DAType == 'Answer', ]

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Questions
questions %>% mutate(MOSScore.f = ordered(MOSScore.f, levels=rev(levels(MOSScore.f)))) %>% 
  ggplot( aes(x = System, fill = MOSScore.f)) + geom_bar(position = "fill",  color="black", width=0.9, size=0.75) +
  facet_grid(.~DAType) + scale_fill_manual(values = alpha(cbPalette, .5)) + theme_minimal() +
  labs(y= "Percentage Chosen (%)", x= "Model", fill="MOS Score") +
  theme(aspect.ratio = 1/5, text=element_text(size=15,  family="Times New Roman"),
        axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_text(size=15, angle = 360, hjust=0.5, vjust=0.5, colour = "black"),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_flip()  +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(labels = function(x) format(x*100, digits=0, nsmall=0)) +
  theme(axis.line.x = element_line(color="black", size = 0.5)) +                               # Remove labels from facet plot
  theme(strip.text.x = element_blank())

ggsave("MOS_Score_Questions.png", width = 6 , height = 1.4)


# Answers
answers %>% mutate(MOSScore.f = ordered(MOSScore.f, levels=rev(levels(MOSScore.f)))) %>% 
  ggplot( aes(x = System, fill = MOSScore.f)) + geom_bar(position = "fill",  color="black", width=0.9, size=0.75) +
  facet_grid(.~DAType) + scale_fill_manual(values = alpha(cbPalette, .5)) + theme_minimal() +
  labs(y= "Percentage Chosen (%)", x= "Model", fill="MOS Score") + 
  theme(aspect.ratio = 1/5, text=element_text(size=15,  family="Times New Roman"),
        axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_text(size=15, angle = 360, hjust=0.5, vjust=0.5, colour = "black"),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_flip()  +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(labels = function(x) format(x*100, digits=0, nsmall=0)) +
  theme(axis.line.x = element_line(color="black", size = 0.5)) +                               # Remove labels from facet plot
  theme(strip.text.x = element_blank())

ggsave("MOS_Score_Answers.png", width = 6 , height = 1.4)


#all together
mosscores %>% mutate(MOSScore.f = ordered(MOSScore.f, levels=rev(levels(MOSScore.f)))) %>% 
  ggplot( aes(x = System, fill = MOSScore.f)) + geom_bar(position = "fill",  color="black", width=0.9, size=0.75) +
  facet_grid(.~DAType) + scale_fill_manual(values = cbPalette) + theme_minimal() +
  labs(y= "Percentage Chosen (%)", fill="MOS Score") +
  theme(aspect.ratio = 1/3, text=element_text(size=12,  family="Times New Roman"),
        axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, angle = 360, hjust=0.5, vjust=0.5, colour = "black"),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  coord_flip()  +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(labels = function(x) format(x*100, digits=0, nsmall=0)) +
  theme(axis.line.x = element_line(color="black", size = 0.5)) 


ggsave("MOS_test.png", width = 6 , height = 2)


################################
## 5. Ordinal regression      ##
################################

# http://people.vcu.edu/~dbandyop/BIOS625/CLM_R.pdf
# https://aclanthology.org/2021.emnlp-main.703.pdf 


# MODEL ASSUMPTIONS (from https://www.st-andrews.ac.uk/media/ceed/students/mathssupport/ordinal%20logistic%20regression.pdf)
# Assumption 1: DV is ordinal level. [x]
# Assumption 2: IVs are (continuous|categorical|ordinal) --> our IVs are categorical
# Assumption 3: No Multi-collinearity (We only have one IV)
# Assumption 4: Proportional odds "that each independent variable has an identical effect at each cumulative split"
# Assumption 5: Scale effects


# 1. Check whether thresholds are flexible or equidistant (default is flexible)

#The default option is "flexible", which corresponds to the conventional ordered, 
#but otherwise unstructured thresholds. The "symmetric" option restricts the thresholds 
#to be symmetric while the "equidistant" option restricts the thresholds to be equally spaced. (Taken directly from http://people.vcu.edu/~dbandyop/BIOS625/CLM_R.pdf)

# Fitting a non-mixed effects model as assumptions cannot be checked for mixed effects yet in package
model.clma = clm(MOSScore.f ~ System + DAType,
                data = mosscores,
                threshold = "equidistant")

model.clmb = clm(MOSScore.f ~ System + DAType,
                data = mosscores,
                threshold = "flexible")

anova(model.clma,model.clmb)

# Significant evidence that thresholds are flexible so we will continue with that parameter


#Let's test assumptions of model without mixed effects 
nominal_test(model.clmb)
scale_test(model.clmb)

#All model assumptions are met so we can go on with this analysis
#Now that we have established that we meet the assumptions and are using the default "flexible" we can go on.

#Checking random effects structure
clm.0 <- clmm(MOSScore.f ~ 1 + (1|participant), data=mosscores, threshold = "flexible")
clm.1 <- clmm(MOSScore.f ~ 1 + (1|participant) + (1|Stimulus), data=mosscores, threshold = "flexible")
clm.1a <- clmm(MOSScore.f ~ 1 + (System|participant) + (1|Stimulus), data=mosscores, threshold = "flexible")
clm.1b <- clmm(MOSScore.f ~ 1 + (System|participant) + (System|Stimulus), data=mosscores, threshold = "flexible") #

anova(clm.0, clm.1) #clm.1 significantly better
anova(clm.1, clm.1a) #clm1.a significantly better
anova(clm.1a, clm.1b) #clm1.b not significantly better

#We therefore use the random effects structure of  (System|participant) + (1|Stimulus)


# Let's now construct the maximal model with our chosen random effects structure
clm.2.max <- clmm(MOSScore.f ~ System * DAType + (System|participant) + (1|Stimulus), data=mosscores, threshold = "flexible")
summary(clm.2.max)

#Check significance of interaction using a model without the interaction
clm.2 <- clmm(MOSScore.f ~ System + DAType + (System|participant) + (1|Stimulus), data=mosscores, threshold = "flexible")

#Log-likelihood tests of model with and without interaction
anova(clm.2.max,clm.2)

# The model with an interaction is significantly better according to above test.


# Test significant of fixed effects System by comparing this model to clm.2 (i.e. model without interaction)
clm.3 <- clmm(MOSScore.f ~ DAType + (System|participant) + (1|Stimulus), data=mosscores)

anova(clm.2,clm.3)

#System is not significant according to LLT above
#method="Wald"

# Test significant of fixed effects DAType
clm.4 <- clmm(MOSScore.f ~ System + (System|participant) + (1|Stimulus), data=mosscores, threshold = "flexible")
anova(clm.2,clm.4)

#DAType is not significant according to LLT above


# Let's get the confidence intervals for our model
confint(clm.2.max, level =0.95)

#To get the log odds CIs we have to exponentiate the above CIs


#### PLOTTING ####

# Create new columns for plotting histograms
mosscores <- mosscores %>% 
  unite(conditions, c("System", "DAType"))

#Histograms
histogram(~ MOSScore.f | conditions,
          data=mosscores,
          layout=c(2,2)      #  columns and rows of individual plots
)


#Lets plot the predicted probabilities based on our chosen model. This will let us visualise what our interaction means.
#https://stats.stackexchange.com/questions/302592/interpreting-interaction-terms-and-main-effects-in-logit-regression-with-multipl
#https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html plot marginal effects

r <- plot_model(clm.2.max, type = "int", colors = c("#E69F00", "#0072B2"), legend.title="Sentence Type") + 
  theme_sjplot(base_size = 15, base_family = "Times New Roman")
r + labs(y = "Predicted Probability", x = "Model") +ggtitle("Predicted Probability of MOS Score") +
  theme(axis.text.x = element_text(size=8, colour = "black"),
        axis.text.y = element_text(size=15, colour = "black"),
        axis.ticks.y = element_blank()) + theme(legend.position = c(0.8, 0.25)) 

ggsave("Predicted_Probabilities.png", height =5 , width = 8 )

plot_model(clm.2.max) #plotting fixed effects

