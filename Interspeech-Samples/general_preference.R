###############################
###    Install packages     ###
###############################
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(scales)
library(insight)
library(extrafont)
loadfonts()
library(knitr)
library(rmarkdown)
#options(knitr.table.format = 'markdown')


###############################
###     0 Preprocessing     ###
###############################

# 0a. Read in file of stimulus-question number correspondence since qualtrics exported question numbers 

filenames <- read.csv(file = '/Users/Johannah/Desktop/Interspeech_official/question_codes.tsv', sep = "\t", header=FALSE)

# 0b. Read in results file
genPrefRaw <- read.csv(file = '')
head(genPrefRaw)
colnames(genPrefRaw)

# 0c. Remove extraneous columns from qualtrics
genPrefRawTidy <- dplyr::select(genPrefRaw, -c(StartDate, EndDate, IPAddress, RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Q1, Q2, Status, post_question_1, post_question_3))

# 0d. Remove extraneous rows with column titles
genPrefRawTidy_2 <- genPrefRawTidy[-c(1, 2), ] 

# 0e. Convert to numeric
genPrefRawTidy_2$Duration..in.seconds. <- as.numeric(genPrefRawTidy_2$Duration..in.seconds.)
genPrefRawTidy_2$Progress <- as.numeric(genPrefRawTidy_2$Progress)
genPrefRawTidy_2$Finished <- as.numeric(genPrefRawTidy_2$Finished)

###############################
### 1 Check Participants    ###
###############################

# 1a. Check for only completed experiments as some people return after starting
genPrefRawTidy_2 <- genPrefRawTidy_2[genPrefRawTidy_2$Progress == 100 & genPrefRawTidy_2$Finished == 1, ]                          # Remove row based on condition

# -> 2 participants removed

# 1b. Check for participants not using headphones
genPrefRawTidy_2 <- genPrefRawTidy_2[genPrefRawTidy_2$Instructions. == 1, ]

# -> 1 participant

# 1c. Check issues with audio (post-questionnaire)
genPrefRawTidy_2 <- genPrefRawTidy_2[genPrefRawTidy_2$Q114 == 1, ]

# -> 1 participant

# 1d. Check issues with duration: total audio duration = 456 ms
genPrefRawTidy_2 <- genPrefRawTidy_2[genPrefRawTidy_2$Duration..in.seconds. > 500, ]

# -> 0 participants


###############################
###   2 Reformat data       ###
###############################


# 2a. Remove above columns
genPrefRawTidy_2 <- dplyr::select(genPrefRawTidy_2, -c(Instructions., Q114, Progress, Finished, Duration..in.seconds.))
colnames(genPrefRawTidy_2)

# 2b. Swap out generic question names for which stimulus type and number was heard
colnames(genPrefRawTidy_2) = filenames$V2[match(colnames(genPrefRawTidy_2), filenames$V1)]
colnames(genPrefRawTidy_2)

# 2c. Add Participant codes
genPrefRawTidy_2$participant <- 1:nrow(genPrefRawTidy_2)
genPrefRawTidy_2$participant <- sub("^", "PP_", genPrefRawTidy_2$participant)

# 2d. Reformat to long table
genPreflong <- pivot_longer(genPrefRawTidy_2, -c(participant), values_to = "Preference", names_to = "Stimulus")

# 2e Add column for whether stimulus type was question or answer as we are splitting these
genPreflong <- genPreflong %>%
  mutate(DAType = case_when(
    grepl("(question).*", genPreflong$Stimulus) ~ "Question",
    grepl("(answer).*", genPreflong$Stimulus) ~ "Answer"
  ))


###############################
###   3. Recode variables   ###
###############################


# 3a. Convert Stimulus, DAType and participant to factors
genPreflong$Stimulus <- as.factor(genPreflong$Stimulus)
genPreflong$DAType <- as.factor(genPreflong$DAType)
genPreflong$participant <- as.factor(genPreflong$participant)

# 3b. change values to 0 and 1 from 1 and 2
genPreflong$PreferenceCode <- mapvalues(genPreflong$Preference, from = c(1, 2), to = c(0, 1))
genPreflong$PreferenceCode <- as.factor(genPreflong$PreferenceCode)

# 3c. Make two dataframes for Question vs Answer 
questions <- genPreflong[genPreflong$DAType == 'Question', ]
answers <- genPreflong[genPreflong$DAType == 'Answer', ]


###############################
###     4. Basic Stats      ###
###############################

# Let's check for NAs

apply(questions, 2, function(x) any(is.na(x)))
apply(answers, 2, function(x) any(is.na(x)))

# None found

counts_q <- questions %>% 
  count(Preference) 

counts_a <- answers %>% 
  count(Preference)


###############################
### 5. Variance Comp model  ###
###############################

# 5a.i. glmer model questions with binomial logit dist
glm.q.0 <- glmer(PreferenceCode ~ 1 + (1|participant), data=questions, family=binomial(link="logit"))
glm.q.1 <- glmer(PreferenceCode ~ 1 + (1|participant) + (1|Stimulus), data=questions, family=binomial(link="logit"))

# Let's compare the model with a random effect of Stimulus
anova(glm.q.0, glm.q.1)

# Full random effects is significantly better and our distribution differs from chance
summary(glm.q.1)
coef(glm.q.1)

#Let's get the estimate of the intercept which is how much it deviates from a ratio of 0.5
# We have to convert this into probability.  This will differ from the actual preference percentage as it is corrected for variance due to random effects
plogis(0.4430)
#0.6089736

# 5a.ii. Let's get the 95% confidence intervals from our model
confint(glm.q.1, level = .95)
# (Intercept) 0.1777466 0.7150688

#and convert these
plogis(0.1777466) 
plogis(0.7150688)

#0.54432 and 0.6715202

# 5a.iii. How much variance are the random effects acounting for?
dotplot.ranef.mer(ranef(glm.q.1,condVar=TRUE))
ranef(glm.q.1,condVar=TRUE)
get_variance(glm.q.1)

# 5.b.i glmer model answers
glm.a.0 <- glmer(PreferenceCode ~ (1 | participant), data=answers, family=binomial(link="logit"))
glm.a.1 <- glmer(PreferenceCode ~ (1 | participant) + (1|Stimulus), data=answers, family=binomial(link="logit"))

anova(glm.a.0, glm.a.1)

# Full random effects is significantly better and our distribution differs from chance
summary(glm.a.1)
coef(glm.a.1)


#Let's get the estimate of the intercept which is how much it deviates from a ratio of 0.5
# We have to convert this into probability. This will differ from the actual preference percentage as it is corrected for variance due to random effects
plogis(-0.2142)
#0.4466538

# 5b.ii. Let's get the 95% confidence intervals
confint(glm.a.1, level = .95)
#(Intercept) -0.4466080 0.01564039

plogis(-0.4466080)  #converting a logistic coefficient to a probability of success
plogis(0.01564039)

# 0.3901675 0.50391

# 5b.iii. How much variance are the random effects acounting for?
dotplot.ranef.mer(ranef(glm.a.1,condVar=TRUE))
ranef(glm.a.1,condVar=TRUE)
get_variance(glm.a.1)



###############################
###     6. Make Plots       ###
###############################


p <- ggplot(genPreflong, aes(DAType, fill = PreferenceCode)) + 
  geom_bar(position = "fill" , color="black", width=0.9, size=0.75) +
  labs(x="Sentence Type", y= "Which of the following do you prefer (%)?", fill = "System", labels = c("Baseline", "Datamix"), y=NULL) +
  coord_flip()  +
  geom_hline(yintercept = 0.5, linetype = "dashed", size=1) +
  theme(aspect.ratio = 1/5, text=element_text(size=15,  family="Times New Roman"),
                                              axis.text.x = element_text(size=15, colour = "black"),
                                              axis.text.y = element_text(size=15, angle = 360, hjust=0.5, vjust=0.5, colour = "black"),
                                              axis.ticks.y = element_blank(),
                                              panel.grid.major = element_blank(),
                                              panel.grid.minor = element_blank(),
                                              panel.background = element_blank()) + 
  theme(axis.line.x = element_line(color="black", size = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 15, b = 0, l = 0))) +
  scale_y_continuous(labels = function(x) format(x*100, digits=0, nsmall=0)) +
  scale_fill_manual(name = "Model", labels = c("Baseline", "Datamix"), values = alpha(c("white","#56B4E9"), .4))

p 

q <- p + annotate(geom="text", x=c(2,1), y=c(0.25, 0.25), label=c("59.5%*", "45.1%"),
             color="Black", family="Times New Roman", size=5)
q


ggsave("general_preference.png", width = 6 , height = 2)


knitr::stitch_rhtml('general_preference.R')

