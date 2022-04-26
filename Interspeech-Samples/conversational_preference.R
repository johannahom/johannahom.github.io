###############################
###    Install packages     ###
###############################
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(scales)
library(insight)
library(extrafont)
library(tidyverse)
loadfonts()
fonts()
library(knitr)
library(rmarkdown)

###############################
###     0 Preprocessing     ###
###############################

# 0a. Read in file of stimulus-question number correspondence since qualtrics exported question numbers 

filenames <- read.csv(file = '/Users/Johannah/Desktop/Interspeech_official/question_codes.tsv', sep = "\t", header=FALSE)


# 0b. Read in results file
convPrefRaw <- read.csv(file = '')
head(convPrefRaw)
colnames(convPrefRaw)


# 0c. Remove extraneous columns from qualtrics
convPrefRawTidy <- dplyr::select(convPrefRaw, -c(StartDate, EndDate, IPAddress, RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Q1, Q2, Status, post_question_1, post_question_2, post_question_3))

# 0d. Remove extraneous rows with column titles
convPrefRawTidy_2 <- convPrefRawTidy[-c(1, 2), ] 

# 0e. Convert to numeric
convPrefRawTidy_2$Duration..in.seconds. <- as.numeric(convPrefRawTidy_2$Duration..in.seconds.)
convPrefRawTidy_2$Progress <- as.numeric(convPrefRawTidy_2$Progress)
convPrefRawTidy_2$Finished <- as.numeric(convPrefRawTidy_2$Finished)

###############################
### 1 Check Participants    ###
###############################


# 1a. Check for only completed experiments as some people return after starting
convPrefRawTidy_2 <- convPrefRawTidy_2[convPrefRawTidy_2$Progress == 100 & convPrefRawTidy_2$Finished == 1, ]                          # Remove row based on condition

# -> 2 participants

# 1b. Check for participants not using headphones
convPrefRawTidy_2 <- convPrefRawTidy_2[convPrefRawTidy_2$Instructions. == 1, ]

# -> 1 participant

# 1c. Check issues with audio (post-questionnaire)
convPrefRawTidy_2 <- convPrefRawTidy_2[convPrefRawTidy_2$Q114 == 1, ]

# -> 1 participant

# 1d. Check issues with duration: total audio duration = 456 ms
convPrefRawTidy_2 <- convPrefRawTidy_2[convPrefRawTidy_2$Duration..in.seconds. > 500, ]

# -> 1 participant

###############################
###   2 Reformat data       ###
###############################

# 2a. Remove above columns
convPrefRawTidy_2 <- dplyr::select(convPrefRawTidy_2, -c(Instructions., Q114, Progress, Finished, Duration..in.seconds.))

# 2b. Swap out generic question names for which stimulus type and number was heard
colnames(convPrefRawTidy_2) = filenames$V2[match(colnames(convPrefRawTidy_2), filenames$V1)]
colnames(convPrefRawTidy_2)

# 2c. Add Participant codes
convPrefRawTidy_2$participant <- 1:nrow(convPrefRawTidy_2)
convPrefRawTidy_2$participant <- sub("^", "PP_", convPrefRawTidy_2$participant)

# 2d. Reformat to long table
convPreflong <- pivot_longer(convPrefRawTidy_2, -c(participant), values_to = "Preference", names_to = "Stimulus")

# 2e Add column for whether stimulus type was question or answer as we are splitting these
convPreflong <- convPreflong %>%
  mutate(DAType = case_when(
    grepl("(question).*", convPreflong$Stimulus) ~ "Question",
    grepl("(answer).*", convPreflong$Stimulus) ~ "Answer"
  ))

###############################
###   3. Recode variables   ###
###############################

# 3a. Convert Stimulus, DAType and participant to factors
convPreflong$Stimulus <- as.factor(convPreflong$Stimulus)
convPreflong$DAType <- as.factor(convPreflong$DAType)
convPreflong$participant <- as.factor(convPreflong$participant)

# 3b. change values to 0 and 1 from 1 and 2
convPreflong$PreferenceCode <- mapvalues(convPreflong$Preference, from = c(1, 2), to = c(0, 1))
convPreflong$PreferenceCode <- as.factor(convPreflong$PreferenceCode)

# 3c. Make two dataframes for Question vs Answer 
questions <- convPreflong[convPreflong$DAType == 'Question', ]
answers <- convPreflong[convPreflong$DAType == 'Answer', ]


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

# Original idea for using logistic regression https://rpubs.com/palday/glm-test

# 5a.i. glmer model questions with binomial logit dist
glm.q.0 <- glmer(PreferenceCode ~ 1 + (1|participant), data=questions, family=binomial(link="logit"))

glm.q.1 <- glmer(PreferenceCode ~ 1 +(1|participant) + (1|Stimulus), data=questions, family=binomial(link="logit"))

# Let's compare the model with a random effect of Stimulus
anova(glm.q.0, glm.q.1)

# Full random effects is significantly better and our distribution differs from chance
summary(glm.q.1)

coef(glm.q.1)

#Let's get the estimate of the intercept which is how much it deviates from a ratio of 0.5
# We have to convert this into a probability This will differ from the actual preference percentage as it is corrected for variance due to random effects
plogis(0.4678)
#0.6148629

# 5a.ii. Let's get the 95% confidence intervals from our model
confint(glm.q.1, level = .95)
# (Intercept) 0.2365486 0.7042300

#and convert these
plogis(0.2365486) 
plogis(0.7042300)

#0.5588629 and 0.6691249


# 5a.iii. How much variance are the random effects acounting for?
dotplot.ranef.mer(ranef(glm.q.1,condVar=TRUE))
ranef(glm.q.1,condVar=TRUE)
get_variance(glm.q.1)

# We can have a look at individual items later



# 5.b.i glmer model answers
glm.a.0 <- glmer(PreferenceCode ~ (1 | participant), data=answers, family=binomial(link="logit"))
glm.a.1 <- glmer(PreferenceCode ~ (1 | participant) + (1|Stimulus), data=answers, family=binomial(link="logit"))

anova(glm.a.0, glm.a.1)

# Full random effects is significantly better and our distribution differs from chance
summary(glm.a.1)
coef(glm.a.1)


#Let's get the estimate of the intercept which is how much it deviates from a ratio of 0.5
# We have to convert this into probability.  This will differ from the actual preference percentage as it is corrected for variance due to random effects
plogis(-0.1692)
#0.4578006

# 5b.ii. Let's get the 95% confidence intervals
confint(glm.a.1, level = .95)
#(Intercept) -0.3722900 0.03205358


plogis(-0.3722900)  #converting a logistic coefficient to a probability of success
plogis(0.03205358)

# 0.4079878 0.5080127


# 5b.iii. How much variance are the random effects acounting for?
dotplot.ranef.mer(ranef(glm.a.1,condVar=TRUE))
ranef(glm.a.1)
get_variance(glm.a.1)


###############################
###     6. Make Plots       ###
###############################



p <- ggplot(convPreflong, aes(DAType, fill = PreferenceCode)) + 
  geom_bar(position = "fill" , color="black", width=0.9, size=0.75) +
  labs(x="Sentence Type", y= "Which of the following sounds the most conversational? (%)", fill = "System", labels = c("Baseline", "Datamix"), y=NULL) +
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
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(labels = function(x) format(x*100, digits=0, nsmall=0)) +
  scale_fill_manual(name = "Model", labels = c("Baseline", "Datamix"), values = alpha(c("white","#56B4E9"), .4))

p 

q <- p + annotate(geom="text", x=c(2,1), y=c(0.25, 0.25), label=c("60.5%*", "46%"),
                  color="Black", family="Times New Roman", size=5)
q



ggsave("conversational_preference.png", width = 6 , height = 2, plot=q)
embed_fonts("conversational_preference.pdf", outfile="conversational_preference.pdf")


knitr::stitch_rhtml('conversational_preference.R')
