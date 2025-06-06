#############################################################################################
#############################################################################################

# STATISTICAL ANALYSES EXPERIMENT 1
# 02/09/2020

# **TABLE OF CONTENTS**
# SECTION 1: DATA PREPARATION
# SECTION 2: DESCRIPTIVE DATA
# SECTION 3: MAIN ANALYSIS (PRE-REG - reported in main manuscript)
# SECTION 4: POST HOC ANALYSES (NOT PRE-REG - reported in main manuscript)
# SECTION 5: VISUALIZATION (included in main manuscript)
# SECTION 6: EXPLORATORY ANALYSES (NOT PRE-REG - reported in supplemental materials)

#############################################################################################
#############################################################################################

# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# set working directory
setwd("") #enter path in ""

# source functions
source("") # enter path for 'diagnostic_fcns.r' in ""

# Set directories
recs_dir <- "./PreProcessedTables/"

# Load packages
library("ggplot2")
library("lme4")
library("exactRankTests")
library("emmeans")
library("MuMIn")
library("tidyverse")  # attaches purrr and readr
library("fs")

# reads all files in recs folder
flist <- list.files(recs_dir)

#############################################################################################
# SECTION 1: DATA PREPARATION
#############################################################################################

# -------------------------------------------------------------------------------------------
# STEP 1: Create Table for Main Analysis (One overall table with 16 rows per participant)
# -------------------------------------------------------------------------------------------
# Create new summary dataframe for overall data

# Read in tsv files from pre-processing folder
overall.data <- "PreProcessedTables"
tsv_files <- fs::dir_ls(overall.data, regexp = "\\.tsv$")
tsv_files

## Creating data frame
overall.data <- tsv_files %>%
  map(read_tsv) %>%    # read in all the files individually, using the function read_tsv() from the readr package
  reduce(rbind)        # reduce with rbind into one dataframe

#write.csv(overall.data, "overall.data.csv", row.names = FALSE)

#Subsets if you only want to look at subsets (e.g. age or first trial only)
#overall.data <- subset(overall.data, overall.data$Condition=="Con4")
#overall.data <- overall.data[overall.data$Trial == "1" | overall.data$Trial == "2" | overall.data$Trial == "3" | overall.data$Trial == "4",]

# -------------------------------------------------------------------------------------------
# STEP 2: Filter data in overall.data Table
# -------------------------------------------------------------------------------------------
# Filter 1: Exclude if child does not look at the screen for at least one fixation during the relevant phases
overall.data$PrefLook_LT_Object_Nov_PROP[overall.data$InterPhase_Checker_valid==0.00] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
# Filter 2: Exclude trials during which child has not looked at the screen at all during the preferential looking task
overall.data$trial_valid <-ifelse(overall.data$PrefLook_LT_Object_Nov_PROP >= 0, 1, 0) # create trial_valid column, a trial is valid if Prop score is >=0. due to the previous step exclusion do to insufficient looking in relevant phases is considered already.
overall.data$trial_valid[is.na(overall.data$trial_valid)] <- 0 #replace NAs with 0 for averaging in the next steps
# Optional Filter 3: Exclude trials if child looks only at one object during the prefernetial looking task
##overall.data$PrefLook_LT_Object_Nov_PROP[overall.data$PrefLook_LT_Object_Nov_PROP==0.00] <- NA   #replace 0 with NAs
##overall.data$PrefLook_LT_Object_Nov_PROP[overall.data$PrefLook_LT_Object_Nov_PROP==1.00] <- NA   #replace 0 with NAs
## Optional Filter 4: Exclude trials during which child has not looked at the object at all
#overall.data$InterPhase_ObjectOriginFaceTotal_Gazing [overall.data$InterPhase_LF_Object_Gazing==0.00] <- NA # do not consider zero socially caused looks if the child has not performed any look to the object
#overall.data$InterPhase_LF_Object_Gazing [overall.data$InterPhase_LF_Object_Gazing==0.00] <- NA # do not consider

# -------------------------------------------------------------------------------------------
# STEP 3: Create Table for Post Hoc Analyses (1 row per condition = 4 rows per participant)
# -------------------------------------------------------------------------------------------
subject_list <- unique(overall.data$ID)
condition_list <- c("Con1", "Con2", "Con3", "Con4")
mean.overall.data <- data.frame(matrix(NA, nrow = length(subject_list) * 4, ncol = 0), stringsAsFactors = FALSE)

mean.overall.data_ID_vector <- c()
mean.overall.data_Cond_vector <- c()
Inter_LT_FacesObject_PROP_vector <- c()
Inter_LT_FacesTotal_vector <- c()
Inter_LT_Faces_PROP_vector <- c()
Inter_LT_Object_vector <- c()
Inter_LT_Object_PROP_vector <- c()
InterPhase_LT_FaceTotal_Soc_Total_vector <- c()
InterPhase_LT_FaceTotal_Soc_Total_PROP_vector <- c()
InterPhase_LT_FaceTotal_Gazing_vector <- c()
InterPhase_LT_Object_Gazing_vector <- c()
InterPhase_LT_Object_Gazing_PROP_vector <- c()
PrefLook_LT_Object_Nov_PROP_vector <- c()
InterPhase_LF_FacesTotal_Begin_vector <- c()
InterPhase_LF_FacesTotal_End_vector <- c()
InterPhase_LF_FacesTotal_vector <- c()
Overall_LT_ScreenFam_vector <- c()
Inter_PropOverall_LT_Screen_vector <- c()
trial_valid_vector <- c()
InterPhase_LT_FacesObject_PROP_vector <- c()
InterPhase_LT_FacesObject_Gazing_PROP_vector <- c()
Inter_ObjectOriginFaceTotal_vector <- c()
Inter_FacesGazeshiftsRightTotal_vector <- c()
InterPhase_FacesGazeshiftsRightTotal_Begin_vector <- c()
InterPhase_FacesGazeshiftsRightTotal_End_vector <- c()
InterPhase_FacesGazeshiftsRightTotal_TOTAL_vector <- c()
InterPhase_ObjectOriginFaceTotal_Gazing_vector <- c()
Inter_ObjectOriginFaceTotal_Gazing_SocialPROP_vector <- c()
InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP_vector <- c()
InterPhase_LF_Object_Gazing_vector <- c()
InterPhase_ObjectOriginNonSocial_vector <- c()

# within current subject (i) and within current condition (j)
for (i in subject_list) {
  for (j in condition_list) {

    mean.overall.data_ID_vector <- c(mean.overall.data_ID_vector, i)
    mean.overall.data_Cond_vector <- c(mean.overall.data_Cond_vector, j)

    Inter_LT_FacesObject_PROP_vector <- c(Inter_LT_FacesObject_PROP_vector, mean(
      overall.data$Inter_LT_FacesObject_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_LT_FacesTotal_vector <- c(Inter_LT_FacesTotal_vector, mean(
      overall.data$Inter_LT_FacesTotal[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_LT_Faces_PROP_vector <- c(Inter_LT_Faces_PROP_vector, mean(
      overall.data$Inter_LT_Faces_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_LT_Object_vector <- c(Inter_LT_Object_vector, mean(
      overall.data$Inter_LT_Object[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_LT_Object_PROP_vector <- c(Inter_LT_Object_PROP_vector, mean(
      overall.data$Inter_LT_Object_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_FaceTotal_Soc_Total_vector <- c(InterPhase_LT_FaceTotal_Soc_Total_vector, mean(
      overall.data$InterPhase_LT_FaceTotal_Soc_Total[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_FaceTotal_Soc_Total_PROP_vector <- c(InterPhase_LT_FaceTotal_Soc_Total_PROP_vector, mean(
      overall.data$InterPhase_LT_FaceTotal_Soc_Total_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_FaceTotal_Gazing_vector <- c(InterPhase_LT_FaceTotal_Gazing_vector, mean(
      overall.data$InterPhase_LT_FaceTotal_Gazing[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_Object_Gazing_vector <- c(InterPhase_LT_Object_Gazing_vector, mean(
      overall.data$InterPhase_LT_Object_Gazing[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_Object_Gazing_PROP_vector <- c(InterPhase_LT_Object_Gazing_PROP_vector, mean(
      overall.data$InterPhase_LT_Object_Gazing_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))


    PrefLook_LT_Object_Nov_PROP_vector <- c(PrefLook_LT_Object_Nov_PROP_vector, mean(
      overall.data$PrefLook_LT_Object_Nov_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    trial_valid_vector <- c(trial_valid_vector, sum(
      overall.data$trial_valid[which(overall.data$ID == i & overall.data$Condition == j)]))

    InterPhase_LF_FacesTotal_Begin_vector <- c(InterPhase_LF_FacesTotal_Begin_vector, mean(
      overall.data$InterPhase_LF_FacesTotal_Begin[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LF_FacesTotal_End_vector <- c(InterPhase_LF_FacesTotal_End_vector, mean(
      overall.data$InterPhase_LF_FacesTotal_End[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Overall_LT_ScreenFam_vector <- c(Overall_LT_ScreenFam_vector, mean(
      overall.data$Overall_LT_ScreenFam[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_PropOverall_LT_Screen_vector <- c(Inter_PropOverall_LT_Screen_vector, mean(
      overall.data$Inter_PropOverall_LT_Screen[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LF_FacesTotal_vector <- c(InterPhase_LF_FacesTotal_vector, mean(
      overall.data$InterPhase_LF_FacesTotal[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_FacesObject_PROP_vector <- c(InterPhase_LT_FacesObject_PROP_vector, mean(
      overall.data$InterPhase_LT_FacesObject_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LT_FacesObject_Gazing_PROP_vector <- c(InterPhase_LT_FacesObject_Gazing_PROP_vector, mean(
      overall.data$InterPhase_LT_FacesObject_Gazing_PROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_ObjectOriginFaceTotal_vector <- c(Inter_ObjectOriginFaceTotal_vector, mean(
      overall.data$Inter_ObjectOriginFaceTotal[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_FacesGazeshiftsRightTotal_vector <- c(Inter_FacesGazeshiftsRightTotal_vector, mean(
      overall.data$Inter_FacesGazeshiftsRightTotal[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_FacesGazeshiftsRightTotal_Begin_vector <- c(InterPhase_FacesGazeshiftsRightTotal_Begin_vector, mean(
      overall.data$InterPhase_FacesGazeshiftsRightTotal_Begin[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_FacesGazeshiftsRightTotal_End_vector <- c(InterPhase_FacesGazeshiftsRightTotal_End_vector, mean(
      overall.data$InterPhase_FacesGazeshiftsRightTotal_End[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_FacesGazeshiftsRightTotal_TOTAL_vector <- c(InterPhase_FacesGazeshiftsRightTotal_TOTAL_vector, mean(
      overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_ObjectOriginFaceTotal_Gazing_vector <- c(InterPhase_ObjectOriginFaceTotal_Gazing_vector, mean(
      overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    Inter_ObjectOriginFaceTotal_Gazing_SocialPROP_vector <- c(Inter_ObjectOriginFaceTotal_Gazing_SocialPROP_vector, mean(
      overall.data$Inter_ObjectOriginFaceTotal_Gazing_SocialPROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP_vector <- c(InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP_vector, mean(
      overall.data$InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_LF_Object_Gazing_vector <- c(InterPhase_LF_Object_Gazing_vector, mean(
      overall.data$InterPhase_LF_Object_Gazing[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))

    InterPhase_ObjectOriginNonSocial_vector <- c(InterPhase_ObjectOriginNonSocial_vector, mean(
      overall.data$InterPhase_ObjectOriginNonSocial[which(overall.data$ID == i & overall.data$Condition == j)]
      , na.rm = TRUE))


  }
}

mean.overall.data$ID <- mean.overall.data_ID_vector
mean.overall.data$Condition <- mean.overall.data_Cond_vector
mean.overall.data$Inter_LT_FacesObject_PROP <- Inter_LT_FacesObject_PROP_vector
mean.overall.data$Inter_LT_FacesTotal <- Inter_LT_FacesTotal_vector
mean.overall.data$Inter_LT_Faces_PROP<- Inter_LT_Faces_PROP_vector
mean.overall.data$Inter_LT_Object <- Inter_LT_Object_vector
mean.overall.data$Inter_LT_Object_PROP <- Inter_LT_Object_PROP_vector
mean.overall.data$InterPhase_LT_FaceTotal_Soc_Total <- InterPhase_LT_FaceTotal_Soc_Total_vector
mean.overall.data$InterPhase_LT_FaceTotal_Soc_Total_PROP <- InterPhase_LT_FaceTotal_Soc_Total_PROP_vector
mean.overall.data$InterPhase_LT_FaceTotal_Gazing <- InterPhase_LT_FaceTotal_Gazing_vector
mean.overall.data$InterPhase_LT_Object_Gazing <- InterPhase_LT_Object_Gazing_vector
mean.overall.data$InterPhase_LT_Object_Gazing_PROP <- InterPhase_LT_Object_Gazing_PROP_vector
mean.overall.data$PrefLook_LT_Object_Nov_PROP <- PrefLook_LT_Object_Nov_PROP_vector
mean.overall.data$InterPhase_LF_FacesTotal_Begin <- InterPhase_LF_FacesTotal_Begin_vector
mean.overall.data$InterPhase_LF_FacesTotal_End <- InterPhase_LF_FacesTotal_End_vector
mean.overall.data$InterPhase_LF_FacesTotal <- InterPhase_LF_FacesTotal_vector
mean.overall.data$Overall_LT_ScreenFam <- Overall_LT_ScreenFam_vector
mean.overall.data$Inter_PropOverall_LT_Screen <- Inter_PropOverall_LT_Screen_vector
mean.overall.data$trial_valid <- trial_valid_vector
mean.overall.data$InterPhase_LT_FacesObject_PROP <- InterPhase_LT_FacesObject_PROP_vector
mean.overall.data$InterPhase_LT_FacesObject_Gazing_PROP <- InterPhase_LT_FacesObject_Gazing_PROP_vector
mean.overall.data$Inter_ObjectOriginFaceTotal <- Inter_ObjectOriginFaceTotal_vector
mean.overall.data$Inter_FacesGazeshiftsRightTotal <- Inter_FacesGazeshiftsRightTotal_vector
mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_Begin <- InterPhase_FacesGazeshiftsRightTotal_Begin_vector
mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_End <- InterPhase_FacesGazeshiftsRightTotal_End_vector
mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL <- InterPhase_FacesGazeshiftsRightTotal_TOTAL_vector
mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing <- InterPhase_ObjectOriginFaceTotal_Gazing_vector
mean.overall.data$Inter_ObjectOriginFaceTotal_Gazing_SocialPROP <- Inter_ObjectOriginFaceTotal_Gazing_SocialPROP_vector
mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP <- InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP_vector
mean.overall.data$InterPhase_LF_Object_Gazing <- InterPhase_LF_Object_Gazing_vector
mean.overall.data$InterPhase_ObjectOriginNonSocial <- InterPhase_ObjectOriginNonSocial_vector

# -------------------------------------------------------------------------------------------
# STEP 4: Get an overview of number of valid trials after data exclusion
# -------------------------------------------------------------------------------------------
# Overview of valid trials for each individual child)
aggregate(overall.data$trial_valid ~ Condition+ID, data=overall.data, sum, na.rm=TRUE)
# summary valid trial info condition 1
validCon1 <- subset(mean.overall.data, mean.overall.data$Condition=="Con1")
sum(validCon1$trial_valid)
min(validCon1$trial_valid)
max(validCon1$trial_valid)
mean(validCon1$trial_valid)
sd(validCon1$trial_valid)
# summary valid trial info condition 2
validCon2 <- subset(mean.overall.data, mean.overall.data$Condition=="Con2")
sum(validCon2$trial_valid)
min(validCon2$trial_valid)
max(validCon2$trial_valid)
mean(validCon2$trial_valid)
sd(validCon2$trial_valid)
# summary valid trial info condition 3
validCon3 <- subset(mean.overall.data, mean.overall.data$Condition=="Con3")
sum(validCon3$trial_valid)
min(validCon3$trial_valid)
max(validCon3$trial_valid)
mean(validCon3$trial_valid)
sd(validCon3$trial_valid)
# summary valid trial info condition 4
validCon4 <- subset(mean.overall.data, mean.overall.data$Condition=="Con4")
sum(validCon4$trial_valid)
min(validCon4$trial_valid)
max(validCon4$trial_valid)
mean(validCon4$trial_valid)
sd(validCon4$trial_valid)

#############################################################################################
# SECTION 2: DESCRIPTIVE DATA (means and sds)
#############################################################################################

## Means and SDs of proportional looking time to novel object in the preferential looking phase
# condition 1
mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
# condition 2
mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
# condition 3
mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
# condition 4
mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

#############################################################################################
# SECTION 3: MAIN ANALYSIS (PRE-REGISTERED - in main manuscript)
#############################################################################################

# Transform variables
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

# Condition.code # Robert: no need to center and scale
#overall.data$Con_SocInt.num.code <- as.vector(scale(as.numeric(overall.data$Con_SocInt.num)))
#overall.data$Con_Object.num.code <- as.vector(scale(as.numeric(overall.data$Con_Object.num)))

# HYPOTHESIS 1: Interaction between third-party social context and third-party gaze
#contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
model.1a = lmer(PrefLook_LT_Object_Nov_PROP ~ Con_SocInt * Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.1a, test="Chisq")
summary(model.1a)
r.squaredGLMM(model.1a)

Red = lmer(PrefLook_LT_Object_Nov_PROP ~ (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
anova(Red, model.1a)

# Plotting residuals and fitted values
diagnostics.plot(model.1a)
ranef.diagn.plot(model.1a)
#diagnostics.plot(model.1b)
#ranef.diagn.plot(model.1b)

#############################################################################################
# SECTION 4: POST-HOC ANALYSES (NOT PRE-REGISTERED - In main manuscript)
#############################################################################################

# PART (1): ONE SAMPLE T-TESTS
# condition 1
t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], mu=0.50, alternative="two.sided")
d.onesample.Con1 <- (mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")],na.rm = TRUE) - 0.50) / sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")],na.rm = TRUE) #Effect size cohen's d
d.onesample.Con1
# cndition 2
t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], mu=0.50, alternative="two.sided")
d.onesample.Con2 <- (mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")],na.rm = TRUE) - 0.50) / sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")],na.rm = TRUE) #Effect size cohen's d
d.onesample.Con2
# condition 3
t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], mu=0.50, alternative="two.sided")
d.onesample.Con3 <- (mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")],na.rm = TRUE) - 0.50) / sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")],na.rm = TRUE) #Effect size cohen's d
d.onesample.Con3
# condition 4
t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], mu=0.50, alternative="two.sided")
d.onesample.Con4 <- (mean(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")],na.rm = TRUE) - 0.50) / sd(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")],na.rm = TRUE) #Effect size cohen's d
d.onesample.Con4

# PART (2): PAIR-WISE COMPARISONS
emmeans(model.1a, pairwise~Con_SocInt * Con_Object, adjust="Bonferroni")
#emmeans(model.1a, pairwise~Con_SocInt * Con_Object, adjust="none")

## condition 1 and condition 2
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], paired=T, na.rm = TRUE) # use sub-samples of Table 2
## condition 1 and condition 3
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], paired=T, na.rm = TRUE) # use sub-samples of Table 2
## condition 1 and condition 4
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con1")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], paired=T, na.rm = TRUE) # use sub-samples of Table 2
## condition 2 and condition 3
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], paired=T, na.rm = TRUE) # use sub-samples of Table 2
## condition 2 and condition 4
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con2")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], paired=T, na.rm = TRUE) # use sub-samples of Table 2
## condition 3 and condition 4
#t.test(mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con3")], mean.overall.data$PrefLook_LT_Object_Nov_PROP[which(mean.overall.data$Condition == "Con4")], paired=T, na.rm = TRUE) # use sub-samples of Table 2

# PART (3): INFLUENCE OF DIRECT ATTENTION TO THE OBJECT DURING ENCODING ON NOVELTY PREFERENCE IN SUBSEQUENT PREF LOOKING PHASE
overall.data$z.Inter_LT_Object <- as.vector(scale(overall.data$Inter_LT_Object))
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))

model.1b = lmer(PrefLook_LT_Object_Nov_PROP ~ z.Inter_LT_Object + (1|ID) + (0+ z.Inter_LT_Object|ID), data = overall.data, REML=F) # Data table with one row per trial (16/child), DV: "PrefLook_LT_Object_Novel_PROP"
drop1(model.1b, test="Chisq")
summary(model.1b)

## cor.test(overall.data$PrefLook_LT_Object_Nov_PROP, overall.data$Inter_LT_Object, method = "pearson", use="complete.obs")

# PART (4): DIFFERENCE IN INFANTS PROCESSING IF THEY HAD LOOKED AT THE OBJECT AT ALL
overall.data$Object <- ifelse(overall.data$Inter_LT_Object > "0",1, 0)
overall.data$z.Object <- as.vector(scale(overall.data$Object))
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))

model.learntrials = lmer(PrefLook_LT_Object_Nov_PROP ~ Object + (1|ID) + (0+ z.Object|ID), data = overall.data, REML=F) # Data table with one row per trial (16/child), DV: "PrefLook_LT_Object_Novel_PROP"
drop1(model.learntrials, test="Chisq")
summary(model.learntrials)

mean(overall.data$PrefLook_LT_Object_Nov[which(overall.data$Object == "1")], na.rm = TRUE)
sd(overall.data$PrefLook_LT_Object_Nov[which(overall.data$Object == "1")], na.rm = TRUE)
mean(overall.data$PrefLook_LT_Object_Nov[which(overall.data$Object == "0")], na.rm = TRUE)
sd(overall.data$PrefLook_LT_Object_Nov[which(overall.data$Object == "0")], na.rm = TRUE)

##Number of trials during which infants looked to the object (or not looked to it) - Figure S1 in supplements
overall.data$Object[overall.data$trial_valid==0] <- NA   #replace 0 with NAs
table(overall.data$Object)
table(overall.data$Object[which(overall.data$trial_valid == "1")])
table(overall.data$Object[which(overall.data$Condition == "Con1")])
table(overall.data$Object[which(overall.data$Condition == "Con2")])
table(overall.data$Object[which(overall.data$Condition == "Con3")])
table(overall.data$Object[which(overall.data$Condition == "Con4")])

#################################################################################################
# SECTION 5: VISUALIZATION
#################################################################################################

# -----------------------------------------------------------------------------------------------
# Boxplots of prop. looking time to novel object in preferential looking phase for each condition
# -----------------------------------------------------------------------------------------------
Boxplot_3 <- ggplot(mean.overall.data, aes(Condition, PrefLook_LT_Object_Nov_PROP))
Boxplot_3 + geom_boxplot(fill=c("grey", "grey","grey", "grey")) + scale_y_continuous(limits=c(0.0,1.0), name="Mean proportion of looking time") + labs(x="Condition") + scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=10, vjust=-0.4, hjust=0.54),
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0),
        panel.background = element_blank(),
        #axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  ) + geom_hline(aes(yintercept=0.5), linetype="dotted", size = 1)

############################################################################################
#### SECTION 6: EXPLORATORY ANALYSES (NOT PRE-REG - reported in supplemental materials)
############################################################################################

# -------------------------------------------------------------------------------------------
# OVERALL VIDEO DURATION
# -------------------------------------------------------------------------------------------

# CONDITION DIFFERENCES IN OVERALL LOOKING TIME TO THE SCREEN

### Condition differences in overall LT at the sreen in familiarization phase
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

model.overallLTFam = lmer(Overall_LT_ScreenFam ~ Con_SocInt * Con_Object + z.Trial + (1|ID) + (0+Condition|ID) +  (0+ z.Trial|ID), data = overall.data, REML=F) #Interaction not significant
drop1(model.overallLTFam, test = "Chisq")
summary(model.overallLTFam)

model.overallLTFam = lmer(Overall_LT_ScreenFam ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Condition|ID) +  (0+ z.Trial|ID), data = overall.data, REML=F) #Interaction not significant
drop1(model.overallLTFam, test = "Chisq")
summary(model.overallLTFam)

mean(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$Overall_LT_ScreenFam[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

### Condition differences in overall LT at the sreen in pref-looking phase
model.overallLTPref = lmer(Overall_LT_ScreenPrefLook ~ Condition + z.Trial + (1|ID) + (0+Condition|ID) +  (0+ z.Trial|ID), data = overall.data, REML=F)
drop1(model.overallLTPref, test = "Chisq")
summary(model.overallLTPref)

# CONDITION DIFFERENCES IN LOOKING TIME TO THE OBJECT (OVERALL)
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

model.LTObj = lmer(Inter_LT_Object ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTObj, test="Chisq")
summary(model.LTObj)

mean(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_Object[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

# CONDITION DIFFERENCES IN LOOKING TIME TO THE FACES (OVERALL)
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

model.LTFaces = lmer(Inter_LT_FacesTotal ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFaces, test="Chisq")
summary(model.LTFaces)

mean(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$Inter_LT_FacesTotal[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

# CONDITION DIFFERENCES IN SHIFTS BETWEEN FACES
mean(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$Inter_FacesGazeshiftsRightTotal[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

model.LTObjinter = lmer(Inter_FacesGazeshiftsRightTotal ~ Con_SocInt * Con_Object + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTObjinter, test="Chisq")
summary(model.LTObjinter)

# CONDITION DIFFERENCES IN SOCIALLY CAUSED OBJECT LOOKS
cor.test(overall.data$PrefLook_LT_Object_Nov_PROP, overall.data$Inter_ObjectOriginFaceTotal, method = "pearson", use="complete.obs")

# -------------------------------------------------------------------------------------------
# INTERACTION PHASE ONLY
# -------------------------------------------------------------------------------------------

# CONDITION DIFFERENCES IN GAZE SHIFTS BETWEEN FACES
model.LTFacePropObj = lmer(InterPhase_FacesGazeshiftsRightTotal_TOTAL ~ Con_SocInt * Con_Object + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFacePropObj, test="Chisq")
summary(model.LTFacePropObj)

model.LTFacePropObj = lmer(InterPhase_FacesGazeshiftsRightTotal_TOTAL ~ Con_SocInt + Con_Object + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFacePropObj, test="Chisq")
summary(model.LTFacePropObj)

mean(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_FacesGazeshiftsRightTotal_TOTAL[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

# -------------------------------------------------------------------------------------------
# GAZING PHASE ONLY
# -------------------------------------------------------------------------------------------

### CONDITION DIFFERENCES IN LOOKING TIME TO THE OBJECT (GAZING PHASE)
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

model.LTObjgaz = lmer(InterPhase_LT_Object_Gazing ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTObjgaz, test="Chisq")
summary(model.LTObjgaz)

mean(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_Object_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

### CONDITION DIFFERENCES IN LOOKING TIME TO THE FACES
overall.data$Con_SocInt.num <- ifelse(overall.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
overall.data$Con_Object.num <- ifelse(overall.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
overall.data$z.Trial <- as.vector(scale(overall.data$Trial))
overall.data$Soc.Obj.inter <- overall.data$Con_SocInt.num*overall.data$Con_Object.num

model.LTFacesInter = lmer(InterPhase_LT_FaceTotal_Gazing ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFacesInter, test="Chisq")
summary(model.LTFacesInter)

mean(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LT_FaceTotal_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

# CONDITION DIFFERENCES IN SOCIALLY CAUSED GAZE LOOKS AT OBJECT
## Optional Filter 4: Exclude trials during which child has not looked at the object at all
overall.data$InterPhase_ObjectOriginFaceTotal_Gazing [overall.data$InterPhase_LF_Object_Gazing==0.00] <- NA # do not consider zero socially caused looks if the child has not performed any look to the object
overall.data$InterPhase_LF_Object_Gazing [overall.data$InterPhase_LF_Object_Gazing==0.00] <- NA # do not consider

model.LTFacePropObj = lmer(InterPhase_ObjectOriginFaceTotal_Gazing ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFacePropObj, test="Chisq")
summary(model.LTFacePropObj)

model.LTFacePropObj = lmer(InterPhase_ObjectOriginFaceTotal_Gazing ~ Con_SocInt * Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = overall.data, REML=F)
drop1(model.LTFacePropObj, test="Chisq")
summary(model.LTFacePropObj)

cor.test(overall.data$PrefLook_LT_Object_Nov_PROP, overall.data$InterPhase_ObjectOriginFaceTotal_Gazing, method = "pearson", use="complete.obs")

mean(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_ObjectOriginFaceTotal_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)

# CONDITION DIFFERENCES IN ALL LOOKS AT THE OBJECT (including both socially caused as well as non-socially caused looks - Figure S3)
mean(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
mean(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con1")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con2")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con3")], na.rm = TRUE)
sd(mean.overall.data$InterPhase_LF_Object_Gazing[which(mean.overall.data$Condition == "Con4")], na.rm = TRUE)


