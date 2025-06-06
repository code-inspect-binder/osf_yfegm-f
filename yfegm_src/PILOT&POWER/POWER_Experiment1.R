#############################################################################################
#############################################################################################

# POWER ANALYSIS
# 22/04/2020

# **TABLE OF CONTENTS**
# SECTION 1: PREPARING PILOT DATA FOR POWER ANALYSIS (N=10)
# SECTION 2: POWER ANALYSIS FOR PLANNED MAIN ANALYSIS (According to Green & MacLeod, 2016)

#############################################################################################
#############################################################################################

# see also concersation with Robert (maybe only POWER for interaction model)

# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# load packages
library("ggplot2")
library("simr")
require(lme4)
library(lme4)
library(lmerTest)
library("exactRankTests")
library("MuMIn")

# set working directory
setwd("") #enter working directory path in ""

# source functions
source("") #enter path for disagnostic_fcns.r function in ""

# set directories
recs_dir <- "./PreProcessedTables_Pilot/"

# reads all files in recs folder (contains one textfile for each child, resulting from the pre-processing for the eye tracking data with "PREPROC_script_Exepriment")
flist <- list.files(recs_dir)

#############################################################################################
# SECTION 1: PREPARING PILOT DATA FOR POWER ANALYSIS
#############################################################################################

# -------------------------------------------------------------------------------------------
# STEP 1: Create Table for Main Analysis (One overall table with 16 rows per participant)
# -------------------------------------------------------------------------------------------
# Create new summary dataframe for overall data
pilot.data <- data.frame(matrix(vector(), 0, 65, dimnames=list(c(), c("ID",	"Sex",	"Age_Days",	"Trial",	"Condition",	"Con_Object",	"Con_SocInt",	"Dyad",	"Overall_LT_ScreenFam",	"Overall_LT_ScreenPrefLook",	"Inter_PropOverall_LT_Screen",	"InterPhase_LT_Soc_Begin",	"InterPhase_LT_Soc_End",	"InterPhase_LT_Soc_Total",	"InterPhase_LT_Gazing",	"Inter_LT_ActorLeft",	"Inter_LT_ActorRight",	"Inter_LT_ActorsTotal",	"Inter_LT_Actors_PROP",	"Inter_LT_FaceLeft",	"Inter_LT_FaceRight",	"Inter_LT_FacesTotal",	"Inter_LT_Faces_PROP", "Inter_LT_Object",	"Inter_LT_FacesObject_PROP",	"Inter_LT_Object_PROP",	"InterPhase_LT_FaceLeft_Soc_Begin",	"InterPhase_LT_FaceRight_Soc_Begin",	"InterPhase_LT_FaceTotal_Soc_Begin",	"InterPhase_LT_FaceTotal_Soc_Begin_PROP", "InterPhase_LT_FaceLeft_Soc_End",	"InterPhase_LT_FaceRight_Soc_End",	"InterPhase_LT_FaceTotal_Soc_End",	"InterPhase_LT_FaceTotal_Soc_End_PROP",	"InterPhase_LT_FaceLeft_Soc_Total",	"InterPhase_LT_FaceRight_Soc_Total",	"InterPhase_LT_FaceTotal_Soc_Total",	"InterPhase_LT_FaceTotal_Soc_Total_PROP",	"InterPhase_LT_Object_Soc_Begin",	"InterPhase_LT_Object_Soc_Begin_PROP",	"InterPhase_LT_Object_Soc_End",	"InterPhase_LT_Object_Soc_End_PROP",	"InterPhase_LT_Object_Soc_Total",	"InterPhase_LT_Object_Soc_Total_PROP",	"InterPhase_LT_FaceLeft_Gazing",	"InterPhase_LT_FaceRight_Gazing",	"InterPhase_LT_FaceTotal_Gazing",	"InterPhase_LT_FaceTotal_Gazing_PROP",	"InterPhase_LT_Object_Gazing",	"InterPhase_LT_Object_Gazing_PROP",	"InterPhase_Checker_Soc",	"InterPhase_Checker_Gazing",	"InterPhase_Checker_valid",	"PrefLook_Object_Fam",	"PrefLook_Object_Fam_Pos",	"PrefLook_Object_Nov",	"PrefLook_Object_Nov_Pos",	"PrefLook_LT_Object_Left",	"PrefLook_LT_Object_Right",	"PrefLook_LT_Total",	"PrefLook_LT_Object_Fam",	"PrefLook_LT_Object_Nov",	"PrefLook_LT_Object_Nov_PROP",	"PrefLook_FL",	"PrefLook_FL_Meaning"))), stringsAsFactors=F)
# Loop through all individual textfiles and add them together in pilot.data
for (i in 1:length(flist)) {
  current_subject <- flist[i]
  added_subject <- read.table(file = file.path(recs_dir, current_subject), sep = "\t", header = TRUE)
  pilot.data <- rbind(pilot.data, added_subject)
}
# Determining columns of interest
coi <-
  c(
    "ID",
    "Sex",
    "Age_Days",
    "Trial",
    "Condition",
    "Con_Object",
    "Con_SocInt",
    "Inter_LT_ActorsTotal",
    "Inter_LT_Actors_PROP",
    "Inter_LT_FacesTotal",
    "Inter_LT_Faces_PROP",
    "Inter_LT_Object",
    "Inter_LT_Object_PROP",
    "InterPhase_LT_FaceTotal_Soc_Begin",
    "InterPhase_LT_FaceTotal_Soc_End",
    "InterPhase_LT_FaceTotal_Soc_Total",
    "InterPhase_LT_FaceTotal_Soc_Total_PROP",
    "InterPhase_LT_FaceTotal_Gazing",
    "InterPhase_LT_Object_Gazing",
    "InterPhase_LT_Object_Gazing_PROP",
    "InterPhase_Checker_valid",
    "PrefLook_LT_Object_Nov_PROP",
    "PrefLook_FL_Meaning"
  )
# Create reduced dataframe with colums of interest only
pilot.data <- pilot.data[, coi]

# --------------------------------------------------------------------------------------------------
# STEP 2: Filter data in pilot.data Table (see also Wahl et al., 2013).
# --------------------------------------------------------------------------------------------------
# Filter 1: Exclude if child does not look at the screen for at least one fixation during the relevant phases
pilot.data$PrefLook_LT_Object_Nov_PROP[pilot.data$InterPhase_Checker_valid==0.00] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
# Filter 2: Exclude trials if child looks only at one object during the prefernetial looking task
#pilot.data$PrefLook_LT_Object_Nov_PROP[pilot.data$PrefLook_LT_Object_Nov_PROP==0.00] <- NA   #replace 0 with NAs
#pilot.data$PrefLook_LT_Object_Nov_PROP[pilot.data$PrefLook_LT_Object_Nov_PROP==1.00] <- NA   #replace 0 with NAs
# Filter 3: Exclude trials during which child has not looked at the screen at all during the preferential looking task
pilot.data$trial_valid <-ifelse(pilot.data$PrefLook_LT_Object_Nov_PROP >= 0, 1, 0) # create trial_valid column, a trial is valid if Prop score is >=0. due to the previous step exclusion do to insufficient looking in relevant phases is considered already.
pilot.data$trial_valid[is.na(pilot.data$trial_valid)] <- 0 #replace NAs with 0 for averaging in the next steps


#####################################################################################################
# SECTION 2: POWER ANALYSIS FOR PLANNED MAIN ANALYSIS (Steps according to Green & MacLeod, 2016)
#####################################################################################################

# -------------------------------------------------------------------------------------------
# Step 1: FITTING THE MODELS
# -------------------------------------------------------------------------------------------
### Manuscript: "We will calculate a general linear mixed model (GLMM) for the mean prop. looking time
### at the novel object in the preferential-looking phase (dependent variable).
### As fixed effects we will include the interaction between social context (interactive, non-interactive)
### and look at object (look at object, no look at object), as well as trial (to control for possible
### effects of trial order). All factors will be tested within subjects. We will include appropriate
### random effects in the model. If the two-way interaction does not reveal a significant effect, we
### will repeat our main analysis including social context (interactive, non-interactive) and look at
### object (look at object, no look at object) as main effects."

# Transform variables
pilot.data$Con_SocInt.num <- ifelse(pilot.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
pilot.data$Con_Object.num <- ifelse(pilot.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
pilot.data$z.Trial <- as.vector(scale(pilot.data$Trial))
pilot.data$Soc.Obj.inter <- pilot.data$Con_SocInt.num*pilot.data$Con_Object.num

#pilot.data$Con_SocInt.num <- ifelse(pilot.data$Con_SocInt == "SOC",1, 0) # Make condition variables numeric
#pilot.data$Con_Object.num <- ifelse(pilot.data$Con_Object == "LOB",1, 0) # Make condition variables numeric
#pilot.data$z.Trial <- as.vector(scale(pilot.data$Trial))
## Condition.code
#pilot.data$Con_SocInt.num.code <- as.vector(scale(as.numeric(pilot.data$Con_SocInt.num)))
#pilot.data$Con_Object.num.code <- as.vector(scale(as.numeric(pilot.data$Con_Object.num)))

# FULL MODELS
model.1a = lmer(PrefLook_LT_Object_Nov_PROP ~ Con_SocInt * Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = pilot.data, REML=F)
drop1(model.1a, test="Chisq")

#model.1b = lmer(PrefLook_LT_Object_Nov_PROP ~ Con_SocInt + Con_Object + z.Trial + (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = pilot.data, REML=F)
#drop1(model.1b, test="Chisq")

Red = lmer(PrefLook_LT_Object_Nov_PROP ~ (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = pilot.data, REML=F)

anova(Red, model.1a)
#anova(Red, model.1b)


#model.1a = lmer(PrefLook_LT_Object_Nov_PROP ~ Con_SocInt.num * Con_Object.num + z.Trial + (1|ID) + (0+Con_SocInt.num.code|ID) + (0+Con_Object.num.code|ID) + (0+Con_SocInt.num.code:Con_Object.num.code|ID) + (0+ z.Trial|ID), data = pilot.data, REML=F) # Data table with one row per trial (16/child), DV: "PrefLook_LT_Object_Novel_PROP"
#drop1(model.1a, test="Chisq")
#summary(model.1a)

#model.1b = lmer(PrefLook_LT_Object_Nov_PROP ~ Con_SocInt.num + Con_Object.num + z.Trial + (1|ID) + (0+Con_SocInt.num.code|ID) + (0+Con_Object.num.code|ID) + (0+Con_SocInt.num.code:Con_Object.num.code|ID) + (0+ z.Trial|ID), data = pilot.data, REML=F) # Data table with one row per trial (16/child), DV: "PrefLook_LT_Object_Novel_PROP"
#drop1(model.1b, test="Chisq")
#summary(model.1b)

# Plotting residuals and fitted values
diagnostics.plot(model.1a)
ranef.diagn.plot(model.1a)
#diagnostics.plot(model.1b)
#ranef.diagn.plot(model.1b)

# -------------------------------------------------------------------------------------------
# Step 2: SPECIFYING THE EFFECT SIZE (R-squared; Nakagawa & Schielzeth, 2013)
# -------------------------------------------------------------------------------------------
# REDUCED MODEL (including only the control variables and random effects without the fixed effects)
Red = lmer(PrefLook_LT_Object_Nov_PROP ~ (1|ID) + (0+Con_SocInt.num|ID) + (0+Con_Object.num|ID)+ (0+ z.Trial|ID) + (0+ Soc.Obj.inter |ID), data = pilot.data, REML=F)
coef(summary(Red))
drop1(Red, test="Chisq")
r.squaredGLMM(Red)
#conditional R-squared for Red: 0.2225565

r2.model.1a<-r.squaredGLMM(model.1a)-r.squaredGLMM(Red)
r2.model.1a
#conditional R-squared model.1a: 0.07403356
#marginal R-squared model.1a: 0.05971391

#r2.model.1b<-r.squaredGLMM(model.1b)-r.squaredGLMM(Red)
#r2.model.1b
#conditional R-squared model.1b: 0.00760462
#marginal R-squared model.1b: 0.01155094

# Average R squared delta for Model 1a and 1b
#r2.model1.ab<-round((r2.model.1a+r2.model.1b)/2,3)
#r2.model1.ab
#conditional R-squared average: 0.006
#marginal R-squared average: 0.048

# -------------------------------------------------------------------------------------------
# Step 4: INCREASING THE SAMPLE SIZE
# -------------------------------------------------------------------------------------------
model.1a <- extend(model.1a, along="ID", n=32)
#model.1b <- extend(model.1b, along="ID", n=32)
Red<-extend(Red, along="ID", n=32)

# -------------------------------------------------------------------------------------------
# Step 3: RUNNING THE POWER ANALYSIS
# -------------------------------------------------------------------------------------------
powerSim(model.1a, test=compare(Red, method= c("lr")), seed=1, nsim=1000)
#99.60% (98.98, 99.89)
#powerSim(model.1b, test=compare(Red, method= c("lr")), seed=1, nsim=1000)
#55.20% (52.06, 58.31)










