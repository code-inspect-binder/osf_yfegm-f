# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# import utility functions & mute RStudio diagnostics
# !diagnostics suppress=allocateTrials, destructureString, getAOIs, getCS, getLooks, getObjects, getPrefLookPositions, getStartEndPositions
sapply(list.files(c("util"), pattern = "*.r$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv)

# import user interface
# !diagnostics suppress=coi, recs_dir, _filename, inter_trial_chunk_patterns, aoi_fam_body_object, aoi_fam_face, aoi_preflook, aoi_screen
source("PREPROC_interface_Experiment1.r")

# reads all files in recs folder
flist <- list.files(recs_dir)


# Loop over all subjects
for (i in 1:length(flist)) {

#for (i in 1:1 {

  current_subject <- flist[i]

  # read tsv files
  df0_raw <- read.table(file = file.path(recs_dir, current_subject), sep = "\t", header = TRUE)

  # create COI df (todo: this df is not necessary)
  df1_coi <- df0_raw[, coi]


  # get start and end index pairs for inter_trial chunks
  familiarization_attention_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[1], "MovieStart", "MovieEnd")
  familiarization_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[2], "MovieStart", "MovieEnd")
  preflook_attention_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[3], "MovieStart", "MovieEnd")
  preflook_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[4], "MovieStart", "MovieEnd")



  # Allocate Trials and Fillup StudioEventData Label (todo: this df is not necessary)
  df2_trial <- df1_coi
  df2_trial <- allocateTrials(df2_trial, familiarization_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocateTrials(df2_trial, familiarization_startend)
  df2_trial <- allocateTrials(df2_trial, preflook_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocateTrials(df2_trial, preflook_startend)


  # track the number of max trials (todo use start end positions to determien trial length, also create a function that checks trial length consistency over all startend positions)
  total_trials <- max(df2_trial$Trial, na.rm = TRUE)

  # track vector of all inter names (important for dfX_base performance)
  inter_vectors <- as.character(unique(df2_trial$StudioEventData[familiarization_startend$start]))

  # track valid trials
  # todo


  # AOI Columns
  df3_aoi <- df2_trial

  # AOI column for Familiarization Phase for Body & Object (left, right, center)
  df3_aoi <- getAOIs(df3_aoi, aoi_fam_body_object, familiarization_startend)
  # AOI column for Familiarization Phase for faces (left & right)
  df3_aoi <- getAOIs(df3_aoi, aoi_fam_face, familiarization_startend)
  # AOI column for Familiarization Phase for Face & Object (left, right, center)
  df3_aoi <- getAOIs(df3_aoi, aoi_fam_face_object, familiarization_startend)
  # AOI column for Preferential Looking Phase for objects (left & right)
  df3_aoi <- getAOIs(df3_aoi, aoi_preflook, preflook_startend)
  # AOI column for Familiarization Phase & Preferential Looking Phase for screen (TRUE/FALSE)
  df3_aoi <- getAOIs(df3_aoi, aoi_screen, c(familiarization_startend, preflook_startend))

  ####################################################################################################
  # Initialize empty Dataframe with 0 columns and row count equals current total_trials
  dfX_base <- data.frame(matrix(NA, nrow = total_trials, ncol = 0), stringsAsFactors = FALSE)

  # Build Summary table
  # ==================================================================================================
  # NAME INFORMATIONS
  # --------------------------------------------------------------------------------------------------
  dfX_base$ID <- destructureString(lut_filename, current_subject)$ID
  dfX_base$Sex <- destructureString(lut_filename, current_subject)$Sex
  dfX_base$Age_Days <- destructureString(lut_filename, current_subject)$Age_Days
  dfX_base$Trial <- 1:total_trials
  dfX_base$Condition <- destructureString(lut_fam_phase, inter_vectors)$Condition
  dfX_base$Con_Object <- destructureString(lut_fam_phase, inter_vectors)$Con_Object
  dfX_base$Con_SocInt <- destructureString(lut_fam_phase, inter_vectors)$Con_SocInt
  dfX_base$Dyad <- destructureString(lut_fam_phase, inter_vectors)$Dyad

   # --------------------------------------------------------------------------------------------------
  # Familiarization Phase
  # --------------------------------------------------------------------------------------------------
  # OVERALL SCREEN (no specific AOI): Looking times over total duration of the video
  dfX_base$Overall_LT_ScreenFam <- getLooks(df3_aoi, aoi_screen, familiarization_startend)$looking_times # total LT to the screen during familiarisation phase
  dfX_base$Overall_LT_ScreenPrefLook <- getLooks(df3_aoi, aoi_screen, preflook_startend)$looking_times # total LT to the screen during preferential looking phase
  dfX_base$Inter_PropOverall_LT_Screen <- ifelse(dfX_base$Overall_LT_ScreenFam <= 11000, dfX_base$Overall_LT_ScreenFam/11000, dfX_base$Overall_LT_ScreenFam/max(dfX_base$Overall_LT_ScreenFam)) # proportional looking time at the screen in familiarization phase (LT Screen/maxduration of the trial)
  # OVERALL SCREEN (no specific AOI): Looking times only in INTEARCTION PHASES (in the beginning and ending of the video sequence)
  dfX_base$InterPhase_LT_Soc_Begin <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(2000, 3000))$looking_times
  dfX_base$InterPhase_LT_Soc_End <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(10000, 11000))$looking_times
  dfX_base$InterPhase_LT_Soc_Total <- dfX_base$InterPhase_LT_Soc_Begin + dfX_base$InterPhase_LT_Soc_End
  # OVERALL SCREEN (no specific AOI): Looking times only in GAZING PHASE
  dfX_base$InterPhase_LT_Gazing <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(4000, 9000))$looking_times

  # AOIs: Looking times over TOTAL duration of the video
  dfX_base$Inter_LT_ActorLeft <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_ActorRight <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_ActorsTotal <- dfX_base$Inter_LT_ActorLeft + dfX_base$Inter_LT_ActorRight
  dfX_base$Inter_LT_Actors_PROP <- dfX_base$Inter_LT_ActorsTotal / dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_FaceLeft <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_FaceRight <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_FacesTotal <- dfX_base$Inter_LT_FaceLeft + dfX_base$Inter_LT_FaceRight
  dfX_base$Inter_LT_Faces_PROP <- dfX_base$Inter_LT_FacesTotal / dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_Object <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$center
  dfX_base$Inter_LT_FacesObject_PROP <- dfX_base$Inter_LT_FacesTotal / (dfX_base$Inter_LT_FacesTotal + dfX_base$Inter_LT_Object)
  dfX_base$Inter_LT_Object_PROP <- dfX_base$Inter_LT_Object / dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LF_FaceLeft <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_frequencies$left
  dfX_base$Inter_LF_FaceRight <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_frequencies$right
  dfX_base$Inter_LF_FacesTotal <- dfX_base$Inter_LF_FaceLeft + dfX_base$Inter_LF_FaceRight
  dfX_base$Inter_LF_FaceLeft_Overall <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_frequencies$left
  dfX_base$Inter_LF_FaceRight_Overall <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_frequencies$right
  dfX_base$Inter_LF_FacesTotal_Overall <- dfX_base$Inter_LF_FaceLeft_Overall + dfX_base$Inter_LF_FaceRight_Overall
  dfX_base$Inter_ObjectOriginFaceLeft <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend)$gaze_shifts$left$center
  dfX_base$Inter_ObjectOriginFaceRight <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend)$gaze_shifts$right$center
  dfX_base$Inter_ObjectOriginFaceTotal <- dfX_base$Inter_ObjectOriginFaceLeft + dfX_base$Inter_ObjectOriginFaceRight
  dfX_base$Inter_LF_Object <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend)$looking_frequencies$center
  dfX_base$Inter_ObjectOriginFaceTotal_Gazing_SocialPROP <- dfX_base$Inter_ObjectOriginFaceTotal / dfX_base$Inter_LF_Object
  dfX_base$Inter_FacesGazeshiftsLeftRight <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend)$gaze_shifts$left$right
  dfX_base$Inter_FacesGazeshiftsRightLeft <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend)$gaze_shifts$right$left
  dfX_base$Inter_FacesGazeshiftsRightTotal <- dfX_base$Inter_FacesGazeshiftsRightLeft + dfX_base$Inter_FacesGazeshiftsLeftRight

  # AOIs: Looking times in INTERACTION PHASES (Before and after gazing to the object)
  dfX_base$InterPhase_LT_FaceLeft_Soc_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin <- dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_Begin
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_Begin / dfX_base$InterPhase_LT_Soc_Begin
  dfX_base$InterPhase_LT_FaceLeft_Soc_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_End <- dfX_base$InterPhase_LT_FaceRight_Soc_End + dfX_base$InterPhase_LT_FaceLeft_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_End_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_End / dfX_base$InterPhase_LT_Soc_End
  dfX_base$InterPhase_LT_FaceLeft_Soc_Total <- dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceLeft_Soc_End
  dfX_base$InterPhase_LT_FaceRight_Soc_Total <- dfX_base$InterPhase_LT_FaceRight_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total <- dfX_base$InterPhase_LT_FaceTotal_Soc_Begin + dfX_base$InterPhase_LT_FaceTotal_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_Total / dfX_base$InterPhase_LT_Soc_Total
  dfX_base$InterPhase_LT_Object_Soc_Begin <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(2000, 3000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_Begin_PROP <- dfX_base$InterPhase_LT_Object_Soc_Begin / dfX_base$InterPhase_LT_Soc_Begin
  dfX_base$InterPhase_LT_Object_Soc_End <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(10000, 11000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_End_PROP <- dfX_base$InterPhase_LT_Object_Soc_End / dfX_base$InterPhase_LT_Soc_End
  dfX_base$InterPhase_LT_Object_Soc_Total <- dfX_base$InterPhase_LT_Object_Soc_Begin + dfX_base$InterPhase_LT_Object_Soc_End
  dfX_base$InterPhase_LT_Object_Soc_Total_PROP <- dfX_base$InterPhase_LT_Object_Soc_Total / dfX_base$InterPhase_LT_Soc_Total
  dfX_base$InterPhase_LT_FacesObject_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_Total / (dfX_base$InterPhase_LT_FaceTotal_Soc_Total + dfX_base$InterPhase_LT_Object_Soc_Total)
  dfX_base$InterPhase_LF_FaceLeft_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_frequencies$left
  dfX_base$InterPhase_LF_FaceRight_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_frequencies$right
  dfX_base$InterPhase_LF_FacesTotal_Begin <- dfX_base$InterPhase_LF_FaceLeft_Begin + dfX_base$InterPhase_LF_FaceRight_Begin
  dfX_base$InterPhase_LF_FaceLeft_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_frequencies$left
  dfX_base$InterPhase_LF_FaceRight_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_frequencies$right
  dfX_base$InterPhase_LF_FacesTotal_End <- dfX_base$InterPhase_LF_FaceLeft_End + dfX_base$InterPhase_LF_FaceRight_End
  dfX_base$InterPhase_LF_FacesTotal <- dfX_base$InterPhase_LF_FacesTotal_Begin + dfX_base$InterPhase_LF_FacesTotal_End
  dfX_base$InterPhase_FacesGazeshiftsLeftRight_Begin <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(2000, 3000))$gaze_shifts$left$right
  dfX_base$InterPhase_FacesGazeshiftsRightLeft_Begin <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(2000, 3000))$gaze_shifts$right$left
  dfX_base$InterPhase_FacesGazeshiftsRightTotal_Begin <- dfX_base$InterPhase_FacesGazeshiftsLeftRight_Begin + dfX_base$InterPhase_FacesGazeshiftsRightLeft_Begin
  dfX_base$InterPhase_FacesGazeshiftsLeftRight_End <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(10000, 11000))$gaze_shifts$left$right
  dfX_base$InterPhase_FacesGazeshiftsRightLeft_End <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(10000, 11000))$gaze_shifts$right$left
  dfX_base$InterPhase_FacesGazeshiftsRightTotal_End <- dfX_base$InterPhase_FacesGazeshiftsLeftRight_End + dfX_base$InterPhase_FacesGazeshiftsRightLeft_End
  dfX_base$InterPhase_FacesGazeshiftsRightTotal_TOTAL <- dfX_base$InterPhase_FacesGazeshiftsRightTotal_Begin + dfX_base$InterPhase_FacesGazeshiftsRightTotal_End

  # AOIs: Looking times in GAZING PHASE
  dfX_base$InterPhase_LT_FaceLeft_Gazing <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Gazing <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Gazing <- dfX_base$InterPhase_LT_FaceLeft_Gazing + dfX_base$InterPhase_LT_FaceRight_Gazing
  dfX_base$InterPhase_LT_FaceTotal_Gazing_PROP <- dfX_base$InterPhase_LT_FaceTotal_Gazing / dfX_base$InterPhase_LT_Gazing
  dfX_base$InterPhase_LT_Object_Gazing <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(4000, 9000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Gazing_PROP <- dfX_base$InterPhase_LT_Object_Gazing / dfX_base$InterPhase_LT_Gazing
  dfX_base$InterPhase_LT_FacesObject_Gazing_PROP <- dfX_base$InterPhase_LT_FaceTotal_Gazing / (dfX_base$InterPhase_LT_FaceTotal_Gazing + dfX_base$InterPhase_LT_Object_Gazing)
  dfX_base$InterPhase_ObjectOriginFaceLeft_Gazing <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(4000, 9000))$gaze_shifts$left$center
  dfX_base$InterPhase_ObjectOriginFaceRight_Gazing <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(4000, 9000))$gaze_shifts$right$center
  dfX_base$InterPhase_ObjectOriginFaceTotal_Gazing <- dfX_base$InterPhase_ObjectOriginFaceLeft_Gazing + dfX_base$InterPhase_ObjectOriginFaceRight_Gazing
  dfX_base$InterPhase_LF_Object_Gazing <- getLooks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(4000, 9000))$looking_frequencies$center # Total Looks Object
  dfX_base$InterPhase_ObjectOriginFaceTotal_Gazing_SocialPROP <- dfX_base$InterPhase_ObjectOriginFaceTotal_Gazing / dfX_base$InterPhase_LF_Object_Gazing

  # Checker for trial inclusion
  dfX_base$InterPhase_Checker_Soc <- ifelse(dfX_base$InterPhase_LT_Soc_Begin == 0, FALSE, TRUE) # & dfX_base$InterPhase_LT_Soc_End == 0
  dfX_base$InterPhase_Checker_Gazing <- ifelse(dfX_base$InterPhase_LT_Gazing == 0, FALSE, TRUE)
  dfX_base$InterPhase_Checker_valid <-  ifelse(dfX_base$InterPhase_Checker_Soc == TRUE & dfX_base$InterPhase_Checker_Gazing == TRUE, 1, 0)

  # -------------------------------------------------------------------------------------------
  # Preferential Looking Phase
  # -------------------------------------------------------------------------------------------
  dfX_base$PrefLook_Object_Fam <- getObjects(df3_aoi, familiarization_startend)$familiar
  dfX_base$PrefLook_Object_Fam_Pos <- getPrefLookPositions(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), getObjects(df3_aoi, familiarization_startend)$familiar)$fam_pos
  dfX_base$PrefLook_Object_Nov <- getObjects(df3_aoi, familiarization_startend)$novel
  dfX_base$PrefLook_Object_Nov_Pos <- getPrefLookPositions(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), getObjects(df3_aoi, familiarization_startend)$familiar)$nov_pos
  dfX_base$PrefLook_LT_Object_Left <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$left
  dfX_base$PrefLook_LT_Object_Right <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$right
  dfX_base$PrefLook_LT_Total <- dfX_base$PrefLook_LT_Object_Left + dfX_base$PrefLook_LT_Object_Right
  dfX_base$PrefLook_LT_Object_Fam <-
    ifelse(dfX_base$PrefLook_Object_Fam_Pos == "right",
           dfX_base$PrefLook_LT_Object_Right,
           ifelse(dfX_base$PrefLook_Object_Fam_Pos == "left",
                  dfX_base$PrefLook_LT_Object_Left, NA))
  dfX_base$PrefLook_LT_Object_Nov <-
    ifelse(dfX_base$PrefLook_Object_Nov_Pos == "right",
           dfX_base$PrefLook_LT_Object_Right,
           ifelse(dfX_base$PrefLook_Object_Nov_Pos == "left",
                  dfX_base$PrefLook_LT_Object_Left, NA))
  dfX_base$PrefLook_LT_Object_Nov_PROP <- dfX_base$PrefLook_LT_Object_Nov / dfX_base$PrefLook_LT_Total

  # -------------------------------------------------------------------------------------------
  # First Looks
  # -------------------------------------------------------------------------------------------
  dfX_base$PrefLook_FL <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$first_look
  dfX_base$PrefLook_FL_Meaning <-
    ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Fam_Pos,
           "familiar",
           ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Nov_Pos,
                  "novel", NA))


  #write tables for individual particpants
 write.table(dfX_base, file=paste(tablesDir, current_subject, sep = ""), sep='\t', row.names = FALSE)

}

# ---------------------------------------------------------------------------------------------------
# INCLUSION CHECK AFTER EACH TESTING (while the study is ongoing)
# Use the steps below to decide for each child whether to be included or not BEFORE writing the Table
# After having run the steps below: Repeat processing loop above and save the table.
# ---------------------------------------------------------------------------------------------------
# Check 1: Exclude if child does not look at the screen for at least one fixation during the relevant phases
  dfX_base$PrefLook_LT_Object_Nov_PROP[dfX_base$InterPhase_Checker_valid==0.00] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
# Check 2: Exclude trials if child looks only at one object during the prefernetial looking task
 #dfX_base$PrefLook_LT_Object_Nov_PROP[dfX_base$PrefLook_LT_Object_Nov_PROP==0.00] <- NA   #replace 0 with NAs
 #dfX_base$PrefLook_LT_Object_Nov_PROP[dfX_base$PrefLook_LT_Object_Nov_PROP==1.00] <- NA   #replace 0 with NAs
# Check 3: Exclude trials during which child has not looked at the screen at all during the preferential looking task
  dfX_base$trial_valid <-ifelse(dfX_base$PrefLook_LT_Object_Nov_PROP >= 0, 1, 0) # create trial_valid column, a trial is valid if Prop score is >=0. due to the previous step exclusion do to insufficient looking in relevant phases is considered already.
  dfX_base$trial_valid[is.na(dfX_base$trial_valid)] <- 0   # Replace NA in valid trial column with 0 for nect step
# DROPOUT DETECTION DUE TO FILTERING CRITERIA: How many valid data point does the child contrigute per condition? (<1 means drop)
  aggregate(dfX_base$trial_valid, by=list(Condition=dfX_base$Condition), FUN=sum)

## Means and SDs of proportional looking time to novel object in the preferential looking phase
# condition 1
  mean(dfX_base$PrefLook_LT_Object_Nov_PROP[which(dfX_base$Condition == "Con1")], na.rm = TRUE)
# condition 2
  mean(dfX_base$PrefLook_LT_Object_Nov_PROP[which(dfX_base$Condition == "Con2")], na.rm = TRUE)
# condition 3
  mean(dfX_base$PrefLook_LT_Object_Nov_PROP[which(dfX_base$Condition == "Con3")], na.rm = TRUE)
# condition 4
  mean(dfX_base$PrefLook_LT_Object_Nov_PROP[which(dfX_base$Condition == "Con4")], na.rm = TRUE)


  #write.table(dfX_base, file=paste(tablesDir, current_subject, sep = ""), sep='\t', row.names = FALSE)

  #}

