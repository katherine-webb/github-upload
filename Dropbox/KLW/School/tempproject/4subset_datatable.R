setwd("~/Dropbox/KLW/School/2018-19/PHP 1970/DataAnalysis")
load("FullVars_All.rda")  

DM_vars_final <- subset(FullVars_All, year == "I")
library(survey)


# Analysis ----------------------------------------------------------------

DM_vars_final <- transform(DM_vars_final, race=
                             ifelse(RIDRETH3 == 4, 1, 0))
DM_vars_final <- transform(DM_vars_final, gender=
                             ifelse(RIAGENDR == 1, 1, 0))

DM_vars_final <- transform(DM_vars_final, cancer=
                             ifelse(MCQ220 == 1, 1, 0))

DM_vars_final <- transform(DM_vars_final, MACE=
                             ifelse(MCQ160C == 1|
                                      MCQ160B == 1|
                                      MCQ160F == 1|
                                      MCQ160E == 1, 1, 0))

DM_vars_final <- transform(DM_vars_final, MACEnoCHD=
                             ifelse(MCQ160B == 1|
                                      MCQ160F == 1|
                                      MCQ160E == 1, 1, 0))

DM_vars_final <- transform(DM_vars_final, hypertension =
                             # hypertensive
                             ifelse(BPQ020 == 1|BPQ050A == 1, 1, 0))
DM_vars_final[, "hypertension"][is.na(DM_vars_final[, "hypertension"])] <- 0

DM_vars_final <- transform(DM_vars_final, current_smoker =
                             ifelse(SMQ040==1|SMQ040==2, 1,0))
DM_vars_final[, "current_smoker"][is.na(DM_vars_final[, "current_smoker"])] <- 0


DM_vars_final <- transform(DM_vars_final, former_smoker =
                             ifelse(SMQ040==3, 1, 0))
DM_vars_final[, "former_smoker"][is.na(DM_vars_final[, "former_smoker"])] <- 0

DM_vars_final <- transform(DM_vars_final, ACEorARB =
                             ifelse(ACE_I == 1|ARB == 2|ACE_I_combo == 3|ARB_combo == 4, 1, 0))
DM_vars_final[,"ACEorARB"][is.na(DM_vars_final[,"ACEorARB"])] <- 0

# Means + % ---------------------------------------------------------------

survey <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  nest = TRUE,
  weights = ~WTMEC2YR,
  data = DM_vars_final)

survey <- update(one = 1, survey)
svytotal(~one, survey)
sum(DM_vars_final$WTMEC2YR)

# continuous variables
svymean(~RIDAGEYR, survey, na.rm=T)
svymean(~BMXWAIST, survey, na.rm=T)
svymean(~BMXBMI, survey, na.rm=T)
svymean(~SYavg, survey, na.rm=T)
svymean(~DIavg, survey, na.rm=T)
svymean(~CKD.EPI, survey, na.rm=T)
svymean(~MDRD, survey, na.rm=T)
svymean(~LBDHDD, survey, na.rm=T)
svymean(~LBXTC, survey, na.rm=T)
svymean(~LBXSTR, survey, na.rm=T)
svymean(~LBXTR, survey, na.rm=T)
svymean(~LBDLDL, survey, na.rm=T)
svymean(~LBXSKSI, survey, na.rm=T)
svymean(~LBXSC3SI, survey, na.rm=T)

sqrt(abs(svyvar(~RIDAGEYR, survey, na.rm=T)))
sqrt(abs(svyvar(~BMXWAIST, survey, na.rm=T)))
sqrt(abs(svyvar(~BMXBMI, survey, na.rm=T)))
sqrt(abs(svyvar(~SYavg, survey, na.rm=T)))
sqrt(abs(svyvar(~DIavg, survey, na.rm=T)))
sqrt(abs(svyvar(~CKD.EPI, survey, na.rm=T)))
sqrt(abs(svyvar(~MDRD, survey, na.rm=T)))
sqrt(abs(svyvar(~LBDHDD, survey, na.rm=T)))
sqrt(abs(svyvar(~LBXTC, survey, na.rm=T)))
sqrt(abs(svyvar(~LBXSTR, survey, na.rm=T)))
sqrt(abs(svyvar(~LBXTR, survey, na.rm=T)))
sqrt(abs(svyvar(~LBDLDL, survey, na.rm=T)))
sqrt(abs(svyvar(~LBXSKSI, survey, na.rm=T)))
sqrt(abs(svyvar(~LBXSC3SI, survey, na.rm=T)))

# dichotomous variables
svymean(~gender, survey, na.rm=T)
svymean(~race, survey, na.rm=T)
svymean(~MACE, survey, na.rm=T)
svymean(~hypertension, survey, na.rm=T)
svymean(~current_smoker, survey, na.rm=T)
svymean(~former_smoker, survey, na.rm=T)
svymean(~ACEorARB, survey, na.rm=T)


# ACR
svyquantile(~URDACT, survey, c(.25,.5,.75), na.rm=TRUE)


