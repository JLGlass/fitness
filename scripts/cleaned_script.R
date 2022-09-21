library(fitness)
library(data.table)
library(ComplexHeatmap)
library(lme4)
library(lmerTest)
library(nlme)
library(sjPlot)
library(ggplot2)
library(tableone)
library(GGally)
library(zoo)
library(knitr)
library(rstatix)
library(ggrepel)

# Directories ------
dir.main = "~/Documents/Codes/fitness"
dir.plots = file.path(dir.main, "survey_plots")
dir.labels = "~/mnt/Jacob/fitness/data/labels"
dir.tabulations = file.path(dir.main, "tables_xlsx")


# Themes ------

p.theme.tufte = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'))
p.theme.tufte.xaxisAngle = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'), axis.text.x=element_text(angle=45, hjust=1, vjust=1), axis.title.x=element_blank(), axis.ticks.x=element_blank())
p.theme.tufte.xaxis90 = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), axis.title.x=element_blank())
                                                                     
# load survey data -----

lbl.pro = fread(file.path(dir.labels, 'pro_data.txt'))
vec.pro = lbl.pro[, label]

lst.pro = readTables.lst(lbl.pro)


dt.walk = lst.pro[["walk.test"]]
dt.walk[dt.walk==''] <- NA
dt.walk = do.call(cbind, lapply(dt.walk, as.numeric))


# Merge CPET/6MW ------

cont.vars <- c("SixMWDTotalMeters", "SixMWDEndingHR", 'CpetDuration', 'CpetVo2Peak', 'CpetMaxHeartRate', 'CpetMvv', 'CpetMvvPercent', 'CpetMinutesExercised', 'CpetWorkCapacity', 'WorkCapPercent', 'CpetOxConsump', 'OxConsumpPercent', 'AnaerobicThresh', 'AnaerobicThreshPercent', 'MvPeak', 'MvPeakPercent')

dt.cpet.walked.merged = merge(dt.cpet, dt.walk, by = "FolderLabel")
dt.cpet.walked.merged.numeric = do.call(cbind, lapply(dt.cpet.walked.merged[, ..cont.vars], as.numeric))

dt.cpet.walked.merged.numeric = dt.cpet.walked.merged.numeric[, !(colnames(dt.cpet.walked.merged.numeric) %in%c('CpetMvvPercent', 'OxConsumpPercent', "AnaerobicThreshPercent", "MvPeakPercent"  ))]
colnames(dt.cpet.walked.merged.numeric) <- c("SixMWDTotalMeters", "SixMWDEndingHR", "CpetDuration", "Vo2Peak", "MaxHeartRate","Mvv", "MinutesExercised", "WorkCapacity", "CapPercent", "OxConsump", "AnaerobicThresh", "MvPeak")

cor.vo2max = cor(dt.cpet.walked.merged.numeric, use = "pairwise.complete.obs")

# Correlation heatmaps
Heatmap(cor(dt.cpet.walked.merged.numeric, use = "pairwise.complete.obs"), name = "Correlation")
Heatmap(cor.vo2max, name='Correlation')

dt.merged.numeric = merge(do.call(cbind, lapply(cbind(dt.merged$FolderLabel, dt.merged[, ..cont.vars]), as.numeric)), dt.allData, by.x = "V1", by.y = "id")

dt.cpet.walked.merged.numeric = do.call(cbind, lapply(dt.cpet.walked.merged.numeric, as.numeric))
dt.cpet.walked.merged.numeric = dt.cpet.walked.merged.numeric[, !(colnames(dt.cpet.walked.merged.numeric) %in% c("METs.min", "intensity.min", "steps.median", "steps.min", "intensity.median", "sync.count", "calories.sd", "hr.min", "hr.sd", "ecog.pct", "ecog.pct.corrected", "calories.min" , "calories.max", "calories.median", "ecog.num", "CpetMvvPercent",  "METs.sd", "steps.sd", "intensity.sd", "steps.max", "METs.median", "hr.max", "ecog.num.corrected", "sleep"))] 


# Demographics (Table One)--------
dt.BL = lst.pro[["baseline"]]
dem.cols = c("RaceWhite", "RaceBlack", "RaceAsian", "RacePacific", "RaceAmIn", "RaceOther", "RaceOtherSpecify", "Ethnicity", "Education", "HouseholdIncome", "Smoking", "LivingWithPerson", "ECOG")
dt.BL[, .SD, .SDcols = c(dem.cols)]

tableone::CreateTableOne(data = dt.BL[, .SD, .SDcols = c(dem.cols)], factorVars = dem.cols, includeNA = TRUE)

lccc_1543.dx  = fread('raw_data/LCCC_1543_diagnosis_treatment.csv', na.strings =  "") # Aggregate Diagnosis Data
tableone::CreateTableOne(data = lccc_1543.dx[, .SD, .SDcols = c("Solid_tumor_subtype", "Labels", "ENROLLMENT41", "ENROLLMENT42", "Transplant_Type",  "Condintioning_Type")], factorVars = colnames(lccc_1543.dx), includeNA = TRUE)

# Longitudinal + Swimmers Plot -------

# Create a Timeline Data Table

BL = lst.pro[["baseline"]][, c("FolderLabel", "FormStartDay", "FormStatusDay", "ActivityStartdate", "FormStatusDate", "ActivityStatus", "ActFrmStatus", "ECOG")][, setnames( .SD, paste0("bl_", names(.SD)))]

CPET = lst.pro[["cpet"]][, c("FolderLabel", "FormStartDay", "FormStatusDay", "ActivityStartdate",  "Status_CpetDate", "ActivityStatus", "ActFrmStatus", "CompleteCPET", "NoCpetOtherSpecify")][, setnames( .SD, paste0("cpet_", names(.SD)))]

sixminwalk = lst.pro[["walk.test"]][, c("FolderLabel", "FormStartDay", "FormStatusDay",  "ActivityStartdate",  "SixMWDDate", "ActivityStatus", "ActFrmStatus", "SixMWDStatus", "SixMWDBarrierSpecify")][, setnames( .SD, paste0("wt_", names(.SD)))]

FU.SAT = lst.pro[["followup.sat"]][, c("FolderLabel", "FormStartDay", "FormStatusDay", "ActivityStartdate",  "FormStatusDate", "ActivityStatus", "ActFrmStatus", "PFA11")][, setnames( .SD, paste0("sat_", names(.SD)))]

FU = dcast(lst.pro[["followup"]][, c("FolderLabel", "FormStartDay", "FormStatusDay",  "ActivityStartdate", "FormStatusDate", "ActivityStatus", "ActFrmStatus", "PFA11")][, setnames( .SD, paste0("fu_", names(.SD)))], fu_FolderLabel ~ fu_FormStartDay , value.var = c("fu_ActivityStatus", "fu_FormStatusDate", "fu_FormStatusDay", "fu_ActFrmStatus", "fu_PFA11"))

dt.timeline = merge(merge(merge(merge(BL, CPET, by.x = "bl_FolderLabel", by.y = "cpet_FolderLabel", all.x = TRUE), sixminwalk, by.x = "bl_FolderLabel", by.y = "wt_FolderLabel", all.x = TRUE), FU.SAT, by.x = "bl_FolderLabel", by.y = "sat_FolderLabel", all.x = TRUE), FU, by.x = "bl_FolderLabel", by.y = "fu_FolderLabel", all.x = TRUE)

tracker.activity = fread("dailyActivity_merged.csv")
tracker.activity = tracker.activity[, ActivityDate := as.Date(ActivityDate, "%m/%d/%Y")][TotalSteps > 100, ]
tracker.activity = merge(tracker.activity[, min(ActivityDate), by = "Id"], tracker.activity[, max(ActivityDate), by = "Id"], by = "Id")
setnames(tracker.activity, c("FolderLabel", "Tracker_Start", "Tracker_End"))
dt.timeline = as.data.table(merge(dt.timeline, tracker.activity, by.x = "bl_FolderLabel", by.y = "FolderLabel", all.x = TRUE))

# Indicator variables for completeness
dt.timeline[, BL_complete := ifelse(is.na(bl_ECOG), 0, 1)][, CPET_complete := ifelse((is.na(cpet_CompleteCPET) | cpet_CompleteCPET == 0), 0, 1)][, WALK_complete := ifelse(wt_SixMWDStatus %in% c(NA, 3), 0, 1)][, FU_0_complete := ifelse(is.na(fu_PFA11_0), 0, 1)][, FU_7_complete := ifelse(is.na(fu_PFA11_7), 0, 1)][, FU_14_complete := ifelse(is.na(fu_PFA11_14), 0, 1)][, FU_21_complete := ifelse(is.na(fu_PFA11_21), 0, 1)][, FU_SAT := ifelse(is.na(sat_PFA11), 0, 1)]

dt.timeline = dt.timeline [, c("bl_FolderLabel", "BL_complete", "CPET_complete", "WALK_complete", "FU_0_complete", "FU_7_complete", "FU_14_complete", "FU_21_complete", "FU_SAT", "Tracker_Start", "Tracker_End", "bl_FormStatusDate", "cpet_Status_CpetDate", "wt_SixMWDDate", "sat_FormStatusDate", "fu_FormStatusDate_0", "fu_FormStatusDate_7", "fu_FormStatusDate_14", "fu_FormStatusDate_21")]

dt.timeline[, Tracker_Start := as.Date(Tracker_Start, "%m/%d/%Y")][, Tracker_End := as.Date(Tracker_End, "%m/%d/%Y")]

dt.timeline[, bl_FormStatusDate := as.Date(bl_FormStatusDate)][, cpet_Status_CpetDate := as.Date(cpet_Status_CpetDate)][, wt_SixMWDDate := as.Date(wt_SixMWDDate)][, sat_FormStatusDate := as.Date(sat_FormStatusDate)]

dt.timeline[, fu_FormStatusDate_0 := as.Date(fu_FormStatusDate_0, "%m/%d/%Y %H:%M")][, fu_FormStatusDate_7 := as.Date(fu_FormStatusDate_7, "%m/%d/%Y %H:%M")][, fu_FormStatusDate_14 := as.Date(fu_FormStatusDate_14, "%m/%d/%Y %H:%M")][, fu_FormStatusDate_21 := as.Date(fu_FormStatusDate_21, "%m/%d/%Y %H:%M")]

# Add Date Range
dt.timeline.mlt = data.table::melt(dt.timeline[, c("bl_FolderLabel", "Tracker_Start", "Tracker_End", "bl_FormStatusDate", "cpet_Status_CpetDate", "wt_SixMWDDate", "sat_FormStatusDate", "fu_FormStatusDate_0", "fu_FormStatusDate_7", "fu_FormStatusDate_14", "fu_FormStatusDate_21")], id.vars = c("bl_FolderLabel", "Tracker_Start"))

dt.timeline.mlt[, days := value - Tracker_Start]
dt.timeline.mlt = dcast.data.table(dt.timeline.mlt, bl_FolderLabel ~ variable, value.var = "days")
names(dt.timeline.mlt) <- paste0("days_", names(dt.timeline.mlt))

dt.timeline = merge(dt.timeline, dt.timeline.mlt, by.x = "bl_FolderLabel", by.y = "days_bl_FolderLabel")

dt.timeline[, discontinued := ifelse(bl_FolderLabel %in% lst.pro[["baseline"]][!(ActivityStatus %in% c("Active", "Completed study activities")), `FolderLabel`], 1, 0)]

dt.timeline[, all_data := rowSums(dt.timeline[, c( "BL_complete", "CPET_complete", "WALK_complete", "FU_0_complete", "FU_7_complete", "FU_14_complete", "FU_21_complete")])][, bl_dat := rowSums(dt.timeline[, c( "BL_complete", "CPET_complete", "WALK_complete")])][, fu_dat := rowSums(dt.timeline[, c( "FU_0_complete", "FU_7_complete", "FU_14_complete", "FU_21_complete")])]

dt.timeline[, days_sat_FormStatusDate := ifelse(FU_SAT == 1, days_sat_FormStatusDate, NA)]
dt.timeline[, time_length_fu.sat := ifelse(FU_SAT == 1, days_sat_FormStatusDate, NA)]
dt.timeline[, time_length_fu.21 := ifelse(FU_21_complete == 1, days_fu_FormStatusDate_21, NA)]

dt.timeline[bl_FolderLabel == "413", bl_FormStatusDate := as.Date("2017-09-06")] # Requires a manual edit
dt.timeline[bl_FolderLabel == "413", days_bl_FormStatusDate :=  (bl_FormStatusDate - Tracker_Start)]

## Swimmers Plot -----


dt.swimmers_plt = data.table::melt(dt.timeline[, c("bl_FolderLabel", "FU_SAT", "days_Tracker_End", "time_length_fu.sat", "days_bl_FormStatusDate", "days_cpet_Status_CpetDate", "days_wt_SixMWDDate", "days_sat_FormStatusDate"  , "days_fu_FormStatusDate_0", "days_fu_FormStatusDate_7", "days_fu_FormStatusDate_14", "days_fu_FormStatusDate_21", "time_length_fu.21")], id.vars = c( "bl_FolderLabel", "FU_SAT",  "days_Tracker_End", "time_length_fu.sat", "time_length_fu.21"))

dt.swimmers_plt[, tracker.length := as.numeric(days_Tracker_End)]
dt.swimmers_plt = dt.swimmers_plt[tracker.length > 2,][!(is.na(tracker.length)),]
dt.swimmers_plt[, post_sat_dat := ifelse(tracker.length > time_length_fu.sat, T, F)]

dt.swimmers_plt[, time_stamp := as.numeric(value)][, value := NULL]
dt.swimmers_plt[, length := ifelse(FU_SAT == 1 & tracker.length > time_length_fu.sat, as.numeric(time_length_fu.sat), as.numeric(days_Tracker_End))]

dt.swimmers_plt[, tracker := ifelse(is.na(length), 0, 1)]
dt.swimmers_plt[, subject := as.character(bl_FolderLabel)]
dt.swimmers_plt[, length := ifelse(is.na(length) | length == 0, 1, length)]

dt.swimmers_plt[, Survey := 'FU']
dt.swimmers_plt[variable == "days_sat_FormStatusDate", Survey := 'FU_SAT']
dt.swimmers_plt[variable == "days_wt_SixMWDDate", Survey := 'SixMW']
dt.swimmers_plt[variable == "days_cpet_Status_CpetDate", Survey := 'CPET']
dt.swimmers_plt[variable == "days_bl_FormStatusDate", Survey := 'BL']

dt.swimmers_plt[, length := ifelse(length > 100 & is.na(time_length_fu.sat), time_length_fu.21 + 2, length)] # For Participant 413

fac.lbl.v2 = unique(dt.swimmers_plt[order(length, decreasing = T), subject])
dt.swimmers_plt[, subject := factor(subject, levels = fac.lbl.v2)]
dt.swimmers_plt = dt.swimmers_plt[order(subject),]
dt.swimmers_plt[, Survey := factor(Survey, levels = c("BL", "CPET", "SixMW", "FU", "FU_SAT"))]


#pdf("survey_plots/swimmer_revised_v2.pdf", width = 30, height = 14)
ggplot(dt.swimmers_plt, aes(x = subject, y = length)) +
  geom_bar(position="dodge", stat="identity", aes(width=0.8)) +
  geom_point(data=dt.swimmers_plt, 
             aes(subject, time_stamp, colour=Survey, shape=Survey), size=7) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=c("#264653")) +
  scale_shape_manual(values = c(17, 16, 18,  8,  15)) +
  scale_color_manual(values = c("#e76f51", "#f4a261", "#000000", "#e9c46a", "#2a9d8f")) + 
  labs (title = "Participants", x = "Subject", y = "Length (Days)") +
  theme_tufte(base_family='Helvetica', base_size = 30) + theme(plot.title = element_text(hjust = 0.5, face='bold')) +
  scale_x_discrete(limits = rev(levels(dt.swimmers_plt$subject)))  +
  geom_segment(data=dt.swimmers_plt[post_sat_dat==T,], aes(x=subject, xend=subject, y=length, yend=length+4), arrow=arrow(type='closed', length=unit(0.1, 'in')), alpha=.1) 
#dev.off()


# Unsupervised Analysis  ------

## Visualizing Assessments (Unsupervised Analysis) -----

pf_domains = fread("pf_domains.csv") # Table with assigned domains per PROMIS question
pf_domains[, domain := factor(domain, levels = 1:3, labels = c("Light", "Moderate", "Strenuous"))]

col.intensity = c("#f1faee", "#457b9d", "#e63946") # Domain Colors
names(col.intensity) = c("Light", "Moderate", "Strenuous")

pfa.names = names(lst.pro[["followup"]])[grepl("PF", names(lst.pro[["followup"]]))] # PROMIS questions

followup.pfa = rbind(lst.pro[["baseline"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", pfa.names)], lst.pro[["followup"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", pfa.names)], lst.pro[["followup.sat"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", pfa.names)])

fu.no.na = followup.pfa[!is.na(PFA11),] # Only complete entries

mat.pfa.mean = as.matrix(fu.no.na[, lapply(.SD, mean), by = c("FolderLabel")][, -c("FormStartDay")], rownames = "FolderLabel")

Heatmap(mat.pfa.mean[, unlist(pf_domains$question)],
        name = "PFA",
        col = brewer.pal(n = 5, name = "RdYlBu"),
        column_split = 3,
        row_dend_width  = unit(2, "cm"),
        row_split = 3,
        bottom_annotation = HeatmapAnnotation(df=data.frame(Domain = pf_domains[, domain]), name = "Domain", col = list(Domain = col.intensity)))

## PRSM Unsupervised Analysis -------

dt.prsm.domains = fread('lookup_tbls/prsm_domains.csv') # Table with assigned domains per PRSM Question
dt.prsm.domains = dt.prsm.domains[Group.No != "",] # Remove empty rows

prsm.names = names(lst.pro[["followup"]])[grepl("PRSM", names(lst.pro[["followup"]]))]
prsm.names = setdiff(prsm.names, c("PRSM_Rash")) # Remove rash question

followup.prsm = rbind(lst.pro[["baseline"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)],
                      lst.pro[["followup"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)], 
                      lst.pro[["followup.sat"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)]) # All PRSM data in one data.table

followup.prsm = followup.prsm[, source := c(rep(-1, 54), rep(c(0, 7, 14, 21), 42), rep(28, 42))] # Add Survey Source

followup.prsm = followup.prsm[order(FolderLabel, source), ]
followup.prsm[, `PRSM_OtherSymptoms` := NULL]

followup.prsm[, prsm_validation := (PRSM_Appetite_s +
                                      PRSM_Anxiety_s +
                                      PRSM_Constip_s +
                                      PRSM_Sad_s +
                                      PRSM_Diarrhea_f +
                                      PRSM_Fatigue_s +
                                      PRSM_Insomnia_s +
                                      PRSM_Nausea_s +
                                      PRSM_Pain_s +
                                      PRSM_ShortBrth_s)]
followup.prsm[, prsm_total := (PRSM_Appetite_s +
                                 PRSM_Anxiety_s +
                                 PRSM_Constip_s +
                                 PRSM_Sad_s +
                                 PRSM_Diarrhea_f +
                                 PRSM_Fatigue_s +
                                 PRSM_Insomnia_s +
                                 PRSM_Nausea_s +
                                 PRSM_Pain_s +
                                 PRSM_ShortBrth_s +
                                 PRSM_Memory_s +
                                 PRSM_Concentrate_s +
                                 PRSM_Headache_s +
                                 PRSM_Paresthesia_s +
                                 PRSM_Cough_s +
                                 PRSM_UrineIncont_f)]
followup.prsm[, prsm.sleep := (PRSM_Fatigue_s + PRSM_Insomnia_s)]
followup.prsm[, prsm.cognitive := (PRSM_Memory_s + PRSM_Concentrate_s)]
followup.prsm[, prsm.sensory:= (PRSM_Pain_s + PRSM_Headache_s + PRSM_Paresthesia_s)]
followup.prsm[, prsm.psych := (PRSM_Anxiety_s + PRSM_Sad_s)]
followup.prsm[, prsm.pulmonary := (PRSM_Cough_s +PRSM_ShortBrth_s)]
followup.prsm[, prsm.food:= (PRSM_Nausea_s + PRSM_Appetite_s)]
followup.prsm[, prsm.toileting := (PRSM_Diarrhea_f+ PRSM_Constip_s + PRSM_UrineIncont_f)]

followup.prsm.avg = followup.prsm[, lapply(.SD, mean, na.rm = TRUE), by = c("FolderLabel")]
followup.prsm.avg[, source := NULL][, FormStartDay := NULL]

prsm.labels = followup.prsm.avg[!is.na(PRSM_Fatigue_s), "FolderLabel"]
prsm.labels = unlist(prsm.labels, use.names=F) # Participants with PRSM data

mat.prsm.avg  = as.matrix(followup.prsm.avg[, 2:17]) # Convert to matrix
mat.prsm.avg[is.na(mat.prsm.avg)] <- 99 # Populate null with 99

mat.prsm.avg.v2 = subset(mat.prsm.avg, rownames(mat.prsm.avg) %in% prsm.labels)

#pdf('survey_plots/prsm_clinical_domains.pdf', width = 11, height = 8.5)
Heatmap(mat.prsm.avg.v2[, dt.prsm.domains$PRSM.Item],
        name = "Symptom Severity",
        col = c(brewer.pal(n = 5, name = "YlOrRd")),
        na_col = "grey",
        column_split = dt.prsm.domains$Domain.Name,
        row_split = 2,
        clustering_distance_rows = "euclidean",
        clustering_method_rows = "ward.D2",
        clustering_distance_columns = "euclidean",
        clustering_method_columns = "ward.D2",
        row_labels = rownames(mat.prsm.avg.v2))
#dev.off()


# Import Aggregate Tracker Data ----

dt.dailyCalories = fread("raw_data/dailyCalories_merged.csv")
dt.dailyIntensities = fread("raw_data/dailyIntensities_merged.csv")
dt.dailySteps = fread("raw_data/dailySteps_merged.csv")

dt.dailyCalories[, ActivityDay:= as.Date(ActivityDay, format = "%m/%d/%Y")]
dt.dailyIntensities[, ActivityDay:= as.Date(ActivityDay, format = "%m/%d/%Y")]
dt.dailySteps[, ActivityDay:= as.Date(ActivityDay, format = "%m/%d/%Y")]

tracker.activity = fread("dailyActivity_merged.csv")
tracker.activity = tracker.activity[, ActivityDate := as.Date(ActivityDate, "%m/%d/%Y")][TotalSteps > 100, ]
tracker.activity = merge(tracker.activity[, min(ActivityDate), by = "Id"], tracker.activity[, max(ActivityDate), by = "Id"], by = "Id")
setnames(tracker.activity, c("FolderLabel", "Tracker_Start", "Tracker_End"))

tracker.activity[, Id.chart := as.character(Id)]
tracker.dates = merge(tracker.activity, dt.startDates, by.x = "Id.chart", by.y = "id", all.x = T)
tracker.dates[, days.elapsed := (ActivityDate - as.Date(start.date))]
tracker.dates[, study.period:=cut(as.numeric(days.elapsed), breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels = c(0, 7, 14, 21, 28, 35))][, study.period:= as.numeric(study.period)]

tracker.dates[study.period == 1, study.period := 0]
tracker.dates[study.period == 2, study.period := 7]
tracker.dates[study.period == 3, study.period := 14]
tracker.dates[study.period == 4, study.period := 21]
tracker.dates[study.period == 5, study.period := 28]
tracker.dates[study.period == 6, study.period := 35]

tracker.dates[, Id.chart := NULL]

tracker.dates[, c("VeryActiveSpeed", "ModeratelyActiveSpeed", "LightActiveSpeed") := list((VeryActiveDistance/VeryActiveMinutes), ModeratelyActiveDistance/FairlyActiveMinutes, LightActiveDistance/LightlyActiveMinutes)]

tracker.dates[, study.period := as.integer(study.period)]
tracker.dates.median = tracker.dates[SedentaryMinutes != 1440,][TotalSteps > 100, c(1:3, 4:19, 22:25)][, lapply(.SD, function (x) as.double(median(x, na.rm = T))), by = c("Id", "study.period")]
tracker.dates.median[, Id.chart := as.character(Id)]

## Import Heart Rate Data -----

## Heart rate in 1 minute intervals
# labels
lbl.hr.min = fread(file.path(dir.labels, 'heartrate_1min.txt'))
o.patients = setNames(as.character(lbl.hr.min[,label]), lbl.hr.min[,label])

# load data
lst.hr.min = read.timeFile.lst(lbl.hr.min, col.time='Time')
invisible(lapply(o.patients, function(x) { lst.hr.min[[x]][, study.day:=as.integer(difftime(Time, dt.startDates[id==x, start.date], units='days'))]}))

dt.heart_rate = rbindlist(lst.hr.min, use.names = T, idcol = "id")
dt.heart_rate = merge(dt.heart_rate, dt.startDates, all.x = T, by = "id")

dt.heart_rate[, days.elapsed := as.Date(Time) - as.Date(start.date)]

dt.heart_rate[, study.period:=cut(as.numeric(days.elapsed), breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels = c(0, 7, 14, 21, 28, 35))][, study.period:= as.numeric(study.period)]
dt.heart_rate[study.period == 1, study.period := 0]
dt.heart_rate[study.period == 2, study.period := 7]
dt.heart_rate[study.period == 3, study.period := 14]
dt.heart_rate[study.period == 4, study.period := 21]
dt.heart_rate[study.period == 5, study.period := 28]
dt.heart_rate[study.period == 6, study.period := 35]

dt.heart_rate.new = merge(dt.heart_rate[Value > 0, list(id, study.day, Value)][, lapply(.SD, median), by = c("id", "study.day")]
                          , dt.heart_rate[Value > 0, list(id, study.day, Value)][, lapply(.SD, sd), by = c("id", "study.day")], by = c("id", "study.day"))

setnames(dt.heart_rate.new, c("id", "study.day", "median.HR", "sd.HR"))

dt.heart_rate.new = merge(dt.heart_rate.new, unique(dt.heart_rate[, list(id, study.day, study.period)]), by = c("id", "study.day"))
dt.heart_rate.new = dt.heart_rate.new[, list(id, median.HR, sd.HR, study.period)][, lapply(.SD, median), by = c("id", "study.period")] #Median of the Medians

## Import Sleep Data ------


## sleep
# labels
lbl.sleep = fread(file.path(dir.labels, 'sleepLogInfo.txt'))
o.patients = setNames(as.character(lbl.sleep[,label]), lbl.sleep[,label])

# load data
lst.sleep = read.timeFile.lst(lbl.sleep, col.time='StartTime')
invisible(lapply(o.patients, function(x) { lst.sleep[[x]][, study.day:=as.integer(difftime(StartTime, dt.startDates[id==x, start.date], units='days'))]}))

dt.sleep = rbindlist(lst.sleep, use.names = T, idcol = "id")
dt.sleep = dt.sleep[order(id, StartTime),]

dt.sleep[, endTimelimit := StartTime + hours(12)]
dt.sleep[, StartTime := as_datetime(StartTime)][, endTimelimit := as_datetime(endTimelimit)]
dt.sleep[, time := strftime(as.ITime(StartTime), format="%H:%M:%S")]
dt.sleep[, time := hour( parse_date_time(time, orders = "HMS"))]
dt.sleep = dt.sleep[time %in% c(18, 19, 20, 21, 22, 23, 0, 1, 2,3, 4, 5, 6, 7), ] # Limit it to main sleep times


dt.sleep[, between := ifelse(between(StartTime, shift(StartTime, 1L, type="lag"), shift(endTimelimit, 1L, type="lag")), 1, 0)]
dt.sleep[, Start.Time.Actual := ifelse(between(StartTime, shift(StartTime, 1L, type="lag"), shift(endTimelimit, 1L, type="lag")), shift(StartTime, 1L, type="lag"), StartTime)]
dt.sleep[, Start.Time.Actual := ifelse(between == 1 & shift(between, 1L, type = "lag") == 1, shift(StartTime, 2L, type="lag"), Start.Time.Actual)]

dt.sleep[, Start.Time.Actual := as.POSIXct(Start.Time.Actual, origin = lubridate::origin, format =  "%Y-%m-%d %H:%M:%OS", tz='UTC')]
dt.sleep[1:2, Start.Time.Actual := as.POSIXct(dt.sleep[1, StartTime])]


dt.sleep = merge(dt.sleep, dt.startDates, all.x = T, by = "id")

dt.sleep[, days.elapsed := as.Date(Start.Time.Actual) - as.Date(start.date)]
dt.sleep[, study.period:=cut(as.numeric(days.elapsed), breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels = c(0, 7, 14, 21, 28, 35))][, study.period:= as.numeric(study.period)]
dt.sleep[study.period == 1, study.period := 0]
dt.sleep[study.period == 2, study.period := 7]
dt.sleep[study.period == 3, study.period := 14]
dt.sleep[study.period == 4, study.period := 21]
dt.sleep[study.period == 5, study.period := 28]
dt.sleep[study.period == 6, study.period := 35]

dt.sleep.aggregated = dt.sleep[, list(id, Start.Time.Actual, Duration, MinutesAfterWakeup, MinutesAsleep, MinutesToFallAsleep, TimeInBed, AwakeDuration,AwakeCount,  RestlessCount, RestlessDuration)][, lapply(.SD, sum), by = c("id", "Start.Time.Actual")]

dt.sleep.efficiency = dt.sleep[, list(id, Start.Time.Actual, Efficiency)][, lapply(.SD, mean), by = c("id", "Start.Time.Actual")]

dt.sleep.aggregated = merge(dt.sleep.aggregated, dt.sleep.efficiency, by = c("id", "Start.Time.Actual"))
dt.sleep.aggregated = merge(dt.sleep.aggregated, dt.startDates, all.x = T, by = "id")
dt.sleep.aggregated[, days.elapsed := as.Date(Start.Time.Actual) - as.Date(start.date)]
dt.sleep.aggregated[, study.period:=cut(as.numeric(days.elapsed), breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels = c(0, 7, 14, 21, 28, 35))][, study.period:= as.numeric(study.period)]

# Relabel study periods
dt.sleep.aggregated[study.period == 1, study.period := 0]
dt.sleep.aggregated[study.period == 2, study.period := 7]
dt.sleep.aggregated[study.period == 3, study.period := 14]
dt.sleep.aggregated[study.period == 4, study.period := 21]
dt.sleep.aggregated[study.period == 5, study.period := 28]
dt.sleep.aggregated[study.period == 6, study.period := 35]

dt.sleep.aggregated[, study.day:= as.numeric(as_date(Start.Time.Actual) -  as_date(start.date))]

dt.sleep.daily = dt.sleep.aggregated[MinutesAsleep > 0, list(id, study.day, Efficiency, MinutesAfterWakeup, MinutesAsleep, MinutesToFallAsleep, TimeInBed, AwakeCount, AwakeDuration, RestlessCount, RestlessDuration)][, lapply(.SD, median), by = c("id", "study.day")]
dt.sleep.daily = merge(dt.sleep.daily, unique(dt.sleep[, list(id, study.day, study.period)]), by = c("id", "study.day")) # add back study.periods

dt.sleep.main = dt.sleep.daily[, lapply(.SD, median), by = c("id", "study.period")]


### Create Insomnia Table ----
dt.insomnia = as.data.table(mat.prsm.avg[, 11], keep.rownames = "id") # PRSM_Insomnia_s
setnames(dt.insomnia, c("id", "insomnia"))

dt.insomnia[, FolderLabel := as.integer(id)]

dt.insomnia[, insomnia_indicator := ifelse(insomnia >= 3, 1, 0)]
dt.insomnia = merge(dt.insomnia, followup.prsm[, list(FolderLabel, PRSM_Insomnia_s, study.period)], by = "FolderLabel", all.x = T)
dt.insomnia[, study.period := study.period + 7]

### Insomnia ANOVA Tests ------

bp.insomnia = merge(dt.sleep.main, dt.insomnia, by = c("id", "study.period"))
bp.insomnia[, insomnia := as.integer(insomnia)]


anova_test(data = bp.insomnia, AwakeDuration ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, Efficiency ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, MinutesAfterWakeup ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, TimeInBed ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, AwakeCount ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, RestlessCount ~ factor(insomnia), wid = id, within = study.period )
anova_test(data = bp.insomnia, RestlessDuration ~ factor(insomnia), wid = id, within = study.period )


# Six Min Walk Derived (Heart Rate Range) -----

### Merge HR and steps by minute ###

lst.hr.min = read.timeFile.lst(lbl.hr.min, col.time='Time')
lst.steps.min = read.timeFile.lst(lbl.steps.min, col.time='ActivityMinute')

vec.hr.lengths = sapply(lst.hr.min, nrow)
vec.steps.lengths = sapply(lst.steps.min, nrow)

vec.patients.merged = intersect(names(vec.hr.lengths[vec.hr.lengths>0]), names(vec.steps.lengths[vec.steps.lengths>0]))
names(vec.patients.merged) = vec.patients.merged

lst.hrSteps.min = lapply(vec.patients.merged, function(x) {merge(lst.hr.min[[x]][,list(Time, study.day, study.hour=hour(Time), HR=Value)], lst.steps.min[[x]][,list(Time=ActivityMinute, study.day, Steps)], by=c('Time', 'study.day')) } )

dt.hrSteps.min = rbindlist(lst.hrSteps.min, idcol='id')

sixmwd.range.hr = copy(dt.hrSteps.min)
sixmwd.range.hr = sixmwd.range.hr[, study.period:=cut(study.day, breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels=o.studyPeriod)]
sixmwd.range.hr[, day := as.Date(Time)]
sixmwd.range.hr = sixmwd.range.hr[, .SD, .SDcols = c("id", "HR", "day", "study.period")][, .(range = (quantile(HR, 0.9) - quantile(HR, 0.1))), by = c("id", "day", "study.period")][, lapply(.SD, mean), by = c("id", "study.period")]
sixmwd.range.hr[study.period == 'baseline', sixmwd.range.hr := -7]
sixmwd.range.hr[study.period == '1', sixmwd.range.hr := 0]
sixmwd.range.hr[study.period == '2', sixmwd.range.hr := 7]
sixmwd.range.hr[study.period == '3', sixmwd.range.hr := 14]
sixmwd.range.hr[study.period == '4', sixmwd.range.hr := 21]
sixmwd.range.hr[study.period == '5+', sixmwd.range.hr := 28]


# Tracker-Derived 6 minute walk ------

lst.steps.min = read.timeFile.lst(lbl.steps.min, col.time='ActivityMinute')
o.patients = setNames(as.character(lbl.steps.min[,label]), lbl.steps.min[,label])

invisible(lapply(o.patients, function(x) { lst.steps.min[[x]][, study.day:=as.integer(difftime(ActivityMinute, dt.startDates[id==x, start.date], units='days'))]}))

invisible(lapply(lst.steps.min, function(x) { x[, study.period:=cut(study.day, breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels=o.studyPeriod)]}))

lbl.steps.min = fread(file.path(dir.labels, 'minuteSteps.txt'))
o.patients = setNames(as.character(lbl.steps.min[,label]), lbl.steps.min[,label])

vec.count = sapply(lst.steps.min, function(x) {nrow(x[Steps>0,])})
o.patients.nz = names(vec.count[vec.count>0])
names(o.patients.nz) = o.patients.nz

## load continuous data ------
lst.steps.min = read.timeFile.lst(lbl.steps.min, col.time='ActivityMinute')

tmp = copy(lst.steps.min)

dt.steps.min = rbindlist(tmp[o.patients.nz])
dt.steps.min[, dist.6:=(sum.6 * vec.stepDist[patient])]

sixmwd.max.avg = dt.steps.min[dist.6 > 100,] 
sixmwd.max.avg[, day := as.Date(ActivityMinute)]
sixmwd.max.avg = sixmwd.max.avg[, .SD, .SDcols = c("patient", "dist.6", "day", "study.period")][, lapply(.SD,  max), by = c("patient", "day", "study.period")][, lapply(.SD, mean), by = c("patient", "study.period")]
sixmwd.max.avg[study.period == 'baseline', study.period := -7]
sixmwd.max.avg[study.period == '1', study.period := 0]
sixmwd.max.avg[study.period == '2', study.period := 7]
sixmwd.max.avg[study.period == '3', study.period := 14]
sixmwd.max.avg[study.period == '4', study.period := 21]
sixmwd.max.avg[study.period == '5+', study.period := 28]

## Sedentary Minutes/Intensities ----

intensities.timed = merge(dt.steps.min[,c("patient", "study.period", "day")], dt.dailyIntensities, by.x =c("patient", "day"), by.y = c("Id", "ActivityDay"), all.x = TRUE)
intensities.timed = intensities.timed[, .SD, .SDcols = c("patient", "SedentaryMinutes", "day", "study.period")][, lapply(.SD, mean), by = c("patient", "study.period")]
intensities.timed[study.period == 'baseline', study.period := -7]
intensities.timed[study.period == '1', study.period := 0]
intensities.timed[study.period == '2', study.period := 7]
intensities.timed[study.period == '3', study.period := 14]
intensities.timed[study.period == '4', study.period := 21]
intensities.timed[study.period == '5+', study.period := 28]


# Aggregate Data -----

vec.pro.pfa = c('baseline', 'followup', 'followup.sat')

lst.tscore = lapply(lst.pro[vec.pro.pfa], function(x) { merge(x[,list(id=as.character(FolderLabel), FormStartDay, Raw.Score=rowSums(.SD)), .SDcols=cols.pf], dt.promis.pf, by='Raw.Score', all.x=T)[, list(T.Score=max(T.Score)), by=c('id', 'FormStartDay')]})

# Heatmap clusters in data table
heatmap.clusters = fread("clusters.csv")
heatmap.clusters[, id := as.character(id)]
heatmap.clusters[, id.int := as.integer(id)]
heatmap.clusters[, T.Score.cluster := factor(T.Score.cluster, levels = c(1, 2), labels = c("High PFA", "Low PFA"))]
heatmap.clusters[, PRSM.cluster  := factor(PRSM.cluster , levels = c(1, 2), labels = c("Low PRSM", "High PRSM"))]

heatmap.clusters = merge(heatmap.clusters, cbind(t.score_wide[,1], tscore.mean = rowMeans(t.score_wide[, 2:7], na.rm = T)), all.x = T, by = "id")
heatmap.clusters = merge(heatmap.clusters, dt.walk[, c("FolderLabel", "SixMWDTotalMeters")], by.x = "id.int", by.y = "FolderLabel", all.x = T)
heatmap.clusters = merge(heatmap.clusters, dt.cpet[, c("FolderLabel", "CpetVo2Peak")], by.x = "id.int", by.y = "FolderLabel", all.x = T)
heatmap.clusters = merge(heatmap.clusters, lm.bl.v2[, c("patient", "dist.6")], by.x = "id.int", by.y = "patient", all.x = T)
heatmap.clusters[, CpetVo2Peak := as.numeric(CpetVo2Peak)]


# All T.Scores in a data table
dt.tscore = do.call('rbind', lst.tscore)
dt.tscore$source = c(rep("BL", 54), rep("FU", 168), rep("SAT", 42))
dt.tscore = as.data.table(dt.tscore)
dt.tscore[, FormStartDay := ifelse(source == "BL", -7, FormStartDay)]

dt.tscore = merge(dt.tscore, heatmap.clusters[, list(id, T.Score.cluster)], by.x = c("id"), by.y = c("id"), all.x = T) # Merge T Score  + clusters
t.score_wide = data.table::dcast(dt.tscore[, c("id", "FormStartDay", "T.Score")], id ~ FormStartDay, value.var = "T.Score")


# Symptom Burden
followup.prsm = rbind(lst.pro[["baseline"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)],
                      lst.pro[["followup"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)], 
                      lst.pro[["followup.sat"]][, .SD, .SDcols = c("FolderLabel", "FormStartDay", prsm.names)])

followup.prsm = as.data.table(followup.prsm)
followup.prsm = followup.prsm[, source := c(rep(-1, 54), rep(c(0, 7, 14, 21), 42), rep(28, 42))]

followup.prsm = followup.prsm[order(FolderLabel, source), ]
followup.prsm[, `PRSM_OtherSymptoms` := NULL]

prsm.sums = followup.prsm[complete.cases(followup.prsm)]
prsm.sums[, `:=` (FormStartDay = NULL, Id.chart = NULL)]
prsm.sums  = prsm.sums[, rowSums(.SD), by = c("FolderLabel", "source")]

# Linear Models -----

## Six Minute Max Average -----
sixmwd.max = dt.steps.min[dist.6 > 100,] # Weed out NAs
sixmwd.max = sixmwd.max[study.period %in% c("baseline", "1", "2"), .SD, .SDcols = c("patient", "dist.6")][, lapply(.SD, function(x) max(x)), by = "patient"]

sixmwd.max.avg = dt.steps.min[dist.6 > 100,] 
sixmwd.max.avg[, day := as.Date(ActivityMinute)]
sixmwd.max.avg = sixmwd.max.avg[, .SD, .SDcols = c("patient", "dist.6", "day", "study.period")][, lapply(.SD,  max), by = c("patient", "day", "study.period")][, lapply(.SD, mean), by = c("patient", "study.period")]

sixmwd.max.avg[study.period == "baseline", study.period := -7]
sixmwd.max.avg[study.period == "5+", study.period := 28]
sixmwd.max.avg[study.period == "1", study.period := 0]
sixmwd.max.avg[study.period == "2", study.period := 7]
sixmwd.max.avg[study.period == "3", study.period := 14]
sixmwd.max.avg[study.period == "4", study.period := 21]

sixmwd.max.avg.bl = merge(sixmwd.max.avg[, min(study.period), by = "patient"], sixmwd.max.avg, by.x = c("patient", "V1"), by.y = c("patient", "study.period"))   
sixmwd.max.avg.bl[, patient := as.integer(patient)]

## Six Minute Walk HR Range ------

sixmwd.range.hr.bl = merge(sixmwd.range.hr[, min(study.period), by = "id"], sixmwd.range.hr,  by.x= c("id", "V1"), by.y = c("id", "study.period"))
sixmwd.range.hr.bl[, id := as.integer(id)]

## Survey Data -----

survey.dat = merge(dt.tscore[FormStartDay == -7,][!is.na(T.Score),], prsm.sums[source == -7, ], by.x = "id", by.y = "FolderLabel")
survey.dat[, prsm := V1][, V1 := NULL]

## Walking Data -----

dt.walk = lst.pro[["walk.test"]]
dt.walk[dt.walk==''] <- NA
dt.walk = do.call(cbind, lapply(dt.walk, as.numeric))

## Intensities ------
intensities.timed = merge(dt.steps.min[,c("patient", "study.period", "day")], dt.dailyIntensities, by.x =c("patient", "day"), by.y = c("Id", "ActivityDay"), all.x = TRUE)
intensities.timed = intensities.timed[, .SD, .SDcols = c("patient", "SedentaryMinutes", "day", "study.period")][, lapply(.SD, mean), by = c("patient", "study.period")]

intensities.timed[study.period == "baseline", study.period := -7]
intensities.timed[study.period == "5+", study.period := 28]
intensities.timed[study.period == "1", study.period := 0]
intensities.timed[study.period == "2", study.period := 7]
intensities.timed[study.period == "3", study.period := 14]
intensities.timed[study.period == "4", study.period := 21]


## Linear Model Data Table ------
lm.bl.v2 = merge(sixmwd.max.avg.bl, survey.dat, by.x = "patient", by.y = "id")

lm.bl.v2 = merge(lm.bl.v2, sixmwd.range.hr.bl, by.x = "patient", by.y = "id")
lm.bl.v2 = merge(lm.bl.v2, intensities.timed, by.x = c("patient", "V1.x"), by.y = c("patient", "study.period"), all.x = TRUE)
lm.bl.v2 = merge(lm.bl.v2, dt.walk[, c("FolderLabel", "SixMWDTotalMeters")], by.x = "patient", by.y = "FolderLabel", all.x = TRUE)
lm.bl.v2 = merge(lm.bl.v2, heatmap.clusters[, -c("dist.6")], by.x = "patient", by.y = "id.int", all.x = T)

summary(lm(T.Score ~ dist.6, lm.bl.v2))
summary(lm(T.Score ~ prsm, lm.bl.v2)) # T Score vs Symptom Burden


## GLM: SixMinWalk ~ Sensor Derived -----

fit.walk.dat = copy(dt.hrSteps.min)

six.min.walk_participants = dt.timeline[WALK_complete == 1, ][!is.na(Tracker_Start), c("bl_FolderLabel", "wt_SixMWDDate")]
six.min.walk_participants[, bl_FolderLabel := as.character(bl_FolderLabel)]

fit.walk.dat  = merge(fit.walk.dat, six.min.walk_participants, by.x = "id", by.y = "bl_FolderLabel")
fit.walk.dat = fit.walk.dat[, fit_date := as.Date(Time)][fit_date == wt_SixMWDDate, ]

fit.walk.dat = merge(fit.walk.dat, dt.steps.min, by.x = c("id", "Time"), by.y = c("patient", "ActivityMinute"), all.x = TRUE)
fit.walk.dat = fit.walk.dat[order(id, -dist.6),]

estimated_distances = fit.walk.dat[, head(dist.6, 5), by="id"][, est_6mwd := V1][, V1 := NULL]
estimated_distances[, id := as.integer(id)]

dist.walk.test = merge(tracker.activity, six.min.walk_participants, by.x = c("Id", "ActivityDate"), by.y = c("bl_FolderLabel", "wt_SixMWDDate"))
dist.walk.test[, distance_total_meters := TrackerDistance * 1000][, stride := distance_total_meters/TotalSteps]
dist.walk.test = merge(dist.walk.test, dt.walk[, c("FolderLabel", "SixMWDTotalMeters")], by.x = "Id", by.y = "FolderLabel")

estimated_distances = merge(dist.walk.test[, c("Id", "distance_total_meters", "stride", "SixMWDTotalMeters")], estimated_distances, by.x = "Id", by.y = "id")
estimated_distances[, error := as.numeric((SixMWDTotalMeters-est_6mwd)**2)]

estimated_distances = unique(merge(estimated_distances[, min(error), by = "Id"], estimated_distances, by.x = c("Id", "V1"), by.y = c("Id", "error"), all.x = TRUE))
estimated_distances = estimated_distances[est_6mwd != 0,]

ggplot(estimated_distances, aes(est_6mwd, SixMWDTotalMeters, label = Id)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Estimated 6MWD", y = "Actual 6MWD") + geom_text_repel()

summary(lm(SixMWDTotalMeters ~ est_6mwd, estimated_distances)) # Six MW, estimated

# Longitudinal Models ------

# Merge Survey Data
survey.dat.long = merge(dt.tscore[!is.na(T.Score),], prsm.sums, by.x = c("id", "FormStartDay"), by.y = c("FolderLabel", "source"))
survey.dat.long[, prsm := V1][, V1 := NULL]

lmer.dist = merge(sixmwd.max.avg, survey.dat.long, by.x = c("patient", "study.period"), by.y = c("id", "FormStartDay"), all.x = TRUE, all.y = TRUE)
lmer.dist = merge(lmer.dist, sixmwd.range.hr, by.x = c("patient", "study.period"), by.y= c("id", "study.period"), all.x = TRUE, all.y = TRUE)
lmer.dist = merge(lmer.dist, intensities.timed,  by = c("patient", "study.period"), all.x = TRUE, all.y = TRUE)

# Merge everything
lmer.dist.daily = merge(tracker.dates.median, lmer.dist[, list(patient, study.period, dist.6, T.Score, source, prsm, day_since_BL, T.Score.cluster, PRSM.cluster)], by.x=c("Id.chart", "study.period"), by.y= c("patient", "day_since_BL"), all.y = T) # T-Score
lmer.dist.daily = merge(lmer.dist.daily, dt.heart_rate.new, by.x= c("Id.chart", "study.period"),  by.y = c("id", "study.period"), all.x = T) # HR
lmer.dist.daily = merge(lmer.dist.daily, dt.sleep.main, by.x= c("Id.chart", "study.period"),  by.y = c("id", "study.period"), all.x = T) # Sleep
lmer.dist.daily = merge(lmer.dist.daily, dt.insomnia, by.x= c("Id.chart", "study.period"),  by.y = c("id", "study.period"), all.x = T) # Insomnia
lmer.dist.daily = merge(lmer.dist.daily, followup.prsm, by.x= c("Id.chart", "study.period"),  by.y = c("Id.chart", "study.period"), all.x = T) # Symptom Burden

# Create new variables
lmer.dist.daily[, time := VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes]
lmer.dist.daily[, gait.speed:=((1000 * TotalDistance) / (60 * time))]
lmer.dist.daily[, active_distance := (VeryActiveDistance + ModeratelyActiveDistance)]
lmer.dist.daily[, c("VeryActiveSpeed", "ModeratelyActiveSpeed", "LightActiveSpeed") := list((VeryActiveDistance/VeryActiveMinutes), ModeratelyActiveDistance/FairlyActiveMinutes, LightActiveDistance/LightlyActiveMinutes)]

## Export Longitudinal Models ------
tab_model(lmerTest::lmer(T.Score ~ scale(median.HR) + scale(TotalSteps)  +  scale(prsm_total) + (1|Id), data = lmer.dist.daily))
tab_model(lmerTest::lmer(T.Score ~ scale(median.HR) + scale(LightlyActiveMinutes) + scale(VeryActiveMinutes)  +  scale(prsm_total) + (1|Id), data = lmer.dist.daily))


# Supplementary Plots ------

## T Score: Spaghetti Plot ----

mypal2 <- colorRampPalette(brewer.pal(6, "Reds"))
mypal <- colorRampPalette(brewer.pal(6, "Blues"))

ggplot(dt.tscore[dt.tscore$id %in% t.score_wide[complete.cases(t.score_wide)]$id,], aes(x = (FormStartDay + 7), y = T.Score, color = interaction(id, as.factor(T.Score.cluster)))) + geom_line() +  geom_point() + theme_minimal() + scale_color_manual(values = c(mypal(12), mypal2(12))) + labs(x = "Day", y = "T-Score", title = "T-Score vs Time") + theme(legend.position = "bottom")

## PRSM: Spaghetti Plot -----

prsm.sums.spaghetti = prsm.sums[, list(FolderLabel, source, prsm_validation, prsm_total)]
prsm.sums.spaghetti[, source := ifelse(source == -1, -7, source)]
prsm.sums.spaghetti = merge(prsm.sums.spaghetti, heatmap.clusters[, list(id.int, PRSM.cluster)], by.x = "FolderLabel", by.y= "id.int")

prms.sums.wide = dcast(prsm.sums.spaghetti, FolderLabel ~ source)

mypal2 <- colorRampPalette(brewer.pal(6, "Reds"))
mypal <- colorRampPalette(brewer.pal(6, "Blues"))

ggplot(prsm.sums.spaghetti[FolderLabel %in% prms.sums.wide[complete.cases(prms.sums.wide)]$FolderLabel,], aes(x = source + 7, y = prsm_total,  color = interaction(FolderLabel, as.factor(PRSM.cluster))))  + geom_line() +  geom_point() + labs(x = "Time Point", y = "PRSM Sum", title = "PRSM Sum vs Time") + theme_minimal() + scale_color_manual(values = c(mypal(12), mypal2(12))) + theme(legend.position = "bottom")

## Violin Plots -----

p4 = ggplot(lmer.dist.daily, aes(x= as.factor(study.period ), y=T.Score)) + 
  geom_violin(fill = "dodgerblue4") + labs(x = "Days Since Baseline", y =  "T.Score") +  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="black") # + lims(y = c(20, 65))

p5 = ggplot(lmer.dist.daily, aes(x= as.factor(study.period), y=prsm_validation)) + 
  geom_violin(fill = "dodgerblue4") + labs(x = "Days Since Baseline", y =  "PRSM") +  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="black") # + lims(y = c(10, 35))

p6 = ggplot(lmer.dist.daily, aes(x= as.factor(study.period), y=dist.6 )) + 
  geom_violin(fill = "dodgerblue4") + labs(x = "Days Since Baseline", y =  "Six Min Derived") +  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="black")

grid.arrange(p4, p5, p6, ncol = 1)

## Box Plots ------

p1 = ggplot(lmer.dist.daily, aes(x= as.factor(Id), y=T.Score)) + 
  geom_boxplot(fill = "darkred") + labs(x = "Participant", y =  "T.Score") +
  coord_flip()
p2 = ggplot(lmer.dist.daily, aes(x= as.factor(Id), y=prsm_total)) + 
  geom_boxplot(fill = "darkred") + labs(x = "Participant", y =  "PRSM") +
  coord_flip()
p3 = ggplot(lmer.dist.daily, aes(x= as.factor(Id), y=dist.6 )) + 
  geom_boxplot(fill = "cornsilk4") + labs(x = "Participant", y =  "Six Min Derived") +
  coord_flip()
p4 = ggplot(lmer.dist.daily, aes(x= as.factor(Id), y=median.HR )) + 
  geom_boxplot(fill = "cornsilk4") + labs(x = "Participant", y =  "Median HR") +
  coord_flip()
p5 = ggplot(lmer.dist.daily, aes(x= as.factor(Id), y=TotalSteps )) + 
  geom_boxplot(fill = "cornsilk4") + labs(x = "Participant", y =  "Total Steps") +
  coord_flip()
grid.arrange(p1, p2, nrow = 1)
grid.arrange(p3, p4, p5, nrow = 1)

## Correlation Plot ------

#pdf(file.path(dir.plots, "correlation_matrix.pdf"), width = 12, height = 10)
ggpairs(data = lmer.dist.daily[, list(T.Score, TotalSteps, VeryActiveDistance, LightActiveDistance, median.HR, prsm_total)], title = "Pairwise Comparisons", method = "spearman", lower = list(continuous = wrap("smooth", method = "lm",  colour = "darkslategray")), diag=list(continuous= wrap("barDiag", colour = "darkslategray"))) + p.theme.tufte
#dev.off()

#pdf(file.path(dir.plots, "correlation_matrix_sixMWD.pdf"), width = 12, height = 10)
ggpairs(data = lm.bl.v2[, list(dist.6.x, T.Score, SixMWDTotalMeters.x, prsm, CpetVo2Peak)], title = "Pairwise Comparisons", method = "spearman", columnLabels = c("Derived Six Min Walk", "T-Score", "Six Min Walk Distance", "Symptom Burden", "VO2 Max"), lower = list(continuous = wrap("smooth", method = "lm",  colour = "darkslategray")), diag=list(continuous= wrap("barDiag", fill = "#1c3664", color = "#000000"))) + p.theme.tufte
#dev.off()


# Data Completeness -----

## Continuous Heart Rate Data -----
dt.heart_rate = rbindlist(lst.hr.min, use.names = T, idcol = "id")
dt.heart_rate = merge(dt.heart_rate, dt.startDates, all.x = T, by = "id")

dt.heart_rate[, days.elapsed := as.Date(Time) - as.Date(start.date)]

dt.heart_rate[, study.period:=cut(as.numeric(days.elapsed), breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels = c(0, 7, 14, 21, 28, 35))][, study.period:= as.numeric(study.period)]

dt.heart_rate[study.period == 1, study.period := 0]
dt.heart_rate[study.period == 2, study.period := 7]
dt.heart_rate[study.period == 3, study.period := 14]
dt.heart_rate[study.period == 4, study.period := 21]
dt.heart_rate[study.period == 5, study.period := 28]
dt.heart_rate[study.period == 6, study.period := 35]


## Aggregating Heart Data ------
dt.heart_rate.daily = dt.heart_rate[, list(id, Value, study.day)][, median(Value), by = c("id", "study.day")]
dt.heart_rate.daily[, id := as.integer(id)]

## Sync Data -----

# labels
lbl.sync = fread(file.path(dir.labels, 'syncEvents.txt'))
o.patients = setNames(as.character(lbl.sync[,label]), lbl.sync[,label])

# load data
lst.sync = read.timeFile.lst(lbl.sync, col.time=c('DateTime', 'SyncDateUTC'))
invisible(lapply(o.patients, function(x) { lst.sync[[x]][, study.day:=as.integer(difftime(DateTime, dt.startDates[id==x, start.date], units='days'))]}))
mat.sync.count = lstToMatrix.mat(lst.sync, str.id='study.day', str.measure='study.day', fn.aggregate=length)

sync.data = rbindlist(lst.sync, use.names = TRUE, idcol = "id")


## Earliest Sync Date Per Person -----
first.sync.date = sync.data[, list(id, day)][, min(day), by = "id"]
setnames(first.sync.date, c("id", "first_sync_date"))
first.sync.date[, id := as.integer(id)]

# Sync Counts, Missing Steps, Sedentary, Days Between Sync
# Merge Aggregate and Continuous Steps
dt.intervals = merge(tracker.activity[!(Id %in% incomplete.ids), list(Id, ActivityDate, TotalSteps)], dt.steps.daily.derived, by.x = c("Id", "ActivityDate"), by.y = c("patient", "day"), all.x = T, all.y = T)

# Boolean Indicator
dt.intervals[, missing_continuous_steps := ifelse(TotalSteps > 0 & (is.na(daily.steps) | daily.steps == 0), 1, 0)]
dt.intervals[, sedentary := ifelse(TotalSteps == 0, 1, 0)]

# Merge HR Data
dt.intervals = merge(dt.intervals, dt.heart_rate.daily, all.x= T, by.x = c("Id", "study.day"), by.y = c("id", "study.day"))
dt.intervals[, missing_HR := ifelse(is.na(V1), 1, 0)]

dt.intervals = merge(dt.intervals, dt.startDates,  by.x = "Id", by.y = "id")
dt.intervals[is.na(date), date := as_date(start.date) + study.day]

dt.intervals = merge(dt.intervals, first.sync.date, by.x = "Id", by.y = "id", all.x = T)
dt.intervals = merge(dt.intervals, sync.data.daily, by.x = c("Id", "study.day"), by.y = c("id", "study.day"), all.x = T)
dt.intervals = dt.intervals[order(Id, study.day),]
dt.intervals[, last.sync := na.locf(sync.day, fromLast = TRUE)]
dt.intervals[, days.btwn.sync := as.numeric(last.sync - ActivityDate)]


dt.intervals = dt.intervals[study.day < 40]
dt.intervals[, study.period.y := NULL]

dt.boolean = copy(dt.intervals)
dt.boolean = dt.boolean[, list(Id, study.day,start.date,  ActivityDate, TotalSteps, study.period.x, daily.steps, missing_continuous_steps, sedentary, missing_HR, last.sync, days.btwn.sync)]
dt.boolean[, missing_steps := missing_day][, missing_HR := missing][, sedentary := non_missing_zero][, `:=` (missing_day = NULL,  missing = NULL, non_missing_zero = NULL)]

dt.boolean = cbind(dt.boolean[, list(Id, study.day, study.period.x, ActivityDate, start.date, days.btwn.sync,  last.sync,  TotalSteps)], dt.boolean[, list(TotalSteps)][, lapply(.SD, function(x) x <- ifelse(x > 0, F, T))], 
                   dt.boolean[, list(missing_continuous_steps, missing_HR, sedentary)][, lapply(.SD, function(x) x <- ifelse(x == 1, T, F))],
                   dt.boolean[, list(days.btwn.sync)][, lapply(.SD, function(x) x <- ifelse(as.numeric(x) <= 7, T, F))], 
                   dt.boolean[, list(days.btwn.sync)][, lapply(.SD, function(x) x <- ifelse(as.numeric(x) <= 30, T, F))])

setnames(dt.boolean, c("patient", "study.day", "study.period", "date", "start.date", "Days_Between_Sync", "last.sync.date", "TotalSteps", "Missing_TotalSteps", "Missing_Steps", "Missing_HR", "Sedentary", "Days_Btwn_Sync_7", "Days_Btwn_Sync_30"))

dt.boolean = dt.boolean[date >= start.date,]
dt.boolean[, .N, by= c("Missing_TotalSteps", "Missing_Steps", "Missing_HR", "Sedentary", "Days_Btwn_Sync_7", "Days_Btwn_Sync_30")]
dt.boolean[is.na(Missing_TotalSteps),]

dt.boolean[Missing_HR == F & Missing_Steps == T,]

dt.boolean[patient %in% c(103, 111, 112, 115, 116) & study.day == 0, Missing_HR := F]
dt.boolean[patient %in% c(106, 202, 205, 307) & study.day == 0, Missing_HR := T]
dt.boolean[is.na(Missing_HR) & patient == 106, Missing_HR := T]
dt.boolean[(Missing_TotalSteps) == T & Missing_Steps == F,]
dt.boolean[Days_Btwn_Sync_7 == T & Missing_HR == T & Missing_Steps == T,]

unique(dt.hrSteps.min[id %in% c(106, 202, 205, 307) & study.day == 0,]$id)
unique(dt.steps.min[patient %in% c(106, 202, 205, 307) & study.day == 0,])

dt.boolean[Missing_TotalSteps == T & Missing_Steps == F,]
dt.boolean[Days_Btwn_Sync_7 == T & Missing_HR == T & Missing_Steps == T & Missing_TotalSteps == T,]
dt.boolean[Days_Btwn_Sync_7 == T & Missing_HR == T & Missing_Steps == T,]
dt.boolean[Days_Btwn_Sync_30 == F & Missing_HR == F & Missing_Steps == F,]
dt.boolean[Missing_TotalSteps == T & Missing_HR == F & Sedentary == T,]


## Label Matrix Heatmap -----
dt.lookup = fread('lookup_tbls/lookup_labels_v2.csv')
vec.data.labels  = unlist(unique(dt.lookup$Label))
vec.data.labels = c( vec.data.labels, "Tracker Not In Use")
dt.boolean.labeled = merge(dt.boolean, dt.lookup, by = c("Missing_TotalSteps", "Missing_Steps", "Missing_HR", "Sedentary", "Days_Btwn_Sync_7", "Days_Btwn_Sync_30"), all.x = T)

dt.boolean.labeled = dt.boolean.labeled[, list(patient, study.day, Label_Factor)]
dt.boolean.labeled = dcast(dt.boolean.labeled, study.day ~ patient, value.var = "Label_Factor")
mat.boolean.labeled = as.matrix(dt.boolean.labeled, rownames = "study.day")

vec.col.labels = c("1" = "#25136C", "2" = "#103E93", "3" = "#04674D","4" = "#D8B952", "5" = "#D75E4E", "6" = "#963239", "7"= "lightgrey")

#pdf('survey_plots/heatmap_data_capture_hc_4.pdf', width = 11, height = 8.5)
Heatmap(mat.boolean.labeled, 
        cluster_rows = F, 
        cluster_columns = T, 
        column_split = 4,
        col = vec.col.labels, 
        rect_gp = gpar(col = "white", lwd = 1), 
        heatmap_legend_param = list(direction = "horizontal", labels =vec.data.labels,  title = "Data Capture Status", legend_gp = vec.col.labels, col_fun = col_fun),
        row_title = "Study Day", 
        column_title = "Participant")
#dev.off()


## Tabulating inactive patients ----
lst.pro[["walk.test"]][, .N, by = c("ActivityStatus", "ActFrmStatus")]
lst.pro[["cpet"]][, .N, by = c("ActivityStatus", "ActFrmStatus")]
lst.pro[["followup.sat"]][, .N, by = c("ActivityStatus", "ActFrmStatus")]
lst.pro[["followup"]][, .N, by = c("FormStartDay", "ActivityStatus", "ActFrmStatus")][order(ActFrmStatus),]

inactive.pts <- lst.pro[["cpet"]][ActivityStatus != "Active", ][ActivityStatus != "Completed study activities",`FolderLabel`]

setdiff(setdiff(lst.pro[["baseline"]]$FolderLabel, lst.pro[["followup"]]$FolderLabel), inactive.pts)

lst.pro[["walk.test"]][, .N, by = c("ActivityStatus", "ActFrmStatus", "FormStartDay")]
lst.pro[["baseline"]][is.na(ECOG), FolderLabel] #Empty Baseline
