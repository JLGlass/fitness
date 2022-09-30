library(data.table)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(ggplot2)
library(ggthemes)
#library(reshape2)
library(gridExtra)

library(fitness)

### FUNCTIONS ###


min_na <- function(x, na.rm=T, ...) {
  if (length(x) <= 0) {
    return(NA)
  }
  
  return(min(x, na.rm=na.rm, ...))
}


max_na <- function(x, na.rm=T, ...) {
  if (length(x) <= 0) {
    return(NA)
  }
  
  return(max(x, na.rm=na.rm, ...))
}


median_numeric <- function(x, ...) { 
  num = as.numeric(x)
  return(median(num, ...))
}

### Constants ###

#dir.root = 'G:/lab/fitness/'
dir.root = '/Volumes/HOME/lab_github/fitness/data'
#dir.root = '~/fitness/data'
dir.data.complete = file.path(dir.root, 'complete_data')
dir.labels = file.path(dir.root, 'labels')
dir.steps_hourly = file.path(dir.root, 'Hourly_Step_Data')
dir.hr_min = file.path(dir.root, 'Heart_Rate_Data')

dir.plots = file.path(dir.root, 'plots')
dir.surveys = file.path(dir.root, 'PRO & Other Data')


## Colors

col.steps.low = colorRamp2(c(0, 200, 1000), c('white', 'dodgerblue4', 'dodgerblue1'))
col.steps.hourly = colorRamp2(c(0, 1000, 3000), c('white', 'dodgerblue4', 'dodgerblue1'))
col.steps.daily = colorRamp2(c(0, 5000, 15000), c('white', 'dodgerblue4', 'dodgerblue1'))

col.hr = colorRamp2(c(60, 100, 150), c('white', 'firebrick4', 'firebrick1'))
col.hr.sd = colorRamp2(c(0, 10, 50), c('white', 'firebrick4', 'firebrick1'))


col.pfb24 = c('gray', colorRamp2(c(1, 2, 5), c('black', 'goldenrod4', 'goldenrod1'))(1:5))
names(col.pfb24) = c('NA', as.character(1:5))

col.ecog = c('gray', colorRamp2(c(0,2,5), c('darkslategray1', 'darkslategray4', 'black'))(0:5))
names(col.ecog) = c('NA', as.character(0:5))

col.tscore.pf = colorRamp2(c(0,20,70), c('black', 'darkseagreen4', 'darkseagreen1'))


## Themes

p.theme.tufte = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'))
p.theme.tufte.xaxisAngle = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'), axis.text.x=element_text(angle=45, hjust=1, vjust=1), axis.title.x=element_blank(), axis.ticks.x=element_blank())
p.theme.tufte.xaxis90 = theme_tufte(base_family='Helvetica') + theme(plot.title = element_text(hjust = 0.5, face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), axis.title.x=element_blank())


### Script ###


## subjective data ##

## load survey data

lbl.pro = fread(file.path(dir.labels, 'pro_data.txt'))
vec.pro = lbl.pro[, label]

lst.pro = readTables.lst(lbl.pro)

# physical function columns (has Raw.Score 99 duplicated for Raw.Score 100)
dt.promis.pf = fread(file.path(dir.root, 'tables/promis_pfa.txt'))
dt.promis.pf[, Raw.Score:=as.numeric(Raw.Score)]
setkey(dt.promis.pf, 'Raw.Score')

cols.pf = grep('PF', colnames(lst.pro[['baseline']]), value=T)

# assessments with PFA
vec.pro.pfa = c('baseline', 'followup', 'followup.sat')

# get tscores from PFA assessments (incompletes will be NA)
lst.tscore = lapply(lst.pro[vec.pro.pfa], function(x) { merge(x[,list(id=as.character(FolderLabel), FormStartDay, Raw.Score=rowSums(.SD)), .SDcols=cols.pf], dt.promis.pf, by='Raw.Score', all.x=T)[, list(T.Score=max(T.Score)), by=c('id', 'FormStartDay')]})
lst.ecog = lapply(lst.pro[vec.pro.pfa], function(x) { x[,list(id=as.character(FolderLabel), FormStartDay, ECOG)]})

# convert to data tables of id vs. assessment day
dt.tscore = as.data.table(dcast(do.call('rbind', lst.tscore), id ~ FormStartDay, value.var='T.Score', fun.aggregate=mean, fill=NA))
dt.ecog = as.data.table(dcast(do.call('rbind', lst.ecog), id ~ FormStartDay, value.var='ECOG', fun.aggregate=mean, fill=NA))


# study start dates
dt.startDates = lst.pro[['baseline']][, list(id=as.character(FolderLabel), start.date=as.POSIXct(ActivityStartdate, format='%m/%d/%y', tz='UTC'))]
setkey(dt.startDates, 'id')

vec.assessments = c('baseline'='0', 'followup.1'='7', 'followup.2'='14', 'followup.3'='21', 'followup.4'='28')


### run once to generate label files

lbl.sleep = createLabels.dt(dir.data.complete, '*sleepLogInfo*')
write.table(lbl.sleep, file=file.path(dir.labels, 'sleepLogInfo.txt'), row.names=F, quote=F, sep='\t')

lbl.mets.min = createLabels.dt(dir.data.complete, '*minuteMETsNarrow*')
write.table(lbl.mets.min, file=file.path(dir.labels, 'minuteMETs.txt'), row.names=F, quote=F, sep='\t')

lbl.sync = createLabels.dt(dir.data.complete, '*syncEvents*')
write.table(lbl.sync, file=file.path(dir.labels, 'syncEvents.txt'), row.names=F, quote=F, sep='\t')

lbl.dailyActivity = createLabels.dt(dir.data.complete, '*dailyActivity*')
write.table(lbl.dailyActivity, file=file.path(dir.labels, 'dailyActivity.txt'), row.names=F, quote=F, sep='\t')

lbl.hr.min = createLabels.dt(dir.data.complete, '*heartrate_1min*')
write.table(lbl.hr.min, file=file.path(dir.labels, 'heartrate_1min.txt'), row.names=F, quote=F, sep='\t')

lbl.steps.min = createLabels.dt(dir.data.complete, '*minuteStepsNarrow*')
write.table(lbl.steps.min, file=file.path(dir.labels, 'minuteSteps.txt'), row.names=F, quote=F, sep='\t')

lbl.intensity.min = createLabels.dt(dir.data.complete, '*minuteIntensitiesNarrow*')
write.table(lbl.intensity.min, file=file.path(dir.labels, 'minuteIntensities.txt'), row.names=F, quote=F, sep='\t')

lbl.calories.min = createLabels.dt(dir.data.complete, '*minuteCaloriesNarrow*')
write.table(lbl.calories.min, file=file.path(dir.labels, 'minuteCalories.txt'), row.names=F, quote=F, sep='\t')

### end run once


## sleep
# labels
lbl.sleep = fread(file.path(dir.labels, 'sleepLogInfo.txt'))
o.patients = setNames(as.character(lbl.sleep[,label]), lbl.sleep[,label])

# load data
lst.sleep = read.timeFile.lst(lbl.sleep, col.time='StartTime')
invisible(lapply(o.patients, function(x) { lst.sleep[[x]][, study.day:=as.integer(difftime(StartTime, dt.startDates[id==x, start.date], units='days'))]}))
mat.sleep = lstToMatrix.mat(lst.sleep, str.id='study.day', str.measure='MinutesAsleep')

# plot heatmap
col.sleep = colorRamp2(c(0, 60, 180, 300, 1000), brewer.pal(5, 'YlGnBu'))

vec.split = cut(as.numeric(rownames(mat.sleep)), seq(-30, 430, 30), labels=0:14)

pdf(file.path(dir.plots, 'heatmap_sleep_minutes.pdf'), width=10, height=8)
draw(Heatmap(mat.sleep, name='Minutes asleep', cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray95', col=col.sleep, column_title='Sleep in minutes per day', split=vec.split), row_title='Study month')
dev.off()

## METs
# labels
lbl.mets.min = fread(file.path(dir.labels, 'minuteMETs.txt'))
o.patients = setNames(as.character(lbl.mets.min[,label]), lbl.mets.min[,label])


# load data
lst.mets.min = read.timeFile.lst(lbl.mets.min, col.time='ActivityMinute')
invisible(lapply(o.patients, function(x) { lst.mets.min[[x]][, study.day:=as.integer(difftime(ActivityMinute, dt.startDates[id==x, start.date], units='days'))]}))

mat.mets.min = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=min_na)
mat.mets.max = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=max_na)
mat.mets.mean = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=mean)
mat.mets.median = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=median_numeric)
mat.mets.sd = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=sd)
mat.mets.sum = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs')

# plot heatmap
col.mets.veryLow = colorRamp2(c(0, 5, 10), brewer.pal(3, 'YlOrRd'))
col.mets.low = colorRamp2(c(0, 10, 20), brewer.pal(3, 'YlOrRd'))
col.mets.mid = colorRamp2(c(0, 10, 25, 50, 100, 200), brewer.pal(6, 'YlOrRd'))
col.mets.high = colorRamp2(c(0, 5000, 10000, 20000, 25000, 30000), brewer.pal(6, 'YlOrRd'))

vec.split = cut(as.numeric(rownames(mat.mets.sum)), seq(-30, 430, 30), labels=0:14)

pdf(file.path(dir.plots, 'heatmap_mets.pdf'), width=10, height=8)
draw(Heatmap(mat.mets.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (min)', column_title='Activity in METs (daily minimum)', col=col.mets.veryLow, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (max)', column_title='Activity in METs (daily maximum)', col=col.mets.mid, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (mean)', column_title='Activity in METs (daily mean)', col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (median)', column_title='Activity in METs (daily median)', col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (sd)', column_title='Activity in METs (daily SD)', col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.sum, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (daily sum)', column_title='Activity in METs (daily sum)', col=col.mets.high, na_col='gray95', split=vec.split), row_title='Study month')
dev.off()


## Sync events
# labels
lbl.sync = fread(file.path(dir.labels, 'syncEvents.txt'))
o.patients = setNames(as.character(lbl.sync[,label]), lbl.sync[,label])

# load data
lst.sync = read.timeFile.lst(lbl.sync, col.time=c('DateTime', 'SyncDateUTC'))
invisible(lapply(o.patients, function(x) { lst.sync[[x]][, study.day:=as.integer(difftime(DateTime, dt.startDates[id==x, start.date], units='days'))]}))
mat.sync.count = lstToMatrix.mat(lst.sync, str.id='study.day', str.measure='study.day', fn.aggregate=length)

# plot heatmap
col.sync = colorRamp2(c(1, 2, 5, 10, 100), brewer.pal(5, 'Greens'))

vec.split = cut(as.numeric(rownames(mat.sync.count)), seq(-30, 900, 30), labels=0:30)

mat.sync.count[mat.sync.count == 0] <- NA
pdf(file.path(dir.plots, 'heatmap_sync.pdf'), width=10, height=8)
draw(Heatmap(mat.sync.count[0:30,], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Count', column_title='Sync frequency (per day)', col=col.sync, na_col='gray90', row_title='Time (Days)'))
dev.off()

## Daily activity log
# labels
lbl.dailyActivity.all = fread(file.path(dir.labels, 'dailyActivity.txt'))
lbl.dailyActivity = lbl.dailyActivity.all[label %in% dt.startDates[,id],]
o.patients = setNames(as.character(lbl.dailyActivity[,label]), lbl.dailyActivity[,label])

# load data
cols.active = c('VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes')
cols.sedentary = c('SedentaryMinutes')
cols.activeMinutes = c('VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes', 'SedentaryMinutes')
cols.allMinutes = c(cols.activeMinutes, 'BedMinutes')
cols.distance = c('TotalDistance', 'VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance')

lst.dailyActivity = read.timeFile.lst(lbl.dailyActivity, col.time='ActivityDate', str.format='%m/%d/%Y')
invisible(lapply(o.patients, function(x) { lst.dailyActivity[[x]][, study.day:=as.integer(difftime(ActivityDate, dt.startDates[id==x, start.date], units='days', tz='UTC'))]}))

# merge time in bed with activity minutes for comprehensive time table

lst.minutes = Map(function(x,y) {merge(x[, .SD, .SDcols=c('study.day', cols.activeMinutes, cols.distance)], y, by='study.day', all.x=T)}, lst.dailyActivity[o.patients], lapply(lst.sleep[o.patients], function(x) { x[,list(BedMinutes=sum(TimeInBed, na.rm=T)), by='study.day']}))

invisible(lapply(lst.minutes, function(x) {x[, minutes.all:=sum(.SD, na.rm=T), .SDcols=cols.allMinutes, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, minutes.active:=sum(.SD, na.rm=T), .SDcols=cols.active, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.active:=sum(.SD, na.rm=T)/minutes.all, .SDcols=cols.active, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.veryActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='VeryActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.fairlyActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='FairlyActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.fairlyActive.cum:=sum(.SD, na.rm=T)/minutes.active, .SDcols=c('VeryActiveMinutes', 'FairlyActiveMinutes'), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.lightlyActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='LightlyActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.pct:=hoursToEcog.pct(pct.active), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.num:=hoursToEcog.num(minutes.active/60), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.pct.corrected:=correctEcog.pct(ecog.pct, pct.veryActive, pct.fairlyActive), by='study.day']}))

# convert km/min -> m/s 
invisible(lapply(lst.minutes, function(x) {x[, speed.all:=(1000 * TotalDistance / (60 * sum(.SD))), .SDcols=cols.activeMinutes, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.veryActive:=(1000 * VeryActiveDistance / (60 * VeryActiveMinutes)), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.fairlyActive:=(1000 * ModeratelyActiveDistance / (60 * FairlyActiveMinutes)), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.lightlyActive:=(1000 * LightActiveDistance / (60 * LightlyActiveMinutes)), by='study.day']}))


mat.pctActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.active', fn.aggregate=function(x) {mean(x)*100})
mat.pctVeryActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.veryActive', fn.aggregate=function(x) {mean(x)*100})
mat.pctFairlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.fairlyActive', fn.aggregate=function(x) {mean(x)*100})
mat.pctFairlyActive.cum = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.fairlyActive.cum', fn.aggregate=function(x) {mean(x)*100})
mat.pctLightlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.lightlyActive', fn.aggregate=function(x) {mean(x)*100})

mat.speedAll = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.all', fn.aggregate=function(x) {mean(x)})
mat.speedVeryActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.veryActive', fn.aggregate=function(x) {mean(x)})
mat.speedFairlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.fairlyActive', fn.aggregate=function(x) {mean(x)})
mat.speedLightlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.lightlyActive', fn.aggregate=function(x) {mean(x)})



o.cols = lst.tscore[['baseline']][as.character(id) %in% colnames(mat.pctActive),][order(T.Score, decreasing=T), as.character(id)]
vec.tscore = setNames(lst.tscore[['baseline']][, T.Score], lst.tscore[['baseline']][, as.character(id)])
vec.ecog = setNames(lst.ecog[['baseline']][, ECOG], lst.ecog[['baseline']][, as.character(id)])

# plot
col.pctActive = colorRamp2(c(0, 30, 49, 51, 70, 100), c(rev(brewer.pal(3, 'YlGnBu')), brewer.pal(3, 'YlOrRd')))

vec.split = cut(as.numeric(rownames(mat.pctActive)), seq(-30, 240, 30), labels=0:8)

ha = HeatmapAnnotation(df = data.frame(PF.Tscore=vec.tscore[o.cols], ECOG=vec.ecog[o.cols]), col=list(PF.Tscore=col.tscore.pf, ECOG=col.ecog), show_annotation_name=T)

pdf(file.path(dir.plots, 'heatmap_pctActive.pdf'), width=10, height=8)
draw(Heatmap(mat.pctActive[,o.cols], name='%', column_title='Overall activity percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctVeryActive[,intersect(o.cols, colnames(mat.pctVeryActive))], name='%', column_title='Very active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctFairlyActive[,intersect(o.cols, colnames(mat.pctFairlyActive))], name='%', column_title='Fairly active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctFairlyActive.cum[,intersect(o.cols, colnames(mat.pctFairlyActive))], name='%', column_title='Fairly active or better percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctLightlyActive[,intersect(o.cols, colnames(mat.pctLightlyActive))], name='%', column_title='Lightly active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
dev.off()


col.speed = colorRamp2(c(0, 0.4, 0.6, 0.8, 1), brewer.pal(5, 'YlOrRd'))

pdf(file.path(dir.plots, 'heatmap_speed.pdf'), width=10, height=8)
draw(Heatmap(mat.speedAll[,o.cols], name='m/s', column_title='Cohort 2: Overall gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedVeryActive[,intersect(o.cols, colnames(mat.speedVeryActive))], name='m/s', column_title='Cohort 2: Very active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedFairlyActive[,intersect(o.cols, colnames(mat.speedFairlyActive))], name='m/s', column_title='Cohort 2: Fairly active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedLightlyActive[,intersect(o.cols, colnames(mat.speedLightlyActive))], name='m/s', column_title='Cohort 2: Lightly active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
dev.off()


## Heart rate in 1 minute intervals
# labels
lbl.hr.min = fread(file.path(dir.labels, 'heartrate_1min.txt'))
o.patients = setNames(as.character(lbl.hr.min[,label]), lbl.hr.min[,label])

# load data
lst.hr.min = read.timeFile.lst(lbl.hr.min, col.time='Time')
invisible(lapply(o.patients, function(x) { lst.hr.min[[x]][, study.day:=as.integer(difftime(Time, dt.startDates[id==x, start.date], units='days'))]}))
mat.hr.min = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=min_na)
mat.hr.max = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=max_na)
mat.hr.mean = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=mean)
mat.hr.median = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=median_numeric)
mat.hr.sd = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=sd)

# plot heatmap
#col.sync = colorRamp2(c(1, 2, 5, 10, 100), brewer.pal(5, 'Greens'))

vec.split = cut(as.numeric(rownames(mat.hr.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_hr_1min.pdf'), width=10, height=8)
draw(Heatmap(mat.hr.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (min)', column_title='Heart rate (min)', col=col.hr, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (max)', column_title='Heart rate (max)', col=col.hr, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (mean)', column_title='Heart rate (mean)', col=col.hr, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (median)', column_title='Heart rate (median)', col=col.hr, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (sd)', column_title='Heart rate (sd)', col=col.hr.sd, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


## Steps in 1 minute intervals
# labels
lbl.steps.min = fread(file.path(dir.labels, 'minuteSteps.txt'))
o.patients = setNames(as.character(lbl.steps.min[,label]), lbl.steps.min[,label])

# load data
lst.steps.min = read.timeFile.lst(lbl.steps.min, col.time='ActivityMinute')
invisible(lapply(o.patients, function(x) { lst.steps.min[[x]][, study.day:=as.integer(difftime(ActivityMinute, dt.startDates[id==x, start.date], units='days'))]}))
mat.steps.min = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=min_na)
mat.steps.max = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=max_na)
mat.steps.mean = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=mean)
mat.steps.median = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=median_numeric)
mat.steps.sd = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=sd)

# plot heatmap
col.steps.min = colorRamp2(c(1, 5, 10, 20, 50, 100, 200), brewer.pal(7, 'Blues'))
col.steps.sd = colorRamp2(c(1, 10, 30), brewer.pal(3, 'Blues'))

vec.split = cut(as.numeric(rownames(mat.steps.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_minuteSteps.pdf'), width=10, height=8)
draw(Heatmap(mat.steps.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (min)', column_title='Steps (min)', col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (max)', column_title='Steps (max)', col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (mean)', column_title='Steps (mean)', col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (median)', column_title='Steps (median)', col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (sd)', column_title='Steps (sd)', col=col.steps.sd, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()

## Intensities in 1 minute intervals
# labels
lbl.intensity.min = fread(file.path(dir.labels, 'minuteIntensities.txt'))
o.patients = setNames(as.character(lbl.hr.min[,label]), lbl.hr.min[,label])

# load data
lst.intensity.min = read.timeFile.lst(lbl.intensity.min, col.time='ActivityMinute')
invisible(lapply(o.patients, function(x) { lst.intensity.min[[x]][, study.day:=as.integer(difftime(ActivityMinute, dt.startDates[id==x, start.date], units='days'))]}))
mat.intensity.min = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=min_na)
mat.intensity.max = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=max_na)
mat.intensity.mean = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=mean)
mat.intensity.median = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=median_numeric)
mat.intensity.sd = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=sd)

# plot heatmap
col.intensity = colorRamp2(c(0, 1, 2, 3, 5), brewer.pal(5, 'Reds'))

vec.split = cut(as.numeric(rownames(mat.intensity.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_intensity.pdf'), width=10, height=8)
draw(Heatmap(mat.intensity.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (min)', column_title='Intensity (min)', col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (max)', column_title='Intensity (max)', col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (mean)', column_title='Intensity (mean)', col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (median)', column_title='Intensity (median)', col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (sd)', column_title='Intensity (sd)', col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


## Calories in 1 minute intervals
# labels
lbl.calories.min = fread(file.path(dir.labels, 'minuteCalories.txt'))
o.patients = setNames(as.character(lbl.calories.min[,label]), lbl.calories.min[,label])

# load data
lst.calories.min = read.timeFile.lst(lbl.calories.min, col.time='ActivityMinute')
invisible(lapply(o.patients, function(x) { lst.calories.min[[x]][, study.day:=as.integer(difftime(ActivityMinute, dt.startDates[id==x, start.date], units='days'))]}))

mat.calories.min = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=min_na)
mat.calories.max = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=max_na)
mat.calories.mean = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=mean)
mat.calories.median = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=median_numeric)
mat.calories.sd = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=sd)
mat.calories.sum = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=sum)

# plot heatmap
col.calories.min = colorRamp2(c(0, 1, 2), brewer.pal(3, 'Purples'))
col.calories.day = colorRamp2(c(0, 1500, 3000), brewer.pal(3, 'Purples'))

vec.split = cut(as.numeric(rownames(mat.calories.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_calories.pdf'), width=10, height=8)
draw(Heatmap(mat.calories.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (min)', column_title='Calories (min)', col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (max)', column_title='Calories (max)', col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (mean)', column_title='Calories (mean)', col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (median)', column_title='Calories (median)', col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (sd)', column_title='Calories (sd)', col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.sum, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per day calories (sum)', column_title='Calories (sum)', col=col.calories.day, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


### Merge HR and steps by minute ###
vec.hr.lengths = sapply(lst.hr.min, nrow)
vec.steps.lengths = sapply(lst.steps.min, nrow)

vec.patients.merged = intersect(names(vec.hr.lengths[vec.hr.lengths>0]), names(vec.steps.lengths[vec.steps.lengths>0]))
names(vec.patients.merged) = vec.patients.merged

lst.hrSteps.min = lapply(vec.patients.merged, function(x) {merge(lst.hr.min[[x]][,list(Time, study.day, study.hour=hour(Time), HR=Value)], lst.steps.min[[x]][,list(Time=ActivityMinute, study.day, Steps)], by=c('Time', 'study.day')) } )

dt.hrSteps.min = rbindlist(lst.hrSteps.min, idcol='id')
dt.hrSteps.active = dt.hrSteps.min[Steps>0, list(active.hours=as.integer(.N/60)), by=c('id', 'study.day')]
dt.hrSteps.active[, ecog.num:=hoursToEcog.num(active.hours)]


pdf(file.path(dir.plots, 'hist_allSteps_minute.pdf'))
hist(dt.hrSteps.min[, Steps], main='Histogram of steps per minute', xlab='Steps (per minute)', breaks=1000, ylim=c(0,18000))
dev.off()


lst.p = lapply(vec.patients.merged, function(x) { plotScatter.plt(lst.hrSteps.min[[x]], col.x='Steps', col.y='HR', vec.xlim=c(0,220), vec.ylim=c(0,220), str.title=x, str.xlab='Steps (per minute)', str.ylab='HR (per minute)', p.theme=p.theme.tufte)} )

pdf(file.path(dir.plots, 'scatter_hrSteps_perMinute.pdf'))
print(lst.p)
dev.off()

lst.g = lapply(lst.p, ggplotGrob)

pdf(file.path(dir.plots, 'scatter_hrSteps_perMinute_multi.pdf'))
marrangeGrob(grobs=lst.p, nrow=3, ncol=3)
dev.off()



### Make a monolithic data table of daily summarized info ###


lst.ranges = list('0'=0:4, '1'=5:11, '2'=12:18, '3'=19:25, '4'=26:32)
vec.weekToDay = c('0'='0', '1'='7', '2'='14', '3'='21', '4'='28')

lst.tables = list()

for (i in names(lst.ranges)) {
  print(i)

  lst.data = list()

  lst.data[['tscore']] = dt.tscore[, list(id, tscore=get(vec.weekToDay[[i]]))]
  lst.data[['ecog']] = dt.ecog[, list(id, ecog=get(vec.weekToDay[[i]]))]

  lst.data[['sleep']] = aggregateList.dt(lst.sleep, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='MinutesAsleep', fn.lstAggregate=sum, str.variable='id', str.value='sleep', fn.rangeAggregate=median_numeric)
  lst.data[['METs.min']] = aggregateList.dt(lst.mets.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='METs', fn.lstAggregate=min, str.variable='id', str.value='METs.min', fn.rangeAggregate=median_numeric)
  lst.data[['METs.max']] = aggregateList.dt(lst.mets.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='METs', fn.lstAggregate=max, str.variable='id', str.value='METs.max', fn.rangeAggregate=median_numeric)
  lst.data[['METs.median']] = aggregateList.dt(lst.mets.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='METs', fn.lstAggregate=median_numeric, str.variable='id', str.value='METs.median', fn.rangeAggregate=median_numeric)
  lst.data[['METs.sd']] = aggregateList.dt(lst.mets.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='METs', fn.lstAggregate=sd, str.variable='id', str.value='METs.sd', fn.rangeAggregate=median_numeric)
  lst.data[['sync']] = aggregateList.dt(lst.sync, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='SyncDateUTC', fn.lstAggregate=length, str.variable='id', str.value='sync.count', fn.rangeAggregate=median_numeric)
  lst.data[['veryActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='VeryActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='VeryActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['fairlyActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='FairlyActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='FairlyActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['lightlyActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='LightlyActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='LightlyActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['sedentaryMinutes']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='SedentaryMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='SedentaryMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['bedMinutes']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='BedMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='BedMinutes', fn.rangeAggregate=median_numeric)

  lst.data[['speed.daily']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='speed.all', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.all', fn.rangeAggregate=median_numeric)
  lst.data[['speed.veryActive']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='speed.veryActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.veryActive', fn.rangeAggregate=median_numeric)
  lst.data[['speed.fairlyActive']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='speed.fairlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.fairlyActive', fn.rangeAggregate=median_numeric)
  lst.data[['speed.lightlyActive']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='speed.lightlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.lightlyActive', fn.rangeAggregate=median_numeric)

  lst.data[['ecog.pct']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='ecog.pct', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.pct', fn.rangeAggregate=median_numeric)
  lst.data[['ecog.pct.corrected']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='ecog.pct.corrected', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.pct.corrected', fn.rangeAggregate=median_numeric)

  lst.data[['hr.min']] = aggregateList.dt(lst.hr.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Value', fn.lstAggregate=min_na, str.variable='id', str.value='hr.min', fn.rangeAggregate=median_numeric)
  lst.data[['hr.max']] = aggregateList.dt(lst.hr.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Value', fn.lstAggregate=max_na, str.variable='id', str.value='hr.max', fn.rangeAggregate=median_numeric)
  lst.data[['hr.median']] = aggregateList.dt(lst.hr.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Value', fn.lstAggregate=median_numeric, str.variable='id', str.value='hr.median', fn.rangeAggregate=median_numeric)
  lst.data[['hr.sd']] = aggregateList.dt(lst.hr.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Value', fn.lstAggregate=sd, str.variable='id', str.value='hr.sd', fn.rangeAggregate=median_numeric)
  lst.data[['steps.min']] = aggregateList.dt(lst.steps.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Steps', fn.lstAggregate=min_na, str.variable='id', str.value='steps.min', fn.rangeAggregate=median_numeric)
  lst.data[['steps.max']] = aggregateList.dt(lst.steps.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Steps', fn.lstAggregate=max_na, str.variable='id', str.value='steps.max', fn.rangeAggregate=median_numeric)
  lst.data[['steps.median']] = aggregateList.dt(lst.steps.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Steps', fn.lstAggregate=median_numeric, str.variable='id', str.value='steps.median', fn.rangeAggregate=median_numeric)
  lst.data[['steps.sd']] = aggregateList.dt(lst.steps.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Steps', fn.lstAggregate=sd, str.variable='id', str.value='steps.sd', fn.rangeAggregate=median_numeric)
  lst.data[['steps.daily']] = aggregateList.dt(lst.steps.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Steps', fn.lstAggregate=sum, str.variable='id', str.value='steps.daily', fn.rangeAggregate=median_numeric)

  lst.data[['ecog.num']] = aggregateList.dt(lst.minutes, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='ecog.num', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.num', fn.rangeAggregate=median_numeric)

  dt = merge(lst.data[['ecog.num']], dt.hrSteps.min[study.day %in% lst.ranges[[i]], modelPairs.dt(.SD, 'study.day', 'Steps', 'HR'), by='id'], by='id', all.x=T)
  lst.data[['ecog.num.corrected']] = dt[, list(ecog.num.corrected=ceiling(correctEcog.num(ecog.num, angle))), by='id']

  lst.data[['intensity.min']] = aggregateList.dt(lst.intensity.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Intensity', fn.lstAggregate=min_na, str.variable='id', str.value='intensity.min', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.max']] = aggregateList.dt(lst.intensity.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Intensity', fn.lstAggregate=max_na, str.variable='id', str.value='intensity.max', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.median']] = aggregateList.dt(lst.intensity.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Intensity', fn.lstAggregate=median_numeric, str.variable='id', str.value='intensity.median', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.sd']] = aggregateList.dt(lst.intensity.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Intensity', fn.lstAggregate=sd, str.variable='id', str.value='intensity.sd', fn.rangeAggregate=median_numeric)
  lst.data[['calories.min']] = aggregateList.dt(lst.calories.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Calories', fn.lstAggregate=min_na, str.variable='id', str.value='calories.min', fn.rangeAggregate=median_numeric)
  lst.data[['calories.max']] = aggregateList.dt(lst.calories.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Calories', fn.lstAggregate=max_na, str.variable='id', str.value='calories.max', fn.rangeAggregate=median_numeric)
  lst.data[['calories.median']] = aggregateList.dt(lst.calories.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Calories', fn.lstAggregate=median_numeric, str.variable='id', str.value='calories.median', fn.rangeAggregate=median_numeric)
  lst.data[['calories.sd']] = aggregateList.dt(lst.calories.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Calories', fn.lstAggregate=sd, str.variable='id', str.value='calories.sd', fn.rangeAggregate=median_numeric)
  lst.data[['calories.daily']] = aggregateList.dt(lst.calories.min, col.id='study.day', vec.range=lst.ranges[[i]], col.measure='Calories', fn.lstAggregate=sum, str.variable='id', str.value='calories.daily', fn.rangeAggregate=median_numeric)

  lst.tables[[i]] = Reduce(function(x,y) {merge(x, y, by='id', all=T)}, lst.data)
}


# combine time periods into a monolithic table
dt.allData = rbindlist(lst.tables, idcol='study.week')

# save table for posterity
write.table(dt.allData, file.path(dir.root, 'tables/all_data.txt'), row.names=F, quote=F, sep='\t')



### Modeling ###

library(caret)
library(AppliedPredictiveModeling)
library(Rtsne)

# set up data
vec.outcomeVars = c('ecog', 'tscore')
vec.vars = setdiff(colnames(dt.allData), c('id', 'study.week', vec.outcomeVars))

vec.cols.numeric = setdiff(colnames(dt.allData), c('id', 'study.week'))

dt.allData = dt.allData[!is.na(steps.daily),]
dt.allData[, (vec.cols.numeric):=lapply(.SD, as.numeric), .SDcols=vec.cols.numeric]
dt.allData[, study.week:=factor(study.week)]
dt.allData[, id:=factor(id)]
dt.allData[, sync.count:=ifelse(is.na(sync.count), 0, sync.count)]

# example of manual test / training
num.split = round(nrow(dt.allData) * 0.8)
vec.random = sample(1:nrow(dt.allData))

dt.train = dt.allData[vec.random[1:num.split],]
dt.test = dt.allData[vec.random[(num.split+1):nrow(dt.allData)],]

model.tscore = lm(tscore ~ sleep + METs.max + VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes + BedMinutes + hr.median + steps.max + steps.daily + intensity.max + calories.max + calories.daily, dt.train)

p.tscore = predict(model.tscore, dt.test)
vec.error = p.tscore - dt.test[, tscore]

num.rmse = sqrt(mean(vec.error^2, na.rm=T))
summary(model.tscore)
# end example #

# using caret package
vec.vars.all = c('study.week', 'METs.median', 'METs.sd', 'VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes', 'SedentaryMinutes', 'BedMinutes', 'hr.median', 'hr.sd', 'hr.max', 'steps.daily', 'steps.max', 'steps.sd', 'intensity.max', 'intensity.sd', 'calories.daily', 'calories.max', 'calories.sd', 'sync.count')
formula.ecog.all = as.formula(paste('ecog', paste(vec.vars.all, collapse=' + '), sep='~'))
formula.tscore.all = as.formula(paste('tscore', paste(vec.vars.all, collapse=' + '), sep='~'))

dt.sub.ecog = dt.allData[!is.na(ecog),]
dt.sub.tscore = dt.allData[!is.na(tscore),]

obj.trControl = trainControl(method='repeatedcv', number=10, repeats=5, verboseIter=T)

#grid.glm = expand.grid(alpha=0:1, lambda=seq(0.0001, 0.1, length=10))

#model.tscore = train(formula.tscore, dt.sub, method='glmnet', tuneGrid=grid.glm, trControl=obj.trControl, preProcess=c('medianImpute'), na.action=na.pass)
#coef(model.tscore$finalModel, model.tscore$bestTune$lambda)

model.tscore = train(formula.tscore.all, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog.all, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)

# using custom ecog measurements to predict self reported ecog
formula.ecog.pct = as.formula(ecog ~ ecog.pct)
formula.ecog.num = as.formula(ecog ~ ecog.num)
formula.ecog.pct.corrected = as.formula(ecog ~ ecog.pct.corrected)
formula.ecog.num.corrected = as.formula(ecog ~ ecog.num.corrected)

model.ecog.pct = train(formula.ecog.pct, dt.sub.ecog, method='lm', trControl=obj.trControl, na.action=na.pass)
model.ecog.pct.corrected = train(formula.ecog.pct.corrected, dt.sub.ecog, method='lm', trControl=obj.trControl, na.action=na.pass)
model.ecog.num = train(formula.ecog.num, dt.sub.ecog, method='lm', trControl=obj.trControl, na.action=na.pass)
model.ecog.num.corrected = train(formula.ecog.num.corrected, dt.sub.ecog, method='lm', trControl=obj.trControl, na.action=na.pass)

# model subset
formula.tscore = as.formula(tscore ~ study.week + steps.daily + hr.median + SedentaryMinutes)
formula.ecog = as.formula(ecog ~ study.week + steps.daily + hr.median + SedentaryMinutes)

model.tscore = train(formula.tscore, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)

# model subset with HR max

formula.tscore = as.formula(tscore ~ study.week + steps.daily + hr.median + hr.max + SedentaryMinutes)
formula.ecog = as.formula(ecog ~ study.week + steps.daily + hr.median + hr.max + SedentaryMinutes)

model.tscore = train(formula.tscore, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)

# model subset with itensity.max

formula.tscore = as.formula(tscore ~ study.week + steps.daily + hr.median + intensity.max + intensity.sd + SedentaryMinutes)
formula.ecog = as.formula(ecog ~ study.week + steps.daily + hr.median + intensity.max + intensity.sd + SedentaryMinutes)

model.tscore = train(formula.tscore, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)

# model subset with METs

formula.tscore = as.formula(tscore ~ study.week + steps.daily + hr.median + METs.median + METs.sd + SedentaryMinutes)
formula.ecog = as.formula(ecog ~ study.week + steps.daily + hr.median + METs.median + METs.sd + SedentaryMinutes)

model.tscore = train(formula.tscore, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)

# model subset with itensity (makes steps.daily not significant)

formula.tscore = as.formula(tscore ~ study.week + steps.daily + hr.median + intensity.max + intensity.sd + SedentaryMinutes)
formula.ecog = as.formula(ecog ~ study.week + steps.daily + hr.median + intensity.max + intensity.sd + SedentaryMinutes)

model.tscore = train(formula.tscore, dt.sub.tscore, method='glm', trControl=obj.trControl, na.action=na.pass)
model.ecog = train(formula.ecog, dt.sub.ecog, method='glm', trControl=obj.trControl, na.action=na.pass)


## Plot whole data set
dt.allData[, id.week:=paste(id, study.week, sep='.')]
setkey(dt.allData, 'id.week')

# convert to matrix
mat.allData = as.matrix(sapply(dt.allData[, .SD, .SDcols = c(vec.vars)], as.numeric), rownames=T, rownames.value=dt.allData[, paste(id, study.week, sep='.')])
rownames(mat.allData) = dt.allData[, paste(id, study.week, sep='.')]

# remove vars with near zero variance
df.nzr = nearZeroVar(mat.allData)
mat.filtered = mat.allData[,-df.nzr]

# z-scores for plotting on the same heatmap
mat.scaled = t(scale(mat.filtered, center=T, scale=T))
o.data = dt.allData[id.week %in% colnames(mat.scaled),][order(id, study.week), id.week]

# plot heatmap
vec.colors.week = setNames(brewer.pal(5, 'PuBuGn'), as.character(0:4))
ha = HeatmapAnnotation(df=data.frame(study.week=dt.allData[o.data, as.character(study.week)], Tscore=dt.allData[o.data, tscore], ECOG=dt.allData[o.data, round(ecog)], ECOG.pct=dt.allData[o.data, round(ecog.pct.corrected)], ECOG.angle=dt.allData[o.data, round(ecog.num.corrected)]), col=list(study.week=vec.colors.week, Tscore=col.tscore.pf, ECOG=col.ecog, ECOG.pct=col.ecog, ECOG.angle=col.ecog), annotation_legend_param=list(study.week=list(title='Study week')), show_legend=c(T, T, T, F, F), annotation_name_side='left', border=T)

vec.split = dt.allData[o.data, id]

col.chm = colorRamp2(seq(-4,4,2), brewer.pal(5, 'Spectral'))
ht = Heatmap(mat.scaled[,o.data], cluster_columns=F, cluster_rows=F, top_annotation=ha, column_split=vec.split, show_column_names=F, row_names_side='left', name='Scaled data', column_title_rot=90, column_title_side='bottom', col=col.chm, border=T)

pdf(file.path(dir.plots, 'heatmap_all_scaled.pdf'), width=20, height=14)
draw(ht, column_title='Subject Id', column_title_side='bottom')
dev.off()

png(file.path(dir.plots, 'heatmap_all_scaled.png'), type='cairo', width=10, height=8, units='in', res=300)
draw(ht, column_title='Subject Id', column_title_side='bottom')
dev.off()


# Correlations

# corelations
mat.cor <-  cor(mat.filtered, use='complete.obs')
mat.highCorr <- sum(abs(mat.cor[upper.tri(mat.cor)]) > .999)

pdf(file.path(dir.plots, 'heatmap_corr.pdf'))
Heatmap(mat.cor, name='Correlation')
dev.off()

png(file.path(dir.plots, 'heatmap_corr.png'), type='cairo', width=7, height=6, units='in', res=300)
Heatmap(mat.cor, name='Correlation')
dev.off()


# plot pairwise relationships
pdf(file.path(dir.plots, 'pairs_allData_filtered.pdf'))
featurePlot(x = dt.allData[, .SD, .SDcols=colnames(mat.filtered)], y=dt.allData[, ecog], plot='pairs', auto.key=list(columns=3))
dev.off()

## steps
lst.p = list()
lst.p[['daily']] = ggplot(dt.allData, aes(steps.daily, tscore)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='Steps (daily total)', x='Steps', y='Tscore') + p.theme.tufte
lst.p[['min']] = ggplot(dt.allData, aes(steps.min, tscore)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='Steps (per minute minimum)', x='Steps', y='Tscore') + p.theme.tufte
lst.p[['max']] = ggplot(dt.allData, aes(steps.max, tscore)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='Steps (per minute maximum)', x='Steps', y='Tscore') + p.theme.tufte
lst.p[['median']] = ggplot(dt.allData, aes(steps.median, tscore)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='Steps (per minute median)', x='Steps', y='Tscore') + p.theme.tufte
lst.p[['sd']] = ggplot(dt.allData, aes(steps.sd, tscore)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='Steps (per minute sd)', x='Steps', y='Tscore') + p.theme.tufte

pdf(file.path(dir.plots, 'scatter_corrPairs_steps.pdf'))
print(lst.p)
dev.off()


pdf(NULL); lst.g = lapply(lst.p, ggplotGrob); dev.off()

pdf(file.path(dir.plots, 'scatter_corrPairs_steps_multi.pdf'), width=10)
marrangeGrob(lst.g, nrow=2, ncol=3, top='Steps variables')
dev.off()

## HR

lst.p = list()
lst.p[['min']] = ggplot(dt.allData, aes(tscore, hr.min)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='HR (per minute minimum)', x='HR', y='Tscore') + p.theme.tufte
lst.p[['max']] = ggplot(dt.allData, aes(tscore, hr.max)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='HR (per minute maximum)', x='HR', y='Tscore') + p.theme.tufte
lst.p[['median']] = ggplot(dt.allData, aes(tscore, hr.median)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='HR (per minute median)', x='HR', y='Tscore') + p.theme.tufte
lst.p[['sd']] = ggplot(dt.allData, aes(tscore, hr.sd)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='HR (per minute sd)', x='HR', y='Tscore') + p.theme.tufte

pdf(file.path(dir.plots, 'scatter_corrPairs_HR.pdf'))
print(lst.p)
dev.off()

pdf(NULL); lst.g = lapply(lst.p, ggplotGrob); dev.off()

pdf(file.path(dir.plots, 'scatter_corrPairs_hr_multi.pdf'), width=10)
marrangeGrob(lst.g, nrow=2, ncol=2, top='HR variables')
dev.off()

## Activity
lst.p = list()
lst.p[['very']] = ggplot(dt.allData, aes(tscore, VeryActiveMinutes)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='VeryActiveMinutes (daily total)', x='Activity (minutes)', y='Tscore') + p.theme.tufte
lst.p[['fairly']] = ggplot(dt.allData, aes(tscore, FairlyActiveMinutes)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='FarilyActiveMinutes (daily total)', x='Activity (minutes)', y='Tscore') + p.theme.tufte
lst.p[['light']] = ggplot(dt.allData, aes(tscore, LightlyActiveMinutes)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='LightlyActiveMinutes (daily total)', x='Activity (minutes)', y='Tscore') + p.theme.tufte
lst.p[['sedentary']] = ggplot(dt.allData, aes(tscore, SedentaryMinutes)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='SedentaryMinutes (daily total)', x='Activity (minutes)', y='Tscore') + p.theme.tufte
lst.p[['bed']] = ggplot(dt.allData, aes(tscore, BedMinutes)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='BedMinutes (daily total)', x='Activity (minutes)', y='Tscore') + p.theme.tufte

pdf(file.path(dir.plots, 'scatter_corrPairs_activity.pdf'))
print(lst.p)
dev.off()

pdf(NULL); lst.g = lapply(lst.p, ggplotGrob); dev.off()

pdf(file.path(dir.plots, 'scatter_corrPairs_activity_multi.pdf'), width=10)
marrangeGrob(lst.g, nrow=2, ncol=2, top='Activity variables')
dev.off()


## METs
lst.p = list()
lst.p[['min']] = ggplot(dt.allData, aes(tscore, METs.min)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='METs (min)', x='METs', y='Tscore') + p.theme.tufte
lst.p[['max']] = ggplot(dt.allData, aes(tscore, METs.max)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='METs (max)', x='METs', y='Tscore') + p.theme.tufte
lst.p[['median']] = ggplot(dt.allData, aes(tscore, METs.median)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='METs (median)', x='METs', y='Tscore') + p.theme.tufte
lst.p[['sd']] = ggplot(dt.allData, aes(tscore, METs.sd)) + geom_point(alpha=0.7) + geom_smooth(method='lm') + labs(title='METs (sd)', x='METs', y='Tscore') + p.theme.tufte

pdf(file.path(dir.plots, 'scatter_corrPairs_METs.pdf'))
print(lst.p)
dev.off()

pdf(NULL); lst.g = lapply(lst.p, ggplotGrob); dev.off()

pdf(file.path(dir.plots, 'scatter_corrPairs_METs_multi.pdf'), width=10)
marrangeGrob(lst.g, nrow=2, ncol=2, top='METs variables')
dev.off()


## Unsupervised analysis

# PCA
lbl.data = data.table(label=rownames(mat.filtered), group=dt.allData[, ifelse(is.na(ecog), 'NA', as.character(ecog))])
setkey(lbl.data, 'label')

pca.all = prcomp(na.omit(mat.filtered), center=T, scale.=F, retx=T, tol=NULL)
lst.p = plotPCA.lst(pca.all, lbl.data, str.title='PCA of all data', str.legend='ECOG', col.ecog, p.theme.tufte)

pdf(file.path(dir.plots, 'scatter_pca_filtered.pdf'))
print(lst.p)
dev.off()

# TSNE
tsne.all = Rtsne(na.omit(mat.allData), check_duplicates=F, perplexity=8)
lst.p = plotTsne.lst(tsne.all, rownames(na.omit(mat.allData)), lbl.data, str.title='TSNE of all data', col.ecog, p.theme.tufte, str.legend='ECOG')

pdf(file.path(dir.plots, 'scatter_tsne_filtered.pdf'))
print(lst.p)
dev.off()

# Dendrograms
lst.dend = getDend.lst(t(na.omit(mat.allData)), lbl.data, seq(50, 90, 10), 'euclidean', 'ward.D', col.ecog)

pdf(file.path(dir.plots, 'dend_allData_filtered.pdf'))
lapply(names(lst.dend), function(x) { plot(lst.dend[[x]], main=ifelse(x=='All', x, paste('SD cutoff:', x, '%')))})
dev.off()


### Distribution of steps / min

o.studyPeriod = c('baseline', '1', '2', '3', '4', '5+')
names(o.studyPeriod) = o.studyPeriod

vec.colors.studyPeriod = setNames(brewer.pal(6, 'Set1'), o.studyPeriod)

invisible(lapply(lst.steps.min, function(x) { x[, study.period:=cut(study.day, breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels=o.studyPeriod)]}))

# DD
# lst.steps.min = lapply(lst.steps.min, function(x) { x[, study.period:=cut(study.day, breaks=c(-50, 4, 11, 18, 25, 32, 1000), labels=o.studyPeriod)]})
vec.count = sapply(lst.steps.min, function(x) {nrow(x[Steps>0,])})


lst.p = lapply(names(vec.count[vec.count>0]), function(x) {ggplot(lst.steps.min[[x]][Steps>0,], aes(study.period, Steps, fill=study.period)) + geom_violin(draw_quantiles=c(0.25, 0.5, 0.75)) + scale_fill_manual(values=vec.colors.studyPeriod) + ylim(0,210) + labs(x='Study week', y='Steps / minute', title=x) + theme_minimal()} )


pdf(file.path(dir.plots, 'viol_stepsMin.pdf'))
print(lst.p)
dev.off()


ggplot(tmp, aes(study.period, Steps, fill=study.period)) + geom_violin() + theme_minimal()



## Get all 6 minute intervals

# use the shift function + sum in each period to get the number of steps walked in 6 minutes

o.patients.nz = names(vec.count[vec.count>0])
names(o.patients.nz) = o.patients.nz

invisible(
  lapply(o.studyPeriod, function(i.period) {
    
    lapply(lst.steps.min[o.patients.nz], function(x) {
      if (nrow(x[study.period==i.period,]) > 0) {
        x[study.period==i.period, sum.6:=rowSums(x[study.period==i.period,shift(Steps, n=1:6, fill=0, type='lead')])]
      }
    }) 
    
  })
)


# create histograms
lst.p = lapply(o.studyPeriod, function(i.period) {
    lapply(o.patients.nz, function(x) { 
      ggplot(lst.steps.min[[x]][study.period==i.period,], aes(sum.6)) + geom_histogram(binwidth=10, fill='gray80', color='black') + labs(x='Steps in 6 minutes', y='Frequency', title=paste('', x, ': ', i.period, sep='')) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + p.theme.tufte
    })
})

# plot by period
for (i in o.studyPeriod) {
  print(i)
  pdf(file.path(dir.plots, paste('hist_6minSteps_', i, '.pdf', sep='')))
  print(lst.p[i])
  dev.off()
}

lst.steps.min

tmp = copy(lst.steps.min)

invisible(lapply(o.patients.nz, function(x) { tmp[[x]][, patient:=x]}))

dt.steps.min = rbindlist(tmp[o.patients.nz])
dt.steps.min[, dist.6:=(sum.6 * vec.stepDist[patient])]

lst.steps.max = lapply(o.studyPeriod, function(x) { dt.steps.min[study.period==x, list(max.6mw=max(dist.6)), by='patient']})
  
lbl.pro.dd = lbl.pro
lbl.pro.dd[, dir := "~/mnt/Jacob/fitness/data/PRO & Other Data"]

dt.walkTest = fread(lbl.pro.dd[label=='walk.test', file.path(dir, file)])
vec.stepDist = sapply(lst.dailyActivity, function(x) {x[TotalSteps > 0, mean(1000 * TotalDistance / TotalSteps, na.rm=T)]})

vec.6mwDist = setNames(dt.walkTest[, SixMWDTotalMeters], dt.walkTest[,as.character(FolderLabel)])

pdf(file.path(dir.plots, 'hist_6mwTest.pdf'))
ggplot(dt.walkTest[!is.na(SixMWDTotalMeters),], aes(SixMWDTotalMeters)) + geom_histogram(binwidth=50, fill='gray80', color='black') + labs(x='Six minute walk test distance (m)', y='Frequency', title='Six minute walk test') + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + theme_minimal()
dev.off()


pdf(file.path(dir.plots, 'box_6minSteps.pdf'))
ggplot(dt.steps.min[study.period=='baseline',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Baseline') + p.theme.tufte
ggplot(dt.steps.min[study.period=='1',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Week 1') + p.theme.tufte
ggplot(dt.steps.min[study.period=='2',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Week 2') + p.theme.tufte
ggplot(dt.steps.min[study.period=='3',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Week 3') + p.theme.tufte
ggplot(dt.steps.min[study.period=='4',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Week 4') + p.theme.tufte
ggplot(dt.steps.min[study.period=='5+',], aes(patient, sum.6)) + geom_boxplot() + labs(x='Patient', y='Steps in 6 minutes', title='Week 5+') + p.theme.tufte
dev.off()

dt.walkTest[, FolderLabel:=as.character(FolderLabel)]

pdf(file.path(dir.plots, 'box_6minDistance.pdf'))
ggplot(dt.steps.min[study.period=='baseline',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['baseline']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Baseline') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
ggplot(dt.steps.min[study.period=='1',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['1']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 1') + p.theme.tufte.xaxis90
ggplot(dt.steps.min[study.period=='2',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['2']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 2') + p.theme.tufte.xaxis90
ggplot(dt.steps.min[study.period=='3',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['3']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 3') + p.theme.tufte.xaxis90
ggplot(dt.steps.min[study.period=='4',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['4']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 4') + p.theme.tufte.xaxis90
ggplot(dt.steps.min[study.period=='5+',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['baseline']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 5+') + p.theme.tufte.xaxis90
dev.off()


# plot as PNGs to save space
png(file.path(dir.plots, 'box_6minDistance_baseline.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='baseline',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['baseline']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Baseline') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()

png(file.path(dir.plots, 'box_6minDistance_week1.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='1',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['1']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 1') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()

png(file.path(dir.plots, 'box_6minDistance_week2.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='2',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['2']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 2') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()

png(file.path(dir.plots, 'box_6minDistance_week3.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='3',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['3']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 3') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()

png(file.path(dir.plots, 'box_6minDistance_week4.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='4',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['4']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 4') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()

png(file.path(dir.plots, 'box_6minDistance_week5+.png'), type='cairo', width=8, height=8, res=150, units='in')
ggplot(dt.steps.min[study.period=='5+',], aes(patient, dist.6)) + geom_jitter(alpha=0.3, size=0.2) + geom_point(data=dt.walkTest[!is.na(SixMWDTotalMeters),], aes(FolderLabel, SixMWDTotalMeters), color='red', alpha=0.5) + geom_errorbar(data=lst.steps.max[['baseline']], aes(y=NULL, ymin=max.6mw, ymax=max.6mw), color='blue') + labs(x='Patient', y='Distance in 6 minutes (m)', title='Week 5+') + scale_y_continuous(expand=c(0,0)) + p.theme.tufte.xaxis90
dev.off()



### Model performance status using exercise testing data

dt.cpet = fread(lbl.pro[label=='cpet', file.path(dir, file)])

vec.cols.cpet = c('CpetDuration', 'CpetVo2Peak', 'CpetMaxHeartRate', 'CpetMvv', 'CpetMvvPercent', 'CpetMinutesExerciesed', 'CpetWorkCapacity', 'WorkCapPercent', 'CpetOxConsump', 'OxConsumpPercent')
