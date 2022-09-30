library(data.table)
library(fitness)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(caret)
library(ggpubr)

dir.root = '~/mnt/Jacob/fitness/data'
dir.labels = file.path(dir.root, 'labels')
dir.steps_hourly = file.path(dir.root, 'Hourly_Step_Data')
dir.hr_min = file.path(dir.root, 'Heart_Rate_Data')

dir.plots = file.path(dir.root, 'plots')
dir.surveys = file.path(dir.root, 'PRO & Other Data')

dir.new = file.path(dir.root, 'new_cohort')
dir.data.new = file.path(dir.new, 'Export-1-26-2020_2_41_pm')

dir.tables = file.path(dir.root, 'tables')

### run once to generate label files

lbl.sleep = createLabels.dt(dir.data.new, '*sleepLogInfo*')
write.table(lbl.sleep, file=file.path(dir.labels, 'sleepLogInfo_new.txt'), row.names=F, quote=F, sep='\t')

lbl.mets.min = createLabels.dt(dir.data.new, '*minuteMETsNarrow*')
write.table(lbl.mets.min, file=file.path(dir.labels, 'minuteMETs_new.txt'), row.names=F, quote=F, sep='\t')

lbl.sync = createLabels.dt(dir.data.new, '*syncEvents*')
write.table(lbl.sync, file=file.path(dir.labels, 'syncEvents_new.txt'), row.names=F, quote=F, sep='\t')

lbl.dailyActivity = createLabels.dt(dir.data.new, '*dailyActivity*')
write.table(lbl.dailyActivity, file=file.path(dir.labels, 'dailyActivity_new.txt'), row.names=F, quote=F, sep='\t')

lbl.hr.min = createLabels.dt(dir.data.new, '*heartrate_1min*')
write.table(lbl.hr.min, file=file.path(dir.labels, 'heartrate_1min_new.txt'), row.names=F, quote=F, sep='\t')

lbl.steps.min = createLabels.dt(dir.data.new, '*minuteStepsNarrow*')
write.table(lbl.steps.min, file=file.path(dir.labels, 'minuteSteps_new.txt'), row.names=F, quote=F, sep='\t')

lbl.intensity.min = createLabels.dt(dir.data.new, '*minuteIntensitiesNarrow*')
write.table(lbl.intensity.min, file=file.path(dir.labels, 'minuteIntensities_new.txt'), row.names=F, quote=F, sep='\t')

lbl.calories.min = createLabels.dt(dir.data.new, '*minuteCaloriesNarrow*')
write.table(lbl.calories.min, file=file.path(dir.labels, 'minuteCalories_new.txt'), row.names=F, quote=F, sep='\t')

### end run once

# physical function columns (has Raw.Score 99 duplicated for Raw.Score 100)
vec.cols.pf10a = paste('pf', 1:10, sep='_')

dt.promis.pf.10a = fread(file.path(dir.root, 'tables/promis_pfa_short10a.txt'))
dt.promis.pf.10a[, Raw.Score:=as.numeric(Raw.Score)]
setkey(dt.promis.pf.10a, 'Raw.Score')

date.origin = as.POSIXct('1970-01-01', tz='UTC')

dt.registry = fread(file.path(dir.new, 'Performance Registry Data.csv'))
dt.registry[, consent_date:=as.POSIXct(consent_date, format='%m/%d/%y', tz='UTC')]
dt.registry[, physical_function_timestamp:=as.POSIXct(physical_function_timestamp, format='%m/%d/%y %H:%M', tz='UTC')]
dt.registry[, Tscore:=dt.promis.pf.10a[Raw.Score==sum(6-.SD), T.Score], .SDcols=vec.cols.pf10a, by='record_id']
dt.registry[, start_date:=as.POSIXct(ifelse(is.na(physical_function_timestamp), consent_date, physical_function_timestamp), tz='UTC', origin=date.origin)]
setkey(dt.registry, 'record_id')


# set up column order based on Tscore
o.cols = dt.registry[as.character(record_id) %in% colnames(mat.pctActive),][order(Tscore, decreasing=T), as.character(record_id)]
vec.tscore = setNames(dt.registry[, Tscore], dt.registry[, as.character(record_id)])



# load questionaire dates / data
vec.cols.dates = c('User Start Date', 'Module Start Date', 'Module Completion Date')
dt.chart.dates = fread(file.path(dir.new, 'CHART Administration Dates.csv'))
dt.chart.dates[, (vec.cols.dates):=lapply(.SD, as.POSIXct, format='%m/%d/%y', tz='UTC'), .SDcols=vec.cols.dates]

dt.chart.data = fread(file.path(dir.new, 'Performance Registry CHART Data (2.5.20).csv'))

dt.chart.merged.all = merge(dt.chart.data, dt.chart.dates, by.x=c('User', 'Week (0=baseline)', 'Module'), by.y=c('User Name', 'Administration', 'Module'))
dt.chart.merged = dt.chart.merged.all[Module=='Physical Function',]
dt.chart.merged[, Answer:=as.numeric(Answer)]

dt.chart.tscore=dt.chart.merged[, list(Tscore=dt.promis.pf.10a[Raw.Score==sum(Answer, na.rm=T), T.Score]), by=c('User', 'Week (0=baseline)', 'Module Start Date')]
setnames(dt.chart.tscore, c('record_id', 'week', 'start_date', 'Tscore'))

dt.tscore = rbind(dt.registry[, list(record_id, week=-1, start_date, Tscore)], dt.chart.tscore)
dt.tscore = merge(dt.tscore, dt.registry[, list(record_id, study_start_date=start_date)], by='record_id', all.x=T)
dt.tscore[, study.day:=(1+as.integer(difftime(start_date, study_start_date, units='days')))]


#lst.dates = lapply(vec.patients.new, function(x) { lst = list(); lst[['baseline']] = dt.tscore[]} )

mat.tscore = as.matrix(dcast(dt.tscore, week ~ record_id, value.var='Tscore'), rownames='week')
rownames(mat.tscore) = c('Baseline', 0:14)

pdf(file.path(dir.plots, 'heatmap_tscore.pdf'))
Heatmap(mat.tscore[, o.cols], cluster_columns=F, cluster_rows=F, col=col.tscore, column_title='Weekly T Score measurements', row_names_side='left', name='T Score', heatmap_legend_param=list(color_bar='discrete'))
dev.off()

# load MD recorded ECOG / KPS data
dt.ps_MD = fread(file.path(dir.new, 'WoodPerformanceRegis_DATA_LABELS_2020-01-28_1122.csv'))
dt.ps_MD[, `Date of CPS`:=as.POSIXct(`Date of CPS`, format='%m/%d/%y', tz='UTC')]
setkey(dt.ps_MD, 'record_id')



# set up annotation for all the heatmaps
## colors for annotation
col.tscore = colorRamp2(seq(20, 100, 20), brewer.pal(5, 'YlGnBu'))
col.kps = colorRamp2(seq(100, 20, -20), brewer.pal(5, 'BuPu'))
col.ecog = colorRamp2(0:4, brewer.pal(5, 'PuBuGn'))

## annotation object
setkey(dt.compare, 'id')
ha = HeatmapAnnotation(df = data.frame(Tscore.0=vec.tscore[o.cols], Tscore.0.pred=dt.compare[study.week==0,][o.cols, round(Tscore.pred)], ECOG.MD=dt.compare[study.week==0,][o.cols, ECOG.MD], ECOG.pred=dt.compare[study.week==0,][o.cols, round(ECOG.pred)], KPS=dt.ps_MD[o.cols, KPS]), col=list(Tscore.0=col.tscore, Tscore.0.pred=col.tscore, ECOG.MD=col.ecog, ECOG.pred=col.ecog, KPS=col.kps), show_annotation_name=T, show_legend=c(T, F, T, F, T), annotation_legend_param=list(Tscore.0=list(title='T Score', color_bar='discrete'), ECOG.MD=list(title='ECOG', color_bar='discrete', at=2:0), KPS=list(color_bar='discrete')))


## sleep
# labels
lbl.sleep.all = fread(file.path(dir.labels, 'sleepLogInfo_new.txt'))
lbl.sleep = lbl.sleep.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.sleep[,label]), lbl.sleep[,label])

# load data
lst.sleep = addStudyTimes.lst(read.timeFile.lst(lbl.sleep, col.time='StartTime'), dt.tscore, 'StartTime')
mat.sleep = lstToMatrix.mat(lst.sleep, str.id='study.day', str.measure='MinutesAsleep')

# plot heatmap
col.sleep = colorRamp2(c(0, 60, 180, 300, 1000), brewer.pal(5, 'YlGnBu'))

vec.split = cut(as.numeric(rownames(mat.sleep)), seq(-30, 430, 30), labels=0:14)

pdf(file.path(dir.plots, 'heatmap_sleep_minutes_new3.pdf'), width=10, height=8)
draw(Heatmap(mat.sleep, name='Minutes asleep', cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray95', col=col.sleep, column_title='Cohort 2: Sleep in minutes per day', top_annotation=ha, split=vec.split), row_title='Study month')
dev.off()

## METs
# labels
lbl.mets.min.all = fread(file.path(dir.labels, 'minuteMETs_new.txt'))
lbl.mets.min = lbl.mets.min.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.mets.min[,label]), lbl.mets.min[,label])

# load data
lst.mets.min = addStudyTimes.lst(read.timeFile.lst(lbl.mets.min, col.time='ActivityMinute'), dt.tscore, 'ActivityMinute')

mat.mets.min = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=min)
mat.mets.max = lstToMatrix.mat(lst.mets.min, str.id='study.day', str.measure='METs', fn.aggregate=max)
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

pdf(file.path(dir.plots, 'heatmap_mets_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.mets.min, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (min)', column_title='Cohort 2: Activity in METs (daily minimum)', top_annotation=ha, col=col.mets.veryLow, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.max, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (max)', column_title='Cohort 2: Activity in METs (daily maximum)', top_annotation=ha, col=col.mets.mid, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.mean, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (mean)', column_title='Cohort 2: Activity in METs (daily mean)', top_annotation=ha, col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.median, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (median)', column_title='Cohort 2: Activity in METs (daily median)', top_annotation=ha, col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.sd, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (sd)', column_title='Cohort 2: Activity in METs (daily SD)', top_annotation=ha, col=col.mets.low, na_col='gray95', split=vec.split), row_title='Study month')
draw(Heatmap(mat.mets.sum, cluster_columns=F, cluster_rows=F, show_row_names=F, name='METs (daily sum)', column_title='Cohort 2: Activity in METs (daily sum)', top_annotation=ha, col=col.mets.high, na_col='gray95', split=vec.split), row_title='Study month')
dev.off()


## Sync events
# labels
lbl.sync.all = fread(file.path(dir.labels, 'syncEvents_new.txt'))
lbl.sync = lbl.sync.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.sync[,label]), lbl.sync[,label])

# load data
lst.sync = addStudyTimes.lst(read.timeFile.lst(lbl.sync, col.time=c('DateTime', 'SyncDateUTC')), dt.tscore, 'SyncDateUTC')

invisible(lapply(o.patients, function(x) { ifelse(nrow(lst.sync[[x]]) == 0, NA, lst.sync[[x]][, study.day:=1+(as.integer(difftime(DateTime, dt.registry[x, consent_date], units='days')))])}))
mat.sync.count = lstToMatrix.mat(lst.sync, str.id='study.day', str.measure='SyncDateUTC', fn.aggregate=length)

# plot heatmap
col.sync = colorRamp2(c(1, 2, 5, 10, 100), brewer.pal(5, 'YlGn'))

vec.split = cut(as.numeric(rownames(mat.sync.count)), seq(-30, 900, 30), labels=0:30)

pdf(file.path(dir.plots, 'heatmap_sync_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.sync.count, cluster_columns=F, cluster_rows=F, show_row_names=F, name='Count', column_title='Cohort 2: Sync frequency (per day)', top_annotation=ha, col=col.sync, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()

## Daily activity log
# labels
lbl.dailyActivity.all = fread(file.path(dir.labels, 'dailyActivity_new.txt'))
#lbl.dailyActivity = lbl.dailyActivity.all[label %in% dt.startDates[,id],]
lbl.dailyActivity = lbl.dailyActivity.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.dailyActivity[,label]), lbl.dailyActivity[,label])

# load data
cols.active = c('VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes')
cols.sedentary = c('SedentaryMinutes')
cols.activeMinutes = c('VeryActiveMinutes', 'FairlyActiveMinutes', 'LightlyActiveMinutes', 'SedentaryMinutes')
cols.allMinutes = c(cols.activeMinutes, 'BedMinutes')
cols.distance = c('TotalDistance', 'VeryActiveDistance', 'ModeratelyActiveDistance', 'LightActiveDistance')

lst.dailyActivity = addStudyTimes.lst(read.timeFile.lst(lbl.dailyActivity, col.time='ActivityDate', str.format='%m/%d/%Y'), dt.tscore, 'ActivityDate')

# merge time in bed with activity minutes for comprehensive time table

lst.minutes = Map(function(x,y) {merge(x[, .SD, .SDcols=c('study.day', 'study.week', cols.activeMinutes, cols.distance)], y, by='study.day', all.x=T)}, lst.dailyActivity[o.patients], lapply(lst.sleep[o.patients], function(x) { x[,list(BedMinutes=sum(TimeInBed, na.rm=T)), by='study.day']}))

invisible(lapply(lst.minutes, function(x) {x[, minutes.all:=sum(.SD, na.rm=T), .SDcols=cols.allMinutes, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, minutes.active:=sum(.SD, na.rm=T), .SDcols=cols.active, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.active:=sum(.SD, na.rm=T)/minutes.all, .SDcols=cols.active, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.veryActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='VeryActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.fairlyActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='FairlyActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.fairlyActive.cum:=sum(.SD, na.rm=T)/minutes.active, .SDcols=c('VeryActiveMinutes', 'FairlyActiveMinutes'), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.lightlyActive:=sum(.SD, na.rm=T)/minutes.active, .SDcols='LightlyActiveMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, pct.sedentary:=sum(.SD, na.rm=T)/minutes.all, .SDcols='SedentaryMinutes', by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.pct:=hoursToEcog.pct(pct.active), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.num:=hoursToEcog.num(minutes.active/60), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, ecog.pct.corrected:=correctEcog.pct(ecog.pct, pct.veryActive, pct.fairlyActive), by='study.day']}))

# convert km/min -> m/s 
invisible(lapply(lst.minutes, function(x) {x[, speed.all:=(1000 * TotalDistance / (60 * sum(.SD))), .SDcols=cols.activeMinutes, by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.veryActive:=(1000 * VeryActiveDistance / (60 * VeryActiveMinutes)), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.fairlyActive:=(1000 * ModeratelyActiveDistance / (60 * FairlyActiveMinutes)), by='study.day']}))
invisible(lapply(lst.minutes, function(x) {x[, speed.lightlyActive:=(1000 * LightActiveDistance / (60 * LightlyActiveMinutes)), by='study.day']}))

# 6min walk test equivalents
invisible(lapply(lst.minutes, function(x) {x[, sixMinWalk.all:=(6 * speed.all)]}))
invisible(lapply(lst.minutes, function(x) {x[, sixMinWalk.veryActive:=(6 * speed.veryActive)]}))
invisible(lapply(lst.minutes, function(x) {x[, sixMinWalk.fairlyActive:=(6 * speed.fairlyActive)]}))
invisible(lapply(lst.minutes, function(x) {x[, sixMinWalk.lightlyActive:=(6 * speed.lightlyActive)]}))




mat.pctActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.active', fn.aggregate=function(x) {mean(x)*100})
mat.pctVeryActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.veryActive', fn.aggregate=function(x) {mean(x)*100})
mat.pctFairlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.fairlyActive', fn.aggregate=function(x) {mean(x)*100})
mat.pctFairlyActive.cum = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.fairlyActive.cum', fn.aggregate=function(x) {mean(x)*100})
mat.pctLightlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.lightlyActive', fn.aggregate=function(x) {mean(x)*100})
mat.pctSedentary = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='pct.sedentary', fn.aggregate=function(x) {mean(x)*100})

mat.speedAll = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.all', fn.aggregate=function(x) {mean(x)})
mat.speedVeryActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.veryActive', fn.aggregate=function(x) {mean(x)})
mat.speedFairlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.fairlyActive', fn.aggregate=function(x) {mean(x)})
mat.speedLightlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='speed.lightlyActive', fn.aggregate=function(x) {mean(x)})

mat.sixMinWalkAll = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='sixMinWalk.all', fn.aggregate=function(x) {mean(x)})
mat.sixMinWalkVeryActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='sixMinWalk.veryActive', fn.aggregate=function(x) {mean(x)})
mat.sixMinWalkFairlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='sixMinWalk.fairlyActive', fn.aggregate=function(x) {mean(x)})
mat.sixMinWalkLightlyActive = lstToMatrix.mat(lst.minutes, str.id='study.day', str.measure='sixMinWalk.lightlyActive', fn.aggregate=function(x) {mean(x)})


# plot
col.pctActive = colorRamp2(c(0, 30, 49, 51, 70, 100), c(rev(brewer.pal(3, 'YlGnBu')), brewer.pal(3, 'YlOrRd')))

vec.split = cut(as.numeric(rownames(mat.pctActive)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_pctActive_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.pctActive[,o.cols], name='%', column_title='Cohort 2: Overall activity percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctSedentary[,o.cols], name='%', column_title='Cohort 2: Sedentary activity percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctVeryActive[,intersect(o.cols, colnames(mat.pctVeryActive))], name='%', column_title='Cohort 2: Very active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctFairlyActive[,intersect(o.cols, colnames(mat.pctFairlyActive))], name='%', column_title='Cohort 2: Fairly active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctFairlyActive.cum[,intersect(o.cols, colnames(mat.pctFairlyActive))], name='%', column_title='Cohort 2: Fairly active or better percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
draw(Heatmap(mat.pctLightlyActive[,intersect(o.cols, colnames(mat.pctLightlyActive))], name='%', column_title='Cohort 2: Lightly active percentage by time (minutes)', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.pctActive, split=vec.split), row_title='Study month')
dev.off()


col.speed = colorRamp2(c(0.8, 0.7, 0.4, 0.1, 0), brewer.pal(5, 'RdYlGn'))

pdf(file.path(dir.plots, 'heatmap_speed_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.speedAll[,o.cols], name='m/s', column_title='Cohort 2: Overall gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedVeryActive[,intersect(o.cols, colnames(mat.speedVeryActive))], name='m/s', column_title='Cohort 2: Very active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedFairlyActive[,intersect(o.cols, colnames(mat.speedFairlyActive))], name='m/s', column_title='Cohort 2: Fairly active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
draw(Heatmap(mat.speedLightlyActive[,intersect(o.cols, colnames(mat.speedLightlyActive))], name='m/s', column_title='Cohort 2: Lightly active gait speed', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.speed, split=vec.split), row_title='Study month')
dev.off()


col.sixMinWalk = colorRamp2(c(0, 5, 10, 100, 200), brewer.pal(5, 'RdBu'))

pdf(file.path(dir.plots, 'heatmap_sixMinWalk_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.sixMinWalkAll[,o.cols], name='m', column_title='Cohort 2: Overall 6 minute walk distance', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.sixMinWalk, split=vec.split), row_title='Study month')
draw(Heatmap(mat.sixMinWalkVeryActive[,intersect(o.cols, colnames(mat.speedVeryActive))], name='m', column_title='Cohort 2: Very active 6 minute walk distance', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.sixMinWalk, split=vec.split), row_title='Study month')
draw(Heatmap(mat.sixMinWalkFairlyActive[,intersect(o.cols, colnames(mat.speedFairlyActive))], name='m', column_title='Cohort 2: Fairly active 6 minute walk distance', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.sixMinWalk, split=vec.split), row_title='Study month')
draw(Heatmap(mat.sixMinWalkLightlyActive[,intersect(o.cols, colnames(mat.speedLightlyActive))], name='m', column_title='Cohort 2: Lightly active 6 minute walk distance', top_annotation=ha, cluster_columns=F, cluster_rows=F, show_row_names=F, na_col='gray90', col=col.sixMinWalk, split=vec.split), row_title='Study month')
dev.off()


## Heart rate in 1 minute intervals
# labels
lbl.hr.min.all = fread(file.path(dir.labels, 'heartrate_1min_new.txt'))
lbl.hr.min = lbl.hr.min.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.hr.min[,label]), lbl.hr.min[,label])

# load data
lst.hr.min = addStudyTimes.lst(read.timeFile.lst(lbl.hr.min, col.time='Time'), dt.tscore, 'Time')

mat.hr.min = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=min)
mat.hr.max = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=max)
mat.hr.mean = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=mean)
mat.hr.median = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=median_numeric)
mat.hr.sd = lstToMatrix.mat(lst.hr.min, str.id='study.day', str.measure='Value', fn.aggregate=sd)

# plot heatmap
col.hr.max = colorRamp2(c(160, 140, 120, 100, 50), brewer.pal(5, 'Spectral'))
col.hr.median = colorRamp2(c(120, 100, 80, 60, 40), brewer.pal(5, 'Spectral'))
col.hr.min = colorRamp2(c(100, 80, 60, 50, 40), brewer.pal(5, 'Spectral'))

col.hr.sd = colorRamp2(c(0, 10, 20), brewer.pal(3, 'OrRd'))

vec.split = cut(as.numeric(rownames(mat.hr.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_hr_1min_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.hr.min[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (min)', column_title='Cohort 2: Heart rate (min)', top_annotation=ha, col=col.hr.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.max[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (max)', column_title='Cohort 2: Heart rate (max)', top_annotation=ha, col=col.hr.max, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.mean[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (mean)', column_title='Cohort 2: Heart rate (mean)', top_annotation=ha, col=col.hr.median, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.median[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (median)', column_title='Cohort 2: Heart rate (median)', top_annotation=ha, col=col.hr.median, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.hr.sd[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='HR (sd)', column_title='Cohort 2: Heart rate (sd)', top_annotation=ha, col=col.hr.sd, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


## Steps in 1 minute intervals
# labels
lbl.steps.min.all = fread(file.path(dir.labels, 'minuteSteps_new.txt'))
lbl.steps.min = lbl.steps.min.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.steps.min[,label]), lbl.steps.min[,label])

# load data
lst.steps.min = addStudyTimes.lst(read.timeFile.lst(lbl.steps.min, col.time='ActivityMinute'), dt.tscore, 'ActivityMinute')

mat.steps.min = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=min)
mat.steps.max = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=max)
mat.steps.mean = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=mean)
mat.steps.median = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=median_numeric)
mat.steps.sd = lstToMatrix.mat(lst.steps.min, str.id='study.day', str.measure='Steps', fn.aggregate=sd)

# plot heatmap
col.steps.min = colorRamp2(c(1, 5, 10, 20, 50, 100, 200), brewer.pal(7, 'Blues'))
col.steps.sd = colorRamp2(c(1, 10, 30), brewer.pal(3, 'Blues'))

vec.split = cut(as.numeric(rownames(mat.steps.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_minuteSteps_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.steps.min[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (min)', column_title='Cohort 2: Steps (min)', top_annotation=ha, col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.max[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (max)', column_title='Cohort 2: Steps (max)', top_annotation=ha, col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.mean[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (mean)', column_title='Cohort 2: Steps (mean)', top_annotation=ha, col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.median[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (median)', column_title='Cohort 2: Steps (median)', top_annotation=ha, col=col.steps.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.steps.sd[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Steps (sd)', column_title='Cohort 2: Steps (sd)', top_annotation=ha, col=col.steps.sd, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()

## Intensities in 1 minute intervals
# labels
lbl.intensity.min.all = fread(file.path(dir.labels, 'minuteIntensities_new.txt'))
lbl.intensity.min = lbl.intensity.min.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.hr.min[,label]), lbl.hr.min[,label])

# load data
lst.intensity.min = addStudyTimes.lst(read.timeFile.lst(lbl.intensity.min, col.time='ActivityMinute'), dt.tscore, 'ActivityMinute')

mat.intensity.min = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=min)
mat.intensity.max = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=max)
mat.intensity.mean = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=mean)
mat.intensity.median = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=median_numeric)
mat.intensity.sd = lstToMatrix.mat(lst.intensity.min, str.id='study.day', str.measure='Intensity', fn.aggregate=sd)

# plot heatmap
col.intensity = colorRamp2(c(0, 1, 2, 3, 5), brewer.pal(5, 'Reds'))

vec.split = cut(as.numeric(rownames(mat.intensity.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_intensity_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.intensity.min[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (min)', column_title='Cohort 2: Intensity (min)', top_annotation=ha, col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.max[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (max)', column_title='Cohort 2: Intensity (max)', top_annotation=ha, col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.mean[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (mean)', column_title='Cohort 2: Intensity (mean)', top_annotation=ha, col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.median[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (median)', column_title='Cohort 2: Intensity (median)', top_annotation=ha, col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.intensity.sd[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Intensity (sd)', column_title='Cohort 2: Intensity (sd)', top_annotation=ha, col=col.intensity, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


## Calories in 1 minute intervals
# labels
lbl.calories.min.all = fread(file.path(dir.labels, 'minuteCalories_new.txt'))
lbl.calories.min = lbl.calories.min.all[!(label %like% 'Withdrawn' | label %like% 'Off Study'),]

o.patients = setNames(as.character(lbl.calories.min[,label]), lbl.calories.min[,label])

# load data
lst.calories.min = addStudyTimes.lst(read.timeFile.lst(lbl.calories.min, col.time='ActivityMinute'), dt.tscore, 'ActivityMinute')

mat.calories.min = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=min)
mat.calories.max = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=max)
mat.calories.mean = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=mean)
mat.calories.median = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=median_numeric)
mat.calories.sd = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=sd)
mat.calories.sum = lstToMatrix.mat(lst.calories.min, str.id='study.day', str.measure='Calories', fn.aggregate=sum)

# plot heatmap
col.calories.min = colorRamp2(c(0, 1, 2), brewer.pal(3, 'Purples'))
col.calories.day = colorRamp2(c(0, 1500, 3000), brewer.pal(3, 'Purples'))

vec.split = cut(as.numeric(rownames(mat.calories.min)), seq(-30, 240, 30), labels=0:8)

pdf(file.path(dir.plots, 'heatmap_calories_new2.pdf'), width=10, height=8)
draw(Heatmap(mat.calories.min[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (min)', column_title='Cohort 2: Calories (min)', top_annotation=ha, col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.max[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (max)', column_title='Cohort 2: Calories (max)', top_annotation=ha, col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.mean[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (mean)', column_title='Cohort 2: Calories (mean)', top_annotation=ha, col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.median[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (median)', column_title='Cohort 2: Calories (median)', top_annotation=ha, col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.sd[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per minute calories (sd)', column_title='Cohort 2: Calories (sd)', top_annotation=ha, col=col.calories.min, na_col='gray90', split=vec.split), row_title='Study month')
draw(Heatmap(mat.calories.sum[, o.cols], cluster_columns=F, cluster_rows=F, show_row_names=F, name='Per day calories (sum)', column_title='Cohort 2: Calories (sum)', top_annotation=ha, col=col.calories.day, na_col='gray90', split=vec.split), row_title='Study month')
dev.off()


### Merge HR and steps by minute ###
vec.hr.lengths = sapply(lst.hr.min, nrow)
vec.steps.lengths = sapply(lst.steps.min, nrow)

vec.patients.merged = intersect(names(vec.hr.lengths[vec.hr.lengths>0]), names(vec.steps.lengths[vec.steps.lengths>0]))
names(vec.patients.merged) = vec.patients.merged

lst.hrSteps.min = lapply(vec.patients.merged, function(x) {merge(lst.hr.min[[x]][,list(Time, study.week, study.day, study.hour=hour(Time), HR=Value)], lst.steps.min[[x]][,list(Time=ActivityMinute, study.week, study.day, Steps)], by=c('Time', 'study.week', 'study.day')) } )

dt.hrSteps.min = rbindlist(lst.hrSteps.min, idcol='id')
dt.hrSteps.active = dt.hrSteps.min[Steps>0, list(active.hours=as.integer(.N/60)), by=c('id', 'study.week', 'study.day')]
dt.hrSteps.active[, ecog.num:=hoursToEcog.num(active.hours)]


pdf(file.path(dir.plots, 'hist_allSteps_minute_new2.pdf'))
hist(dt.hrSteps.min[, Steps], main='Histogram of steps per minute', xlab='Steps (per minute)', breaks=1000, ylim=c(0,15000))
dev.off()

# do this for each time period




lst.p = lapply(vec.patients.merged, function(x) { plotScatter.plt(lst.hrSteps.min[[x]], col.x='Steps', col.y='HR', vec.xlim=c(0,220), vec.ylim=c(0,220), str.title=x, str.xlab='Steps (per minute)', str.ylab='HR (per minute)', p.theme=p.theme.tufte)} )

pdf(file.path(dir.plots, 'scatter_hrSteps_perMinute_new2.pdf'))
print(lst.p)
dev.off()

lst.g = lapply(lst.p, ggplotGrob)

pdf(file.path(dir.plots, 'scatter_hrSteps_perMinute_multi_new2.pdf'))
marrangeGrob(grobs=lst.p, nrow=3, ncol=3)
dev.off()


### Make a monolithic data table of daily summarized info ###


lst.ranges = list('0'=0:4, '1'=5:11, '2'=12:18, '3'=19:25, '4'=26:32)
vec.weekToDay = c('0'='0', '1'='7', '2'='14', '3'='21', '4'='28')

median_numeric <- function(x, ...) { 
  num = as.numeric(x)
  return(median(num, ...))
}

lst.ranges = list(`0`=1:200)

lst.tables = list()

vec.weeks = 0:max(dt.tscore[, week])

for (i in vec.weeks) {
  print(i)

  lst.data = list()

  lst.data[['tscore']] = dt.tscore[week==i, list(id=record_id, Tscore)]

  lst.data[['sleep']] = aggregateList.dt(lst.sleep, col.id='study.week', vec.range=c(i), col.measure='MinutesAsleep', fn.lstAggregate=sum, str.variable='id', str.value='sleep', fn.rangeAggregate=median_numeric)
  lst.data[['METs.min']] = aggregateList.dt(lst.mets.min, col.id='study.week', vec.range=c(i), col.measure='METs', fn.lstAggregate=min, str.variable='id', str.value='METs.min', fn.rangeAggregate=median_numeric)
  lst.data[['METs.max']] = aggregateList.dt(lst.mets.min, col.id='study.week', vec.range=c(i), col.measure='METs', fn.lstAggregate=max, str.variable='id', str.value='METs.max', fn.rangeAggregate=median_numeric)
  lst.data[['METs.median']] = aggregateList.dt(lst.mets.min, col.id='study.week', vec.range=c(i), col.measure='METs', fn.lstAggregate=median_numeric, str.variable='id', str.value='METs.median', fn.rangeAggregate=median_numeric)
  lst.data[['METs.sd']] = aggregateList.dt(lst.mets.min, col.id='study.week', vec.range=c(i), col.measure='METs', fn.lstAggregate=sd, str.variable='id', str.value='METs.sd', fn.rangeAggregate=median_numeric)
  lst.data[['sync']] = aggregateList.dt(lst.sync, col.id='study.week', vec.range=c(i), col.measure='SyncDateUTC', fn.lstAggregate=length, str.variable='id', str.value='sync.count', fn.rangeAggregate=median_numeric)
  lst.data[['veryActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='VeryActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='VeryActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['fairlyActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='FairlyActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='FairlyActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['lightlyActiveMinutes']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='LightlyActiveMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='LightlyActiveMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['sedentaryMinutes']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='SedentaryMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='SedentaryMinutes', fn.rangeAggregate=median_numeric)
  lst.data[['bedMinutes']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='BedMinutes', fn.lstAggregate=median_numeric, str.variable='id', str.value='BedMinutes', fn.rangeAggregate=median_numeric)

  lst.data[['speed.daily']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='speed.all', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.all', fn.rangeAggregate=median_numeric)
  lst.data[['speed.veryActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='speed.veryActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.veryActive', fn.rangeAggregate=median_numeric)
  lst.data[['speed.fairlyActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='speed.fairlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.fairlyActive', fn.rangeAggregate=median_numeric)
  lst.data[['speed.lightlyActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='speed.lightlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='speed.lightlyActive', fn.rangeAggregate=median_numeric)

  lst.data[['sixMinWalk.daily']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='sixMinWalk.all', fn.lstAggregate=median_numeric, str.variable='id', str.value='sixMinWalk.all', fn.rangeAggregate=median_numeric)
  lst.data[['sixMinWalk.veryActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='sixMinWalk.veryActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='sixMinWalk.veryActive', fn.rangeAggregate=median_numeric)
  lst.data[['sixMinWalk.fairlyActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='sixMinWalk.fairlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='sixMinWalk.fairlyActive', fn.rangeAggregate=median_numeric)
  lst.data[['sixMinWalk.lightlyActive']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='sixMinWalk.lightlyActive', fn.lstAggregate=median_numeric, str.variable='id', str.value='sixMinWalk.lightlyActive', fn.rangeAggregate=median_numeric)

  lst.data[['ecog.pct']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='ecog.pct', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.pct', fn.rangeAggregate=median_numeric)
  lst.data[['ecog.pct.corrected']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='ecog.pct.corrected', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.pct.corrected', fn.rangeAggregate=median_numeric)

  lst.data[['hr.min']] = aggregateList.dt(lst.hr.min, col.id='study.week', vec.range=c(i), col.measure='Value', fn.lstAggregate=min, str.variable='id', str.value='hr.min', fn.rangeAggregate=median_numeric)
  lst.data[['hr.max']] = aggregateList.dt(lst.hr.min, col.id='study.week', vec.range=c(i), col.measure='Value', fn.lstAggregate=max, str.variable='id', str.value='hr.max', fn.rangeAggregate=median_numeric)
  lst.data[['hr.median']] = aggregateList.dt(lst.hr.min, col.id='study.week', vec.range=c(i), col.measure='Value', fn.lstAggregate=median_numeric, str.variable='id', str.value='hr.median', fn.rangeAggregate=median_numeric)
  lst.data[['hr.sd']] = aggregateList.dt(lst.hr.min, col.id='study.week', vec.range=c(i), col.measure='Value', fn.lstAggregate=sd, str.variable='id', str.value='hr.sd', fn.rangeAggregate=median_numeric)
  lst.data[['steps.min']] = aggregateList.dt(lst.steps.min, col.id='study.week', vec.range=c(i), col.measure='Steps', fn.lstAggregate=min, str.variable='id', str.value='steps.min', fn.rangeAggregate=median_numeric)
  lst.data[['steps.max']] = aggregateList.dt(lst.steps.min, col.id='study.week', vec.range=c(i), col.measure='Steps', fn.lstAggregate=max, str.variable='id', str.value='steps.max', fn.rangeAggregate=median_numeric)
  lst.data[['steps.median']] = aggregateList.dt(lst.steps.min, col.id='study.week', vec.range=c(i), col.measure='Steps', fn.lstAggregate=median_numeric, str.variable='id', str.value='steps.median', fn.rangeAggregate=median_numeric)
  lst.data[['steps.sd']] = aggregateList.dt(lst.steps.min, col.id='study.week', vec.range=c(i), col.measure='Steps', fn.lstAggregate=sd, str.variable='id', str.value='steps.sd', fn.rangeAggregate=median_numeric)
  lst.data[['steps.daily']] = aggregateList.dt(lst.steps.min, col.id='study.week', vec.range=c(i), col.measure='Steps', fn.lstAggregate=sum, str.variable='id', str.value='steps.daily', fn.rangeAggregate=median_numeric)

  lst.data[['ecog.num']] = aggregateList.dt(lst.minutes, col.id='study.week', vec.range=c(i), col.measure='ecog.num', fn.lstAggregate=median_numeric, str.variable='id', str.value='ecog.num', fn.rangeAggregate=median_numeric)

  dt = merge(lst.data[['ecog.num']], dt.hrSteps.min[study.week==i, modelPairs.dt(.SD, 'study.day', 'Steps', 'HR'), by='id'], by='id', all.x=T)
  lst.data[['ecog.num.corrected']] = dt[, list(ecog.num.corrected=ceiling(correctEcog.num(ecog.num, angle))), by='id']

  lst.data[['intensity.min']] = aggregateList.dt(lst.intensity.min, col.id='study.week', vec.range=c(i), col.measure='Intensity', fn.lstAggregate=min, str.variable='id', str.value='intensity.min', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.max']] = aggregateList.dt(lst.intensity.min, col.id='study.week', vec.range=c(i), col.measure='Intensity', fn.lstAggregate=max, str.variable='id', str.value='intensity.max', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.median']] = aggregateList.dt(lst.intensity.min, col.id='study.week', vec.range=c(i), col.measure='Intensity', fn.lstAggregate=median_numeric, str.variable='id', str.value='intensity.median', fn.rangeAggregate=median_numeric)
  lst.data[['intensity.sd']] = aggregateList.dt(lst.intensity.min, col.id='study.week', vec.range=c(i), col.measure='Intensity', fn.lstAggregate=sd, str.variable='id', str.value='intensity.sd', fn.rangeAggregate=median_numeric)
  lst.data[['calories.min']] = aggregateList.dt(lst.calories.min, col.id='study.week', vec.range=c(i), col.measure='Calories', fn.lstAggregate=min, str.variable='id', str.value='calories.min', fn.rangeAggregate=median_numeric)
  lst.data[['calories.max']] = aggregateList.dt(lst.calories.min, col.id='study.week', vec.range=c(i), col.measure='Calories', fn.lstAggregate=max, str.variable='id', str.value='calories.max', fn.rangeAggregate=median_numeric)
  lst.data[['calories.median']] = aggregateList.dt(lst.calories.min, col.id='study.week', vec.range=c(i), col.measure='Calories', fn.lstAggregate=median_numeric, str.variable='id', str.value='calories.median', fn.rangeAggregate=median_numeric)
  lst.data[['calories.sd']] = aggregateList.dt(lst.calories.min, col.id='study.week', vec.range=c(i), col.measure='Calories', fn.lstAggregate=sd, str.variable='id', str.value='calories.sd', fn.rangeAggregate=median_numeric)
  lst.data[['calories.daily']] = aggregateList.dt(lst.calories.min, col.id='study.week', vec.range=c(i), col.measure='Calories', fn.lstAggregate=sum, str.variable='id', str.value='calories.daily', fn.rangeAggregate=median_numeric)

  lst.tables[[as.character(i)]] = Reduce(function(x,y) {merge(x, y, by='id', all=T)}, lst.data)
}

dt.allData.test = rbindlist(lst.tables, idcol='study.week')

# do once to write table to disk
write.table(dt.allData.test, file.path(dir.tables, 'new_data.txt'), row.names=F, quote=F, sep='\t')
# end run once

dt.test = dt.allData.test[!is.na(steps.daily) & !is.na(hr.median) & !is.na(SedentaryMinutes) & id %in% dt.registry[, record_id], ]
dt.test[, sqrt.steps.daily:=sqrt(steps.daily)]
dt.test[, log10.speed.all:=log10(speed.all+0.01)]
dt.test[, sqrt.SedentaryMinutes:=sqrt(SedentaryMinutes)]

## generate model on training data
dt.allData.train  = fread(file.path(dir.tables, 'all_data.txt'))
dt.train.ecog = dt.allData.train[!is.na(ecog) & !is.na(steps.daily) & !is.na(hr.median) & !is.na(SedentaryMinutes),]
dt.train.tscore = dt.allData.train[!is.na(tscore),]

dt.train.ecog[, sqrt.steps.daily:=sqrt(steps.daily)]
dt.train.ecog[, log10.speed.all:=log10(speed.all+0.01)]
dt.train.ecog[, sqrt.SedentaryMinutes:=sqrt(SedentaryMinutes)]
dt.train.tscore[, sqrt.steps.daily:=sqrt(steps.daily+0.01)]
dt.train.tscore[, log10.speed.all:=log10(speed.all+0.01)]
dt.train.tscore[, sqrt.SedentaryMinutes:=sqrt(SedentaryMinutes)]

obj.trControl = trainControl(method='repeatedcv', number=10, repeats=5, verboseIter=F)

formula.ecog = as.formula(ecog ~ hr.median + SedentaryMinutes + log10.speed.all + METs.sd)
model.ecog = train(formula.ecog, dt.train.ecog, method='lm', trControl=obj.trControl, na.action=na.pass)

formula.tscore = as.formula(tscore ~ hr.median + SedentaryMinutes + log10.speed.all + METs.sd)
model.tscore = train(formula.tscore, dt.train.tscore, method='lm', trControl=obj.trControl, na.action=na.pass)




vec.ecog.pred = setNames(predict(model.ecog, newdata=dt.test), dt.test[, id])
vec.tscore.pred = setNames(predict(model.tscore, newdata=dt.test), dt.test[, id])


vec.patients.new = setNames(dt.registry[, record_id], dt.registry[, record_id])
setkey(dt.registry, 'record_id')

vec.compVars=c('id', 'METs.median', 'METs.sd', 'study.week', 'steps.daily', 'hr.median', 'SedentaryMinutes', 'sqrt.SedentaryMinutes', 'speed.veryActive', 'speed.fairlyActive', 'speed.lightlyActive', 'speed.all', 'log10.speed.all', 'Tscore')
dt.compare = data.table(dt.test[,.SD, .SDcols=vec.compVars], ECOG.MD=dt.ps_MD[dt.test[,id], ECOG], ECOG.pred=predict(model.ecog, newdata=dt.test), Tscore.pred=predict(model.tscore, newdata=dt.test))
dt.compare[, ECOG.resid:=(ECOG.pred-ECOG.MD)]
dt.compare[, Tscore.resid:=(Tscore.pred-Tscore)]

formula = y ~ x

pdf(file.path(dir.plots, 'scatter_compare.pdf'))
#ggplot(dt.compare, aes(Tscore, Tscore.pred)) + geom_point() + geom_smooth(method='lm') + xlim(0,100) + ylim(0,100) + p.theme.tufte
#ggscatter(dt.compare, x='Tscore', y='Tscore.pred', add='reg.line') + stat_cor(label.x=45, label.y=48) + stat_regline_equation(label.x=45, label.y=46)
ggplot(dt.compare, aes(Tscore.pred, Tscore)) + geom_point() + stat_smooth(method='lm', formula = formula) + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + p.theme.tufte
ggplot(dt.compare, aes(ECOG.pred, ECOG.MD)) + geom_point() + stat_smooth(method='lm', formula = formula) + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + p.theme.tufte
ggplot(dt.compare, aes(ECOG.pred, ECOG.resid)) + geom_point() + geom_abline(slope=0) + ggtitle('ECOG: Residual vs. fitted') + p.theme.tufte
ggplot(dt.compare, aes(Tscore.pred, Tscore.resid)) + geom_point() + geom_abline(slope=0) + ggtitle('Tscore: Residual vs. fitted') + p.theme.tufte
qqplot(dt.compare[, Tscore.pred], dt.compare[, Tscore.resid], main='QQ plot of Tscore predicted vs. residuals', xlab='Tscore predicted', ylab='Residual')
qqplot(dt.compare[, ECOG.pred], dt.compare[, ECOG.resid], main='QQ plot of ECOG predicted vs. residuals', xlab='ECOG predicted', ylab='Residual')
ggplot(dt.compare, aes(ECOG.MD, ECOG.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of ECOG', x='ECOG (MD measured)', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(Tscore, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='Tscore (patient reported; 10Q)', y='Residual') + p.theme.tufte
#ggplot(dt.compare, aes(steps.daily, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='Daily steps', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(hr.median, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='HR (median)', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(SedentaryMinutes, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='Sedentary minutes', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(sqrt.SedentaryMinutes, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='sqrt(sedentary minutes)', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(speed.all, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='Gait speed overall (m/s)', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(log10.speed.all, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='log10(gait speed overall) (m/s)', y='Residual') + p.theme.tufte
ggplot(dt.compare, aes(METs.sd, Tscore.resid)) + geom_point() + geom_abline(slope=0) + labs(title='Residual plot of Tscore', x='METs (SD)', y='Residual') + p.theme.tufte
dev.off()



pdf(file.path(dir.plots, 'hist_residuals.pdf'))
hist(dt.compare[, ECOG.resid], main='ECOG residuals', xlab='ECOG residuals')
hist(dt.compare[, Tscore.resid], main='Tscore residuals', xlab='Tscore residuals')
dev.off()

pdf(file.path(dir.plots, 'hist_variables.pdf'))
hist(dt.compare[, METs.sd], main='Histogram of METs (SD)', xlab='SD of METs')
hist(dt.compare[, steps.daily], main='Histogram of daily steps', xlab='Daily steps')
hist(dt.compare[, sqrt(steps.daily)], main='Histogram of daily steps', xlab='sqrt(daily steps)')
hist(dt.compare[, log10(steps.daily + 0.01)], main='Histogram of log(daily steps)', xlab='log(Daily steps)')
hist(dt.compare[, hr.median], main='Histogram of HR (median)', xlab='HR (median)')
hist(dt.compare[, SedentaryMinutes], main='Histogram of sedentary minutes', xlab='Sedentary minutes')
hist(dt.compare[, sqrt(SedentaryMinutes)], main='Histogram of sedentary minutes', xlab='sqrt(Sedentary minutes)')
hist(dt.compare[, speed.all], main='Histogram of gait speed overall', xlab='Gait speed (daily; m/s)')
hist(dt.compare[, log10(speed.all+0.01)], main='Histogram of gait speed overall', xlab='log10(Gait speed) (daily; m/s)')
hist(dt.compare[, speed.veryActive], main='Histogram of very active minutes', xlab='Gait speed (very active; m/s)')
hist(dt.compare[, speed.fairlyActive], main='Histogram of fairly active minutes', xlab='Gait speed (fairly active; m/s)')
hist(dt.compare[, speed.lightlyActive], main='Histogram of lightly active minutes', xlab='Gait speed (lightly active; m/s)')
dev.off()


write.table(dt.compare, file.path(dir.tables, 'model_vs_actual.txt'), row.names=F, quote=F, sep='\t')


###

pdf(file.path(dir.plots, 'scatter_lenghtOfStay_new.pdf'))
ggplot(dt.registry, aes(walktest_8, `Hospital Stay Length (days)`)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Walk test vs. length of stay', x='6 minute walk distance') + p.theme.tufte
ggplot(dt.registry, aes(sit2stand_1, `Hospital Stay Length (days)`)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Sit to stand test vs. length of stay', x='time (5 sit to stands)') + p.theme.tufte
ggplot(dt.registry, aes(sit2stand_2, `Hospital Stay Length (days)`)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Sit to stand test vs. length of stay', x='Number of sit to stands') + p.theme.tufte
ggplot(dt.registry, aes(Tscore, `Hospital Stay Length (days)`)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Tscore vs. length of stay') + p.theme.tufte
ggplot(dt.registry, aes(Age, `Hospital Stay Length (days)`)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Age vs. length of stay') + p.theme.tufte
dev.off()



# compare physical assessments

lbl.pro.train = fread(file.path(dir.labels, 'pro_data.txt'))
vec.pro.train = lbl.pro.train[, label]

lst.pro.train = readTables.lst(lbl.pro.train)

# physical function columns (has Raw.Score 99 duplicated for Raw.Score 100)
dt.promis.pf = fread(file.path(dir.root, 'tables/promis_pfa.txt'))
dt.promis.pf[, Raw.Score:=as.numeric(Raw.Score)]
setkey(dt.promis.pf, 'Raw.Score')

cols.pf = grep('PF', colnames(lst.pro[['baseline']]), value=T)

# assessments with PFA
vec.pro.pfa = c('baseline', 'followup', 'followup.sat')

lst.tscore.train = lapply(lst.pro[vec.pro.pfa.train], function(x) { merge(x[,list(id=as.character(FolderLabel), FormStartDay, Raw.Score=rowSums(.SD)), .SDcols=cols.pf], dt.promis.pf, by='Raw.Score', all.x=T)[, list(T.Score=max(T.Score)), by=c('id', 'FormStartDay')]})
lst.ecog.train = lapply(lst.pro[vec.pro.pfa.train], function(x) { x[,list(id=as.character(FolderLabel), FormStartDay, ECOG)]})

# convert to data tables of id vs. assessment day
dt.tscore.train = as.data.table(dcast(do.call('rbind', lst.tscore), id ~ FormStartDay, value.var='T.Score', fun.aggregate=mean))
dt.ecog.train = as.data.table(dcast(do.call('rbind', lst.ecog), id ~ FormStartDay, value.var='ECOG', fun.aggregate=mean))


# create an averaged version of dt.allData.train for use in VO2 max model
vec.cols.all = setdiff(colnames(dt.allData.train), c('id', 'study.week'))
dt.allData.train.mean = dt.allData.train[, lapply(.SD, mean, na.rm=T), by=c('id', 'study.week')]

dt.registry.train = merge(lst.pro[['walk.test']][, list(id=as.character(FolderLabel), SixMWDTotalMeters, SixMWDEndingHR)], lst.pro[['cpet']][, list(id=as.character(FolderLabel), CpetVo2Peak=as.numeric(CpetVo2Peak))], by='id')
dt.registry.train = merge(dt.registry.train, dt.tscore.train[, list(id=as.character(id), Tscore.0=`0`, Tscore.7=`7`, Tscore.14=`14`, Tscore.21=`21`, Tscore.28=`28`)], by='id')
dt.registry.train = merge(dt.registry.train, dt.allData.train.mean, by='id')


dt.registry.train.sub = dt.registry.train[!is.na(CpetVo2Peak),]
dt.registry.train.sub[, log10.speed.all:=log10(speed.all+0.01)]

model.vo2 = train(CpetVo2Peak ~ hr.median + SedentaryMinutes + log10.speed.all + METs.sd, dt.registry.train.sub, method='lm', trControl=obj.trControl, na.action=na.pass)
model.vo2 = train(CpetVo2Peak ~ Tscore.0, dt.registry.train.sub, method='lm', trControl=obj.trControl, na.action=na.pass)
model.vo2 = train(CpetVo2Peak ~ hr.median, dt.registry.train.sub, method='lm', trControl=obj.trControl, na.action=na.pass)


pdf(file.path(dir.plots, 'scatter_exercise.pdf'))
ggplot(dt.registry, aes(walktest_8, Tscore)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 2: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry, aes(sit2stand_1, Tscore)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 2: Tscore vs. sit to stand test',  x='time (5 sit to stands)') + p.theme.tufte
ggplot(dt.registry, aes(sit2stand_2, Tscore)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 2: Tscore vs. sit to stand test',  x='Number of sit to stands') + p.theme.tufte

ggplot(dt.registry.train, aes(SixMWDTotalMeters, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDTotalMeters, Tscore.7)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDTotalMeters, Tscore.14)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDTotalMeters, Tscore.21)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDTotalMeters, Tscore.28)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDEndingHR, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='Ending HR') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDEndingHR, Tscore.7)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='Ending HR') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDEndingHR, Tscore.14)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='Ending HR') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDEndingHR, Tscore.21)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='Ending HR') + p.theme.tufte
ggplot(dt.registry.train, aes(SixMWDEndingHR, Tscore.28)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. walk test', x='Ending HR') + p.theme.tufte
ggplot(dt.registry.train, aes(CpetVo2Peak, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. CPET VO2 peak', x='CPET VO2 Peak') + p.theme.tufte
ggplot(dt.registry.train, aes(CpetVo2Peak, Tscore.7)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. CPET VO2 peak', x='CPET VO2 Peak') + p.theme.tufte
ggplot(dt.registry.train, aes(CpetVo2Peak, Tscore.14)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. CPET VO2 peak', x='CPET VO2 Peak') + p.theme.tufte
ggplot(dt.registry.train, aes(CpetVo2Peak, Tscore.21)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. CPET VO2 peak', x='CPET VO2 Peak') + p.theme.tufte
ggplot(dt.registry.train, aes(CpetVo2Peak, Tscore.28)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. CPET VO2 peak', x='CPET VO2 Peak') + p.theme.tufte
dev.off()

dt.registry.train[, sixMW.light:=(6000 * speed.lightlyActive)]
dt.registry.train[, sixMW.fairly:=(6000 * speed.fairlyActive)]
dt.registry.train[, sixMW.very:=(6000 * speed.veryActive)]
dt.registry.train[, sixMW.all:=(6000 * speed.all)]

pdf(file.path(dir.plots, 'scatter_6mw_comparison.pdf'))
ggplot(dt.registry.train[study.week==0,], aes(sixMW.all, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (baseline)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.light, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (baseline)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.fairly, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (baseline)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.very, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (baseline)', y='6 minute walk distance (m)') + p.theme.tufte

ggplot(dt.registry.train[study.week==1,], aes(sixMW.all, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (week 1)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==1,], aes(sixMW.light, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (week 1)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==1,], aes(sixMW.fairly, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (week 1)', y='6 minute walk distance (m)') + p.theme.tufte
ggplot(dt.registry.train[study.week==1,], aes(sixMW.very, SixMWDTotalMeters)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Walk test actual vs. fitbit (week 1)', y='6 minute walk distance (m)') + p.theme.tufte

ggplot(dt.registry.train[study.week==0,], aes(sixMW.all, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. fitbit 6MW (week 0)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.light, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. fitbit 6MW (week 0)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.fairly, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. fitbit 6MW (week 0)') + p.theme.tufte
ggplot(dt.registry.train[study.week==0,], aes(sixMW.very, Tscore.0)) + geom_point() + stat_smooth(method='lm') + stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep='~~~~')), formula=formula) + labs(title='Cohort 1: Tscore vs. fitbit 6MW (week 0)') + p.theme.tufte

dev.off()

dt.registry.test = merge(dt.registry, dt.test, by='record_id')