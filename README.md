# Fitness
Fitness tracker data processing package.

## Usage notes
This R package was developed using FitBit device generated data stored in [Fitabase](https://fitabase.com/). It was used for the analysis of data in [Wearable sensor-based performance status assessment in cancer: A pilot multicenter study from the Alliance for Clinical Trials in Oncology (A19_Pilot2)](https://doi.org/10.1371/journal.pdig.0000178). Code used in the manuscript can be found [here](https://github.com/JLGlass/fitness_analysis_code).


## Example usage:
### Load libraries and set up directories

```R
library(data.table)
library(fitness)

dir.root = '[project home directory]'
dir.data.complete = file.path(dir.root, 'complete_data')
```

### Generate label files

```R
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
```

### Data processing

#### sleep

```R
# load labels
lbl.sleep = fread(file.path(dir.labels, 'sleepLogInfo.txt'))
o.patients = setNames(as.character(lbl.sleep[,label]), lbl.sleep[,label])

# load data
lst.sleep = read.timeFile.lst(lbl.sleep, col.time='StartTime')
invisible(lapply(o.patients, function(x) { lst.sleep[[x]][, study.day:=as.integer(difftime(StartTime, dt.startDates[id==x, start.date], units='days'))]}))
mat.sleep = lstToMatrix.mat(lst.sleep, str.id='study.day', str.measure='MinutesAsleep')
```

#### METs

```R
# load labels
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
```

#### Sync events

```R
# load labels
lbl.sync = fread(file.path(dir.labels, 'syncEvents.txt'))
o.patients = setNames(as.character(lbl.sync[,label]), lbl.sync[,label])

# load data
lst.sync = read.timeFile.lst(lbl.sync, col.time=c('DateTime', 'SyncDateUTC'))
invisible(lapply(o.patients, function(x) { lst.sync[[x]][, study.day:=as.integer(difftime(DateTime, dt.startDates[id==x, start.date], units='days'))]}))
mat.sync.count = lstToMatrix.mat(lst.sync, str.id='study.day', str.measure='study.day', fn.aggregate=length)
```


#### Daily activity log

```R
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
```

#### Heart rate in 1 minute intervals
```R
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
```

#### Steps in 1 minute intervals
```R
# load labels
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
```

#### Intensities in 1 minute intervals
```R
# load labels
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
```

#### Calories in 1 minute intervals
```R
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
```

### Data integration

#### merge time in bed with activity minutes for comprehensive time table

```R
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
```


#### Merge HR and steps by minute 

```R
vec.hr.lengths = sapply(lst.hr.min, nrow)
vec.steps.lengths = sapply(lst.steps.min, nrow)

vec.patients.merged = intersect(names(vec.hr.lengths[vec.hr.lengths>0]), names(vec.steps.lengths[vec.steps.lengths>0]))
names(vec.patients.merged) = vec.patients.merged

lst.hrSteps.min = lapply(vec.patients.merged, function(x) {merge(lst.hr.min[[x]][,list(Time, study.day, study.hour=hour(Time), HR=Value)], lst.steps.min[[x]][,list(Time=ActivityMinute, study.day, Steps)], by=c('Time', 'study.day')) } )

dt.hrSteps.min = rbindlist(lst.hrSteps.min, idcol='id')
dt.hrSteps.active = dt.hrSteps.min[Steps>0, list(active.hours=as.integer(.N/60)), by=c('id', 'study.day')]
dt.hrSteps.active[, ecog.num:=hoursToEcog.num(active.hours)]
```

#### Make a monolithic data table of daily summarized info
```R
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

```
