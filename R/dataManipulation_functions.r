#' @import data.table
NULL


#' Filter dates
#'
#' Function to filter a data table around a specific date
#'
#' @param dt data.table with a date column
#' @param date.start date to filter around
#' @param days.up days prior to date.start to include.
#' @param days.down days after to date.start to include.
#' @param col.date name of the date column in dt.
#' @param col.winDate name of the column to create with the relative number of days
#' @param str.format date string format to use for any non-date formatted columns.
#'
#' @return data.table subset with the dates of interest and an added column of days relative to date.start
#'
#' @export

filterDates.dt <- function(dt, date.start, days.up=7, days.down=7, col.date='date', col.winDate='day.window', str.format='%m-%d-%Y') {
  d.mid = as.POSIXct(date.start, format=str.format)

  if (is.na(days.up)) {
    d.end = d.mid + as.difftime(days.down, units='days')
    dt.sub = dt[as.POSIXct(get(col.date), format=str.format)<=d.end,]
  }
  else if (is.na(days.down)) {
    d.start = d.mid - as.difftime(days.up, units='days')
    dt.sub = dt[as.POSIXct(get(col.date), format=str.format)>=d.start,]
  }
  else {
    d.start = d.mid - as.difftime(days.up, units='days')
    d.end = d.mid + as.difftime(days.down, units='days')
    dt.sub = dt[as.POSIXct(get(col.date), format=str.format)>=d.start & as.POSIXct(get(col.date), format=str.format)<=d.end,]
  }

  dt.sub[, (col.winDate):=as.integer(difftime(as.POSIXct(get(col.date), format=str.format), d.mid, units='days'))]

  return(dt.sub)
}

#' Read time file
#'
#' Reads a file with date/time data using information in a label table
#'
#' @param lbl sample key table with directory, file name, and label columns
#' @param col.time column name or vector of column names with date/time data.
#' @param str.format incoming date format or vector of date formats corresponding to each element of the col.time vector. Default format: (1/22/19 12:33:45 PM).
#' @param str.tz time zone for date conversion. Defaults to UTC to avoid double counting issues.
#' @param col.dir column name in lbl with directory.
#' @param col.file column name in lbl with file.
#' @param col.label column name in lbl with label.
#'
#' @return data.table with time columns in posix format
#'
#' @export

read.timeFile.lst <- function(lbl, col.time='StartTime', str.format='%m/%d/%Y %I:%M:%S %p', str.tz='UTC', col.dir='dir', col.file='file', col.label='label') {
    lst = list()

    for (i in 1:nrow(lbl)) {
        i.label = as.character(lbl[i, get(col.label)])
        i.file = lbl[i, normalizePath(file.path(get(col.dir), get(col.file)))]
        dt = fread(i.file)

        # handle cases where a vector of columns and a single or vector of formats is given
        if (length(col.time) > 1) {
          if (length(str.format) > 1) {
            dt[,(col.time):=Map(as.POSIXct, .SD, format=str.format, tz=str.tz), .SDcols=col.time]
          }
          else {
            dt[,(col.time):=lapply(.SD, as.POSIXct, format=str.format, tz=str.tz), .SDcols=col.time]
          }
        }
        else {
          dt[,(col.time):=as.POSIXct(get(col.time), format=str.format, tz=str.tz)]
        }

        lst[[i.label]] = dt
    }

    return(lst)
}


#' Function to add study week and day columns (integers) to a data.table
#'
#' @param dt data.table
#' @param str.id subject id to analyze in date table
#' @param dt.dates data.table with study date ranges including study start date and weeks of interest
#' @param col.time column with time in dt (POSIX format expected)
#' @param col.datesId column with subject ids in dt.dates
#' @param col.datesStudyStartDate column with study start date in dt.dates
#' @param col.datesStudyWeek column with week numbers in dt.dates
#' @param col.datesStudyWeekDate column with week dates in dt.dates
#' @param col.study.day column to add to dt with study days
#' @param col.study.week column to add to dt with study weeks
#' @param num.week.min minimum week value to use (set very low by default to include all data)
#' @param num.week.max maximum week value to use (set very high by default to include all data)
#'
#' @return data.table with dt + col.study.day and col.study.week
#'
#' @export
#'

addStudyTimes.dt <- function(dt, str.id, dt.dates, col.time='StartTime', col.datesId='record_id', col.datesStudyStartDate='study_start_date', col.datesStudyWeek='week', col.datesStudyWeekDate='start_date', col.study.day='study.day', col.study.week='study.week', num.week.min=-1000, num.week.max=1000) {
  # set study day to the difference in days between study start and current time
  dt[, (col.study.day):=(1+as.integer(difftime(get(col.time), dt.dates[get(col.datesId)==str.id, unique(get(col.datesStudyStartDate))], units='days')))]

  # set study week to the period of time between the previous and current week start
  dt[, (col.study.week):=cut(get(col.study.day), breaks=c(num.week.min,dt.dates[get(col.datesId)==str.id & get(col.datesStudyWeek) >=0, get(col.study.day)], num.week.max), labels=0:nrow(dt.dates[get(col.datesId)==str.id & get(col.datesStudyWeek)>=0,]))]

  return(dt)
}

#' Function to add study week and day columns (integers) to a list of data.tables
#'
#' @param dt data.table
#' @param dt.dates data.table with study date ranges including study start date and weeks of interest
#' @param col.time column with time in dt (POSIX format expected)
#' @param col.datesId column with subject ids in dt.dates
#' @param col.datesStudyStartDate column with study start date in dt.dates
#' @param col.datesStudyWeek column with week numbers in dt.dates
#' @param col.datesStudyWeekDate column with week dates in dt.dates
#' @param col.study.day column to add to dt with study days
#' @param col.study.week column to add to dt with study weeks
#' @param num.week.min minimum week value to use (set very low by default to include all data)
#' @param week.max maximum week value to use (set very high by default to include all data)
#'
#' @return list of data.tables with dt + col.study.day and col.study.week
#'
#' @export

addStudyTimes.lst <- function(lst, dt.dates, col.time='StartTime', col.datesId='record_id', col.datesStudyStartDate='study_start_date', col.datesStudyWeek='week', col.datesStudyWeekDate='start_date', col.study.day='study.day', col.study.week='study.week', num.week.min=-1000, num.week.max=1000) {
  # create a named list element vector for easy lapply use
  vec.elements = setNames(names(lst), names(lst))

  # lapply to add columns using the data.table version of this function
  lst.new = lapply(vec.elements, function(x) {addStudyTimes.dt(lst[[x]], x, dt.dates, col.time, col.datesId, col.datesStudyStartDate, col.datesStudyWeek, col.datesStudyWeekDate, col.study.day, col.study.week, num.week.min, num.week.max)})

  return(lst.new)
}

#' List to matrix
#'
#' Function to convert a list of data.tables into a matrix with list names as columns, id
#'
#' @param lst list of data.tables
#' @param str.id string with id variable name
#' @param str.measure string with measure variable name
#' @param fn.aggregate aggregation function. Default: sum
#'
#' @return a matrix with measure variable as rows, list names as columns, measuring the measure var using fn.aggregate
#'
#' @export

lstToMatrix.mat <- function(lst, str.id, str.measure, fn.aggregate=sum) {
  dt = lstToDt.dt(lst, str.id, str.measure, fn.aggregate)

  vec.names = setdiff(colnames(dt), str.id)
  mat = as.matrix(dt[, .SD, .SDcols=vec.names], rownames.value=dt[, get(str.id)])

  return(mat)
}

#' List to matrix
#'
#' Function to convert a list of data.tables into a matrix with list names as columns, id
#'
#' @param lst list of data.tables
#' @param str.id string with id variable name
#' @param str.measure string with measure variable name
#' @param fn.aggregate aggregation function. Default: sum
#'
#' @return a data.table with measure variable as rows, list names as columns, measuring the measure var using fn.aggregate
#'
#' @export

lstToDt.dt <- function(lst, str.id, str.measure, fn.aggregate=sum) {
  vec.names = setNames(names(lst), names(lst))
  lst.sub = lapply(vec.names, function(x) { dt = lst[[x]][, fn.aggregate(get(str.measure)), by=str.id]; setnames(dt, c(str.id, x)); return(dt) })

  dt.merged = Reduce(function(x, y) { merge(x, y, by=str.id, all=T)}, lst.sub)
  setkeyv(dt.merged, str.id)

  return(dt.merged)
}


#' Create labels
#'
#' Function to create a sample key / label table
#'
#' @param dir.files directory containing the files
#' @param str.filePattern file name pattern
#' @param str.idPattern regular expression to identify sample id in filename. Default: '^([^_]+)_.*'
#'
#' @return data.table with dir, file, and label (id)
#'
#' @export

createLabels.dt <- function(dir.files, str.filePattern, str.idPattern='^([^_]+)_.*') {
  vec.files = list.files(dir.files, pattern=str.filePattern)
  vec.labels = sub(str.idPattern, '\\1', vec.files)
  lbl = data.table(dir=dir.files, file=vec.files, label=vec.labels)
  return(lbl)
}

#' Read tables
#'
#' Function to read in a set of data tables specified by a label table
#'
#' @param dt.lbl data.table with sample labels, directory (can be relative to root), and file names
#' @param col.label label column name
#' @param col.dir directory column name
#' @param col.file file column name
#'
#' @return list of data.tables named by label
#'
#' @export

readTables.lst <- function(dt.lbl, col.label='label', col.dir='dir', col.file='file') {
  lst = list()

  for (i in 1:nrow(dt.lbl)) {
    i.label = dt.lbl[i, get(col.label)]
    i.file = normalizePath(file.path(dt.lbl[i, get(col.dir)], dt.lbl[i, get(col.file)]))
    print(i.file)
    lst[[i.label]] = fread(file=i.file)
  }

  return(lst)
}


#' Aggregate List
#'
#' Helper function to aggregate list data into columns that can later be merged into a monolithic table
#'
#' @param lst list with data
#' @param col.id id in list (ex. study day)
#' @param col.measure column to measure in list
#' @param vec.range vector with range of id values over which to aggregate
#' @param fn.lstAggregate function to aggregate list data
#' @param str.variable name for id column in new table after aggregation over all col.id in vec.idRange
#' @param str.value name for value column in new table
#' @param fn.rangeAggregate function to aggregate data in new table (ex. median of days in the specified date range). Default: median
#'
#' @return data.table with aggregated data
#'
#' @export

aggregateList.dt <- function(lst, col.id, col.measure, vec.range=c(), fn.lstAggregate, str.variable, str.value, fn.rangeAggregate=median) {
  dt.mlt = melt(lstToDt.dt(lst, str.id=col.id, str.measure=col.measure, fn.aggregate=fn.lstAggregate), id.vars=col.id, variable.name=str.variable, value.name=str.value, variable.factor=F, value.factor=F)

  if (length(vec.range) > 0) {
    dt.agg = dt.mlt[get(col.id) %in% vec.range, list(agg.value=fn.rangeAggregate(get(str.value), na.rm=T)), by=str.variable]
  } else {
    dt.agg = dt.mlt[, list(agg.value=fn.rangeAggregate(get(str.value), na.rm=T)), by=str.variable]
  }

  setnames(dt.agg, 'agg.value', str.value)

  return(dt.agg)
}
