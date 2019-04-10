#' @import data.table
NULL


#' Filter dates
#'
#' Function to filter a data table around a specific date
#'
#' @param dt data.table with a date column
#' @param date.start date to filter around
#' @param days.up days prior to date.start to include. Default: 7
#' @param days.down days after to date.start to include. Default: 7
#' @param col.date name of the date column in dt. Default: 'date'
#' @param col.winDate name of the column to create with the relative number of days
#' @param str.format date string format to use for any non-date formatted columns. Default: '\%m-\%d-\%Y'
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
#' @param col.time column name or vector of column names with date/time data. Default: 'StartTime'
#' @param str.format incoming date format or vector of date formats corresponding to each element of the col.time vector. Default: '\%m/\%d/\%Y \%I:\%M:\%S \%p' (1/22/19 12:33:45 PM)
#' @param str.tz time zone for date conversion. Default: 'UTC' (avoids double counting issues)
#' @param col.dir column name in lbl with directory. Default: 'dir'
#' @param col.file column name in lbl with file. Default: 'file'
#' @param col.label column name in lbl with label. Default: 'label'
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

#' List to matrix
#'
#' Function to convert a list of data.tables into a matrix with list names as columns, id
#'
#' @param lst list of data.tables
#' @param str.id string with id variable name
#' @param str.measure string with measure variable name
#' @param fn.aggregate aggregation function. Default: sum
#' @param num.fill value to fill in any matrix elements without data. Default: NA_real_
#'
#' @return a matrix with measure variable as rows, list names as columns, measuring the measure var using fn.aggregate
#'
#' @export

lstToMatrix.mat <- function(lst, str.id, str.measure, fn.aggregate=sum, num.fill=NA_real_) {
  mat = acast(melt(lst, id.vars=str.id, measure.vars=str.measure, na.rm=T), list(str.id, 'L1'), fun.aggregate=fn.aggregate, fill=num.fill, drop=F)
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
#' @param num.fill value to fill in any matrix elements without data. Default: NA_real_
#'
#' @return a data.table with measure variable as rows, list names as columns, measuring the measure var using fn.aggregate
#'
#' @export

lstToDt.dt <- function(lst, str.id, str.measure, fn.aggregate=sum, num.fill=NA_real_) {
  dt = as.data.table(dcast(melt(lst, id.vars=str.id, measure.vars=str.measure, na.rm=T, variable.factor=F, value.factor=F), list(str.id, 'L1'), fun.aggregate=fn.aggregate, fill=num.fill, drop=F))
  return(dt)
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
    lst[[i.label]] = fread(i.file)
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

aggregateList.dt <- function(lst, col.id, col.measure, vec.range, fn.lstAggregate, str.variable, str.value, fn.rangeAggregate=median) {
  dt.mlt = melt(lstToDt.dt(lst, str.id=col.id, str.measure=col.measure, fn.aggregate=fn.lstAggregate), id.vars=col.id, variable.name=str.variable, value.name=str.value, variable.factor=F, value.factor=F)

  dt.agg = dt.mlt[get(col.id) %in% vec.range, list(agg.value=fn.rangeAggregate(get(str.value), na.rm=T)), by=str.variable]
  setnames(dt.agg, 'agg.value', str.value)

  return(dt.agg)
}
