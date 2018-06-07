library(LexisPlotR)

mylexis <- lexis.grid(year.start = 2010, year.end = 2020, age.start = 0, age.end = 5 )
lexis.lifeline(lg = mylexis, entry = entries, colour="white")

entries <- c("2009-01-01", "2008-01-01", "2007-01-01", "2006-01-01",
             "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
             "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01")
