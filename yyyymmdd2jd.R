source('~esswein/lib/R/julday.R')
yyyymmdd2jd = function(yyyymmdd) {
  yyyymmdd = as.integer(yyyymmdd)
  yyyy = yyyymmdd %/% 10000
  mm = (yyyymmdd %% 10000) %/% 100
  dd = yyyymmdd %% 100
  julday(mm,dd,yyyy,0,0,0)  
}
