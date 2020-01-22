# Print some information about a PM2.5 regression object.

PM2.5.rinfo = function(x) {
  if (class(x) == 'character') {
    r = readRDS(x)
  } else if (class(x) == 'PM2.5.regression') {
    r = x
  } else stop('invalid argument')
  cat (r$region,r$SC,r$daterange,r$info,'\n',
    as.character(deparse(formula(r$fit))),';',paste(names(r$model.vars),'=',as.character(r$model.vars)),'\n',
    'local hours = ',as.character(r$local.hour.list),'\n',
    'Rval',signif(r$Rval,3),'num pts',nrow(model.frame(r$fit)),'\n',
    r$warnings,'\n',
    'rundate',r$runinfo$time,'\n')
}
