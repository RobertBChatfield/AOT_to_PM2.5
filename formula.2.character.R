formula.2.character = function(formula) {
# Change "I(expr)" to "expr"

if (class(formula) != 'formula') stop('input is not a formula')

str = deparse(formula,width.cutoff=500)
# Rewrite "I()" parts one at a time:
repeat {
  I.pos = gregexpr('I(',str,fixed=T)
  mp.pos = -1
  if (I.pos[[1]][1] <= 0) break
  lev = 1
  for (ichar in (I.pos[[1]][1]+2):nchar(str)) {
    c = substr(str,ichar,ichar)
    if (c == '(') { lev = lev + 1 ; next }
    if (c == ')') {
      if (lev == 1) { mp.pos = ichar ; break }
      lev = lev - 1
      next
    }
  }
# print(mp.pos)
  if (mp.pos > 0) {
    substr(str,I.pos[[1]][1],I.pos[[1]][1]+1) = '  '
    substr(str,mp.pos,mp.pos) = ' '
  }
}

str = gsub('  *',' ',str)

str

}
