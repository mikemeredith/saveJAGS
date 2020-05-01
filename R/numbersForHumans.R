
# Format big numbers for humans to read

humNum <- function(x) {
  illions <- c("thousand", 'million', 'billion', 'trillion', 'quadrillion', 'quintillion',
      'sextillion', 'septillion', 'octillion', 'nonillion', 'decillion', 'undecillion',
      'duodecillion', 'tredecillion', 'quatttuor-decillion', 'quindecillion', 'sexdecillion',
      'septen-decillion', 'octodecillion', 'novemdecillion', 'vigintillion', 'centillion')
  if(x < 10000)
    return(as.character(x))
  om <- min(log10(x) %/% 3, 22)
  dec <- x / 10^(om*3)
  paste(signif(dec, 3), illions[om])
}


humBytes <- function(x) {
  XBytes <- c("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  om <- min(log10(x) %/% 3, 8)
  dec <- x / 10^(om*3)
  paste(signif(dec, 3), XBytes[om+1])
}

