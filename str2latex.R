# Yuanting Lu
# Draft: 11/16/2018
# Update: 11/17/2018

str2latex <- function(x){
  x <- curlparentheses(x, "sqrt")
  x <- curlparentheses(x, "^")
  x <- gsub("log","ln", x)
  x <- gsub("ln", "\\\\ln ", x)
  x <- gsub("pi", "\\\\pi ", x)
  x <- gsub("\\*","\\\\cdot ",x)
  x <- gsub("sqrt","\\\\sqrt",x)
  x <- gsub("\\(","\\\\left(",x)
  x <- gsub("\\)","\\\\right)",x)
  return(x)
}