# Text Tools
# By xjtang
#----------------------------------------------------------------

# Return the right i letters of a string
strRight <- function(x, i) {
  return(substr(x, nchar(x) - (i - 1), nchar(x)))
}

#--------------------------------------

# Return the left i letters of a string
strLeft <- function(x, i) {
  return(substr(x, 0, i))
}

#--------------------------------------

# Trim the right i letters of a string
trimRight <- function(x, i) {
  return(substr(x, 1, nchar(x) - i))
}

#--------------------------------------

# Trim the right i letters of a string
trimLeft <- function(x, i) {
  return(substr(x, i+1, nchar(x)))
}

#--------------------------------------

# Trim leading and trailing spaces
trimSpace <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

#--------------------------------------
# End