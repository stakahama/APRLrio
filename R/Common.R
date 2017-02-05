
#' Strip
#'
#' Strip ends (whitespace, \code{"\n"}, \code{"\t"})
#'
#' @param x string
#'
#' @return stripped text
#' @export

Strip <- function(x)
  gsub("^[ \n\t]*|[ \n\t]*$","",x)


#' StripNonASCII
#'
#' Remove non-ascii text
#'
#' @param x string
#'
#' @return stripped text
#' @export

StripNonASCII <- function(x)
  iconv(enc2utf8(x), "utf8", "ASCII", sub="")


#' RemoveEmptyLines
#'
#' Remove empty lines
#'
#' @param x string
#'
#' @return text with less lines
#' @export

RemoveEmptyLines <- function(x)
  x[nchar(x)>0]

#' StripWhite
#'
#' Strip whitespace (space)
#'
#' @param x string
#'
#' @return stripped text
#' @export

StripWhite <- function (x)
  gsub("^[ ]*|[ ]*$", "", x)
