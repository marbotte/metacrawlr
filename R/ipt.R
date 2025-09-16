# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Listing files from a datadir of an IPT instance
#'
#' @param dataDir Datadir must be the directory where all of the dataset are found (that is, the next level in the directory hierarchy must be the directories with the names of datasets)
#' @param encoding encoding of the files in the datadir (if null this will be guessed for each file with stringi::stru_enc_detect)
#' @param language lenguaje of the files (when the encoding is associated with a languaje), if encoding is null, this will be guessed with stringi::stri_enc_detect
#' @param verbose boolean, whether to send messages during execution
#'
#' @returns file table (data.frame) with filename (name), encoding, language, extensions, name/code of the dataset (dataFolder), full_name (name of the file with the complete path)
#' @export
#'
#'

ipt_listFiles <- function(dataDir,encoding=NULL,language=NULL, verbose=T)
{
  allFiles<-dir(dataDir, recursive = T, all.files = T, full.names = T)
  if(is.null(encoding))
  {
    if(verbose)
    {message("Guessing encoding of the files\n...")}
    encodings<-Reduce(rbind,lapply(stringi::stri_enc_detect(allFiles),function(x)x[1,,drop=F]))
    encoding<-encodings$Encoding
    language<-encodings$Language
    if(verbose)
    {message("DONE\n\n")}
  }
  basenames<-basename(allFiles)
  dirnames<-dirname(allFiles)
  stopifnot(grepl(paste0("^", dataDir, "\\/([^/]+)\\/.*"), allFiles))
  dataFolder<-gsub(paste0("^",dataDir,"\\/([^/]+)\\/.*"),"\\1",allFiles)
  extensions<-as.character(rep(NA,length(allFiles)))
  extensions[grepl("^.*\\.(.+)$",basenames)]<-sub("^.*\\.(.+)$","\\1",basenames[grepl("^.*\\.(.+)$",basenames)])
  res <- data.frame(name = basenames,
                    encoding = encoding,
                    language = language,
                    extension = extensions,
                    dataFolder = dataFolder,
                    full_name = allFiles)
  return(res)
}

#' Extract xml file contents from the datadir of an ipt instance
#'
#' @param iptFileTable data.frame such as those returned by ipt_listFiles
#' @param modeExtract one of "EML" (for extracting basic Ecological Metadata Language variable from the eml.xml files),  "Structure" (for extracting structural metadata from the "resource.xml" files)
#'
#' @returns a list (names as dataFolder from the IPT instance) with multiline character vector of length 1
#' @export
#'
ipt_extractXml<-function(iptFileTable, modeExtract=c("EML", "Structure", "both"))
{
  modeExtract<-match.arg(modeExtract)
  filesToExtract<-c(if(modeExtract %in% c("EML","both")){"eml.xml"} else {NULL},if(modeExtract %in% c("Structure","both")){"resource.xml"} else {NULL})
  iptFileTable<-iptFileTable[iptFileTable$name %in% filesToExtract,]
  extractedXml<-lapply(by(iptFileTable,iptFileTable$dataFolder,FUN = function(x)Reduce(c,mapply(readLines,x$full_name,encoding=x$encoding))),paste,collapse="\n")
  if(modeExtract=="both")
  {extractedXml<-lapply(extractedXml,FUN =function(x){ paste0("<document>\n",x,"</document>",collapse="\n")})}
  return(extractedXml)
}

# A<-mapply(readLines,res$full_name[res$name=="eml.xml"],encoding=res$encoding[res$name=="eml.xml"])
# n<-199
# #nameFile<-"eml.xml"
# nameFile<-"resource.xml"
# readLines(res$full_name[which(res$name==nameFile)[n]],encoding=res$encoding[which(res$name==nameFile)[n]])

