#Author: Fagun Vasavada
options(warn = -1)
library("data.tree", lib.loc="~/R/win-library/3.3")
library("stringr", lib.loc="~/R/win-library/3.3")

#Read the Console for Data & Partition Files
dataFile <- readline("Enter Your Data File Name (With Extension): ")
partitionFile <- readline("Enter Your Partition File Name (With Extension): ")
outputFile <- readline("Enter Your Output File Name (With Extension): ")

#Custom Method to Read the Horizontally Aligned matriy
read.custom = function(fl, header=TRUE, sep=" ") {
  n = max(count.fields(fl, sep=sep), na.rm=TRUE)
  y = readLines(fl)
  .splitvar = function(y, sep, n) {
    var = unlist(strsplit(y, split=sep))
    length(var) = n
    return(var)
  }
  y = do.call(cbind, lapply(y, .splitvar, sep=sep, n=n))
  y = apply(y, 1, paste, collapse=sep) 
  formattedOut = read.csv(text=y, sep=sep, header=header)
  return(formattedOut)
}

#Calcuate Entropy & Information Gain
IsUniform <- function(data) {
  #Return True if Uniform Output
  length(unique(data[,ncol(data)])) == 1
}
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}
InformationGain <- function( vectortbl ) {
  vectortbl <- as.data.frame.matrix(vectortbl)
  entropyBefore <- Entropy(colSums(vectortbl))
  s <- rowSums(vectortbl)
  entropyAfter <- sum (s / sum(s) * apply(vectortbl, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

#Get Data Set
tempScan <- readLines(str_replace_all(capture.output(cat(getwd(),"/",dataFile)), fixed(" "), ""))
dataSet <- read.csv(text=tempScan, skip=1, sep = " ", header = FALSE, nrows=length(tempScan) - 1)

#Get Partition Set
inputPartition <- read.custom(str_replace_all(capture.output(cat(getwd(),"/",partitionFile)), fixed(" "), ""))

#Calculating F Values Vector
fValue <-c()
temp <- inputPartition
for (i in 1:ncol(temp)) {
  dataTemp <- dataSet[(row.names(dataSet) %in% temp[,i]),]
  #Calculate Information Gain
  infoGain <- sapply(colnames(dataTemp)[-ncol(dataTemp)],
                     function(x) InformationGain(
                       table(dataTemp[,x], dataTemp[,ncol(dataTemp)])
                     )
  )
  #Calculate Max Gain Feature
  maxfeature <- names(infoGain)[infoGain == max(infoGain)][1]
  maxInfoGain <- (nrow(dataTemp)/nrow(dataSet)) * max(infoGain)
  #Calculate F Value
  fValue <- append(fValue, maxInfoGain)
}

#Choose the Partition to spli With Max FValue
chosenPartition <- which(fValue == max(fValue))

#Working Dataset for the Selected Partition
data <- dataSet[(row.names(dataSet) %in% temp[,chosenPartition]),]

#Write Partitions to Output Files
write("Partitions Created After Running ID3 Algorithm",
      file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
write("_____________________________________________________________________________",
      file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
for(i in 1:length(temp)) {
  #cat("\n",p[i],row.names(childObs[[i]]))
  if(i!=chosenPartition)
  {
    write(str_replace_all(capture.output(cat(names(temp[i]),temp[,i])), fixed("NA"), ""), file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
  }
}

#Run ID3 Algorithm on the Partition
node <- Node$new("data")
node$obsCount <- nrow(data)
#if the data-set is uniform (i.e. All Diverge to Same Output)
if (IsUniform(data)) {
  #construct a leaf having the  name of the pure feature (e.g. 'toxic')
  child <- node$AddmerChild(unique(data[,ncol(data)]))
  node$feature <- tail(names(data), 1)
  node$feature
  child$obsCount <- nrow(data)
  child$obsCount
  child$feature <- ''
} else {
  #Choose The Feature With The Highest Information Gain
  ig <- sapply(colnames(data)[-ncol(data)],
               function(x) InformationGain(
                 table(data[,x], data[,ncol(data)])
               )
  )
  feature <- names(ig)[ig == max(ig)][1]
  node$feature <- feature
  #Take The Subset of Data With That Selected Feature Value
  childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
  
  p <- c("z1","z2","z3","z4","z5","z6","z7","z8","z9","z10")
  for(i in 1:length(childObs)) {
    write(capture.output(cat(p[i],row.names(childObs[[i]]))), file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
  }
  cat("Script Successfully Run !!")
  write(capture.output(cat("The Chosen Partition for Split is:",names(temp[chosenPartition])," Selected Feature:", feature)),
        file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
  write(capture.output(cat("Output File Created @: ", str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""))),
        file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
  write("_____________________________________________________________________________\n",
        file = str_replace_all(capture.output(cat(getwd(),"/",outputFile)), fixed(" "), ""), append = TRUE)
}
