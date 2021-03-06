proftable <- function(file, teste, lines = 10) 
{
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  
  filelines <- grep("#File", profdata)
  
  files <- profdata[filelines]
  
  if (length(filelines) > 0)
    profdata <- profdata[-1:-filelines]
  
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  
  calls.len <- range(sapply(calls, length))
  
  parent.call <- unlist(lapply(seq(calls.len[1]), function(i) Reduce(intersect, lapply(calls,"[[", i))))
  
  calls <- lapply(calls, function(x) setdiff(x, parent.call))
  
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  
  colnames(stacktable) <- c("PctTime", "Call")
  
  stacktable <- head(stacktable, lines)
  
  if (length(parent.call) > 0) 
  {
    parent.call <- paste(parent.call, collapse = " > ")
  } 
  else 
  {
    parent.call <- "None"
  }
  
  frac <- sum(stacktable$PctTime)

  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  
  cat("\n")
  
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  
  cat("\n")
  
  cat(paste(files, collapse="\n"))
  
  cat("\n")
  
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
 
  invisible(stacktable)
  line = paste(teste,',',total.time)
  write(line,file="saida.txt",append=TRUE)
}