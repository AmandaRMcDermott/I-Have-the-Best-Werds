temp = list.files(pattern="*.txt")

text_test <-  readLines(temp[1], header = FALSE, sep = "@", quote = "", stringsAsFactors = FALSE)

text_test_2 <-  read.delim(temp[5], header = FALSE, sep = "@", quote = "", stringsAsFactors = FALSE)
text_test_3 <-  readLines(paste("UNGD/", temp[1],sep = ""))
text_test_3 <-  readLines(temp[1], n = 1)
text_test_3
####Failed attempt one
new <- as.data.frame(rbind(c(text_test, text_test_2)))
for (i in 1:10){
  hold <- read.delim(temp[i], header = FALSE, sep = "@")
  rbind(df, hold)
}

###More code that doesn't work.
your_data_frame <- do.call(rbind,lapply("NIC_72_2017",read.delim(header = FALSE, sep = "@")))

###Failed attempt two
###Creating an empty data frame
df <- data.frame(Text=character(), speech = character()) 
for (i in 1:length(temp)){
  print(i)
  df[i, ] <- c(temp[i], read.delim(temp[i], header = FALSE, sep = "@", quote = "", stringsAsFactors = FALSE))
}

###Testing sotuf full
sotu_full[168,2]

###More code that doesn't work
files <- temp
fls <- NULL
lns <- NULL
for (file in files) {
  my_lines <- readLines(file)
  print(file)
  for (line in my_lines) {
    fls <- c(fls, file)
    lns <- c(lns, line)
  }
}
df <- data.frame(file=fls, fline=lns)
print(df)
#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.table(x, header=T)) 

#assuming the same header/columns for all files
datafr = do.call("rbind", datalist) 
