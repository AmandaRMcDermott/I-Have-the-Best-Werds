temp = list.files(pattern="*.txt")

text_test <-  readLines(temp[1], header = FALSE, sep = "@", quote = "", stringsAsFactors = FALSE)
temp[1]
text_test <-  readLines(temp[1])
text_test_2 <-  read.delim(temp[5], header = FALSE, sep = "@", quote = "", stringsAsFactors = FALSE)


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
