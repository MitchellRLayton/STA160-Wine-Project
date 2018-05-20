data = readRDS("FullBoxes.rds")
doc.0237 = data$UCD_Lehmann_0237.jpg
#left bottom right top text confidence

plot(doc.0237$bottom)

#find difference between bottom
doc.0237$bottom.diff = ave(doc.0237$bottom, FUN=function(x) c(0,diff(x)))
doc.0237$height = doc.0237$top - doc.0237$bottom

sort(doc.0237$bottom.diff)
order(doc.0237$bottom.diff)
#213, 312, 381

#get rid of unecessary text
doc.0237$text = gsub('\\_', '', doc.0237$text)
doc.0237$text = gsub('\\.', '', doc.0237$text)
#delete \ 's

doc.0237 = subset(doc.0237, height > 45)
doc.0237$bottom.diff[1] = 0

#find each line by indexing
doc.0237$index = 0
t = 1
doc.0237$index[1] = t
for (i in 1:nrow(doc.0237)) {
  doc.0237$index[i] = t
  if (doc.0237$bottom.diff[i] > 50) {
    t = t+1
    doc.0237$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(doc.0237$index)) {
  splitted[[i]] = doc.0237[doc.0237$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(doc.0237$index))
for (i in 1: max(doc.0237$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: max(doc.0237$index)) {
  if (diff[i] < 600) {
    splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
    diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
    splitted[[i+1]] = NULL
  }
}

frame = NULL
for (i in 1:24) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

#if ;ine has < 5characters, delete
#put . back in prices
#if line starts with number, delete. It is wine no.
