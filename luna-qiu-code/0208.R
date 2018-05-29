data = readRDS("FullBoxes.rds")
doc.0208 = data$UCD_Lehmann_0208.jpg
#left bottom right top text confidence

top = subset(doc.0208, left > 2500 & bottom < 2600 & bottom > 1860)

plot(top$bottom)

#find difference between bottom
top$bottom.diff = ave(top$bottom, FUN=function(x) c(0,diff(x)))
top$height = top$top - top$bottom

top$bottom.diff[1] = 0

#find each line by indexing
top$index = 0
t = 1
top$index[1] = t
for (i in 1:nrow(top)) {
  top$index[i] = t
  if (top$bottom.diff[i] > 70) {
    t = t+1
    top$index[i] = t
  }
}

top = subset(top, height > 50)
top$text = gsub('\\.', ' ', top$text)

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(top$index)) {
  splitted[[i]] = top[top$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(top$index))
for (i in 1: max(top$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

frame = NULL
for (i in 1:7) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

#if line has < 5 characters, delete
#put . back in prices
#if line starts with number, delete. It is wine no.


bottom = subset(doc.0208, left > 2400 & bottom > 3000 & bottom < 5100)
bottom$bottom.diff = ave(bottom$bottom, FUN=function(x) c(0,diff(x)))

bottom$text = gsub('\\_', '', bottom$text)
bottom$text = gsub('\\.', '', bottom$text)
bottom$text = gsub('\\{', '', bottom$text)
bottom$text = gsub('\\\\', '', bottom$text)
bottom$text = gsub('\\£', '', bottom$text)
bottom$text = gsub('\\=', '', bottom$text)
bottom$text = gsub('\\:', '', bottom$text)
bottom$text = gsub('\\§', '', bottom$text)
bottom$text = gsub('\\;', '', bottom$text)
bottom$text = gsub('\\|', '', bottom$text)

#doc.0237 = doc.0237[!doc.0237$text == "",]

bottom$index = 0
t = 1
bottom$index[1] = t
for (i in 1:nrow(bottom)) {
  bottom$index[i] = t
  if (bottom$bottom.diff[i] > 40) {
    t = t+1
    bottom$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(bottom$index)) {
  splitted[[i]] = bottom[bottom$index == i, ]
}

frame = NULL
for (i in 1:length(splitted)) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

frame

#After extracting each line, put info in dataframe

stringsplit = strsplit(frame, " ")
for(i in 1:length(stringsplit)) {
  stringsplit[[i]] = stringsplit[[i]][stringsplit[[i]] != ""]
}

stringsplit = stringsplit[lapply(stringsplit,length)>2]
#source: https://stackoverflow.com/questions/19023446/remove-empty-elements-from-list-with-character0

#break up word: strplit
grepled = NULL
first_true = NULL
l = NULL
p = NULL
for(i in 1:length(stringsplit)) {
  grepled[[i]] = grepl("[0-9]{3}", stringsplit[[i]])
  l[i] = length(grepled[[i]])
  first_true[i] = which(grepled[[i]] == TRUE)[1]
  #may include an if statement so that first_true = ... when there's a number in the front
  #and first_true = ... when there's no number in the front
  p[i] = l[i] - first_true[i]
}

for(i in 1:length(stringsplit)) {
  if(grepled[[i]][l[i]] == FALSE) {
    stringsplit[[i]] = stringsplit[[i]][1:length(stringsplit[[i]])-1]
  }
if(grepled[[i]][1] == TRUE && grepled[[i]][l[i]] == FALSE) {
  stringsplit[[i]] = rev(stringsplit[[i]])
}
}

mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode(p)

name = NULL
price = NULL
for(i in 1:length(stringsplit)) {
  name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - mode(p)) - 1]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - mode(p)):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
}

database = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database$Name = name

for(i in 1:length(stringsplit)) {
  database$price1[i] = price[[i]][1]
  database$price2[i] = price[[i]][2]
}

database = na.omit(database)
