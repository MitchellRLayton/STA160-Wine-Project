data = readRDS("FullBoxes.rds")
doc.0260 = data$UCD_Lehmann_0260.jpg
#left bottom right top text confidence

doc.0260$text = gsub('\\.', '', doc.0260$text)
doc.0260$text = gsub('\\[', '', doc.0260$text)

table = subset(doc.0260, left > 2400 & bottom < 3500)
table

table$bottom.diff = ave(table$bottom, FUN=function(x) c(0,diff(x)))

table$index = 0
t = 1
table$index[1] = t
for (i in 1:nrow(table)) {
  table$index[i] = t
  if (table$bottom.diff[i] > 25) {
    t = t+1
    table$index[i] = t
  }
}

splitted = NULL
for (i in 1: max(table$index)) {
  splitted[[i]] = table[table$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(table$index))
for (i in 1: max(table$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: length(splitted)) {
  if (diff[i] < 680) {
    splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
    diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
    splitted[[i+1]] = NULL
  }
}

frame = NULL
for (i in 1:length(splitted)) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$table.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], table.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

frame

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

