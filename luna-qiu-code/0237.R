data = readRDS("FullBoxes.rds")
doc.0237 = data$UCD_Lehmann_0237.jpg
#left bottom right top text confidence

#get rid of unecessary text
doc.0237$text = gsub('\\_', '', doc.0237$text)
doc.0237$text = gsub('\\.', '', doc.0237$text)
doc.0237$text = gsub('\\{', '', doc.0237$text)
doc.0237$text = gsub('\\\\', '', doc.0237$text)

doc.0237 = doc.0237[!doc.0237$text == "",]

plot(doc.0237$bottom)

#find difference between bottom
doc.0237$bottom.diff = ave(doc.0237$bottom, FUN=function(x) c(0,diff(x)))
doc.0237$height = doc.0237$top - doc.0237$bottom

sort(doc.0237$bottom.diff)
order(doc.0237$bottom.diff)
#213, 312, 381

doc.0237 = subset(doc.0237, height > 48)
doc.0237$bottom.diff[1] = 0

#find each line by indexing
doc.0237$index = 0
t = 1
doc.0237$index[1] = t
for (i in 1:nrow(doc.0237)) {
  doc.0237$index[i] = t
  if (doc.0237$bottom.diff[i] > 30) {
    t = t+1
    doc.0237$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(doc.0237$index)) {
  splitted[[i]] = doc.0237[doc.0237$index == i, ]
}

# #find if name of wine is on multiple lines
# diff = numeric(max(doc.0237$index))
# for (i in 1: max(doc.0237$index)) {
#   diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
# }
# 
# #if name of wine is on multiple lines, combines dataframes
# for (i in 1: max(doc.0237$index)) {
#   if (diff[i] < 600) {
#     splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
#     diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
#     splitted[[i+1]] = NULL
#   }
# }

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

#break up word: strplit
grepled = NULL
first_true = NULL
l = NULL
p = NULL
for(i in 1:length(stringsplit)) {
  grepled[[i]] = grepl("[0-9]{3}", stringsplit[[i]])
  l[i] = length(grepled[[i]])
  first_true[i] = which(grepled[[i]] == TRUE)[2]
  p[i] = l[i] - first_true[i]
  if(grepled[[i]][l[i]] == FALSE) {
    stringsplit[[i]] = stringsplit[[i]][1:length(stringsplit[[i]])-1]
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