data = readRDS("FullBoxes.rds")
doc.3666 = data$UCD_Lehmann_3666.jpg
#left bottom right top text confidence

doc.3666$text = gsub('\\.', '', doc.3666$text)
doc.3666$text = gsub('\\:', '', doc.3666$text)
doc.3666$text = gsub('\\\\', '', doc.3666$text)
doc.3666$text = gsub('\\|', '', doc.3666$text)
doc.3666$text = gsub('\\~', '', doc.3666$text)
doc.3666$text = gsub('\\#', '', doc.3666$text)
doc.3666$text = gsub('\\,', '', doc.3666$text)
doc.3666$text = gsub('\\-', '', doc.3666$text)
doc.3666$text = gsub('\\®', '', doc.3666$text)
doc.3666$text = gsub('\\}', '', doc.3666$text)
doc.3666$text = gsub('\\!', '', doc.3666$text)
doc.3666$text = gsub('\\£', '', doc.3666$text)
doc.3666$text = gsub('\\¥', '', doc.3666$text)
doc.3666$text = gsub('\\+', '', doc.3666$text)
doc.3666$text = gsub('\\=', '', doc.3666$text)
doc.3666$text = gsub('\\©', '', doc.3666$text)
doc.3666$text = gsub('\\>', '', doc.3666$text)
doc.3666$text = gsub('\\?', '', doc.3666$text)
doc.3666$text = gsub('\\>', '', doc.3666$text)
doc.3666$text = gsub('\\§', '', doc.3666$text)
doc.3666$text = gsub('\\/', '', doc.3666$text)
doc.3666$text = gsub('\\—', '', doc.3666$text)
doc.3666$text = gsub('\\%', '', doc.3666$text)
doc.3666$text = gsub('\\]', '', doc.3666$text)

doc.3666 = doc.3666[!(doc.3666$text ==""),]
doc.3666 = doc.3666[doc.3666$left > 0, ]

table = subset(doc.3666, left > 500)
table$bottom.diff = ave(table$bottom, FUN=function(x) c(0,diff(x)))

table$index = 0
t = 1
table$index[1] = t
for (i in 1:nrow(table)) {
  table$index[i] = t
  if (table$bottom.diff[i] > 39) {
    t = t+1
    table$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(table$index)) {
  splitted[[i]] = table[table$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(table$index))
for (i in 1: max(table$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

frame = NULL
for (i in 1:length(splitted)) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

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
  first_true[i] = which(grepled[[i]] == TRUE)[2]
  #may include an if statement so that first_true = ... when there's a number in the front
  #and first_true = ... when there's no number in the front
  p[i] = l[i] - first_true[i]
}

for(i in 1:length(stringsplit)) {
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
  name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - 1) - 1]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - 1):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
}

database = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database$Name = name

for(i in 1:length(stringsplit)) {
  database$price1[i] = price[[i]][1]
  database$price2[i] = price[[i]][2]
}

database = na.omit(database)