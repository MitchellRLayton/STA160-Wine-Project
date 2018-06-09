data = readRDS("FullBoxes.rds")
doc.3028 = data$UCD_Lehmann_3028.jpg
#left bottom right top text confidence

doc.3028$text = gsub('\\.', '', doc.3028$text)
doc.3028$text = gsub('\\:', '', doc.3028$text)
doc.3028$text = gsub('\\\\', '', doc.3028$text)
doc.3028$text = gsub('\\|', '', doc.3028$text)
doc.3028$text = gsub('\\~', '', doc.3028$text)
doc.3028$text = gsub('\\#', '', doc.3028$text)
doc.3028$text = gsub('\\,', '', doc.3028$text)
doc.3028$text = gsub('\\-', '', doc.3028$text)
doc.3028$text = gsub('\\®', '', doc.3028$text)
doc.3028$text = gsub('\\}', '', doc.3028$text)
doc.3028$text = gsub('\\!', '', doc.3028$text)
doc.3028$text = gsub('\\£', '', doc.3028$text)
doc.3028$text = gsub('\\¥', '', doc.3028$text)
doc.3028$text = gsub('\\+', '', doc.3028$text)
doc.3028$text = gsub('\\=', '', doc.3028$text)
doc.3028$text = gsub('\\©', '', doc.3028$text)
doc.3028$text = gsub('\\>', '', doc.3028$text)
doc.3028$text = gsub('\\?', '', doc.3028$text)
doc.3028$text = gsub('\\>', '', doc.3028$text)
doc.3028$text = gsub('\\§', '', doc.3028$text)
doc.3028$text = gsub('\\/', '', doc.3028$text)
doc.3028$text = gsub('\\—', '', doc.3028$text)
doc.3028$text = gsub('\\%', '', doc.3028$text)
doc.3028$text = gsub('\\]', '', doc.3028$text)

doc.3028 = doc.3028[!(doc.3028$text ==""),]
doc.3028 = doc.3028[doc.3028$left > 0, ]

table = subset(doc.3028, bottom > 2200 & bottom < 3550 & left > 1580 & left < 3100)
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
  name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - mode(p)) - 1]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - mode(p)):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
}

database = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)))
database$Name = name

for(i in 1:length(stringsplit)) {
  database$price1[i] = price[[i]][1]
}

database = na.omit(database)


train = database
train$Name[1] = "1790 MADEIRA BRANCO"
train$Name[2] = "1808 SERCIAL"
train$Name[3] = "1805 VERDELHO GRAND RESERVE"
train$Name[4] = "1818 MADEIRA BRANCO"
train$Name[5] = "1856 SERCIAL"
train$Name[6] = "1860 VERDELHO"
train$Name[7] = "1860 SERCIAL"
train$Name[8] = "1866 CAMPANARIO BUAL"
train$Name[9] = "1869 BUAL"
train$Name[10] = "1869 SERCIAL"
train$Name[11] = "1870 SERCIAL"
train$Name[12] = "1870 BUAL"
train$Name[13] = "1874 BUAL"
train$Name[14] = "1882 BUAL"
train$Name[15] = "1884 SERCIAL"
train$Name[16] = "1885 MALMSEY"
train$Name[17] = "1885 SERCIAL PARGO"

#change price 1
train$price1[1] = "27.50"
train$price1[2] = "24.00"
train$price1[3] = "24.00"
train$price1[4] = "20.00"
train$price1[5] = "17.50"
train$price1[6] = "16.75"
train$price1[7] = "16.75"
train$price1[8] = "15.00"
train$price1[9] = "15.00"
train$price1[10] = "15.00"
train$price1[11] = "15.00"
train$price1[12] = "15.00"
train$price1[13] = "15.00"
train$price1[14] = "13.75"
train$price1[15] = "13.75"
train$price1[16] = "13.75"
train$price1[17] = "13.75"




adist(paste(train$Name, collapse = " "), paste(database$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(database$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])


