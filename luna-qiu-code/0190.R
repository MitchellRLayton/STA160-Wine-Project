#read data
#OCR is not perfect. Just accept it
data = readRDS("FullBoxes.rds")
#read file
doc.0190 = data$UCD_Lehmann_0190.jpg
#left bottom right top text confidence

plot(doc.0190$bottom)

#read left table
left = subset(doc.0190, doc.0190$left < max(doc.0190$left)/2 & doc.0190$bottom > 3500)
plot(left$bottom)

#find difference between bottom
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

#get rid of unecessary text
left$text = gsub('\\_', '', left$text)
left$text = gsub('\\.', '', left$text)
left$text = gsub('\\,', '', left$text)
left$text = gsub('\\\\', '', left$text)
left$text = gsub('\\\\', '', left$text)
left$text = gsub('\\£', '', left$text)
left$text = gsub('\\~', '', left$text)


#find each line by indexing
left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 50) {
    t = t+1
    left$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(left$index)) {
  splitted[[i]] = left[left$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(left$index))
for (i in 1: max(left$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: max(left$index)) {
if (diff[i] < 600) {
  splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
  splitted[[i+1]] = NULL
}
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
  first_true[i] = min(which(grepled[[i]] == TRUE))
  p[i] = l[i] - first_true[i]
}

mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode(p)

name = NULL
price = NULL
for(i in 1:length(stringsplit)) {
  name[[i]] = stringsplit[[i]][1:(l[i] - mode(p)) - 1]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(l[i] - mode(p)):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
  
}

database = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)), "price3" = numeric(length(price)), "price4" = numeric(length(price)))
database$Name = name

for(i in 1:length(stringsplit)) {
  database$price1[i] = price[[i]][1]
  database$price2[i] = price[[i]][2]
  database$price3[i] = price[[i]][3]
  database$price4[i] = price[[i]][4]
}

#left bottom right top text confidence
#read right table
right = subset(doc.0190, doc.0190$left > max(doc.0190$left)/2 & doc.0190$bottom > 3470 & doc.0190$bottom < 5250)
plot(right$bottom)

#find difference between bottom
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

#get rid of unecessary text
right$text = gsub('\\_', '', right$text)
right$text = gsub('\\.', '', right$text)
right$text = gsub('\\,', '', right$text)
right$text = gsub('\\\\', '', right$text)
right$text = gsub('\\\\', '', right$text)
right$text = gsub('\\£', '', right$text)
right$text = gsub('\\~', '', right$text)
right$text = gsub('\\:', '', right$text)
right$text = gsub('\\;', '', right$text)
right$text = gsub('\\$', '', right$text)
right$text = gsub('\\|', '', right$text)


#find each line by indexing
right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 50) {
    t = t+1
    right$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted.right = NULL
for (i in 1: max(right$index)) {
  splitted.right[[i]] = right[right$index == i, ]
}

#find if name of wine is on multiple lines
diff.right = numeric(max(right$index))
for (i in 1: max(right$index)) {
  diff.right[i] = splitted.right[[i]]$right[nrow(splitted.right[[i]])] - splitted.right[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: max(right$index)) {
  if (diff.right[i] < 600) {
    splitted.right[[i]] = rbind(splitted.right[[i]], splitted.right[[i+1]])
    diff.right[i] = splitted.right[[i]]$right[nrow(splitted.right[[i]])] - splitted.right[[i]]$left[1]
    splitted.right[[i+1]] = NULL
  }
}

frame.right = NULL
for (i in 1:length(splitted.right)) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted.right[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted.right[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame.right[i] = paste(splitted.right[[i]]$text, collapse = ' ')
}

#After extracting each line, put info in dataframe

stringsplit = strsplit(frame.right, " ")
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
  first_true[i] = min(which(grepled[[i]] == TRUE))
  p[i] = l[i] - first_true[i]
}

#find rows with no numbers
index = which(first_true == Inf)

# delete rows with no numbers
# doesn't work
# for(j in 1: length(stringsplit)) {
#   for(i in 1: length(index)) {
#   if (j == index[i]) {
#     stringsplit[[i]] = NULL
#     index[i+1] = index[i+1] - 1
#   }
# }
# }

mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode(p)

name = NULL
price = NULL
for(i in 1:length(stringsplit)) {
  name[[i]] = stringsplit[[i]][1:(l[i] - mode(p)) - 1]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(l[i] - mode(p)):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
  
}

database.right = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)), "price3" = numeric(length(price)), "price4" = numeric(length(price)))
database.right$Name = name

for(i in 1:length(stringsplit)) {
  database.right$price1[i] = price[[i]][1]
  database.right$price2[i] = price[[i]][2]
  database.right$price3[i] = price[[i]][3]
  database.right$price4[i] = price[[i]][4]
}

output = rbind(database, database.right)

train = output

train$Name[1] = "BIRCHBROOK"
train$Name[3] = "BELLOW'S CLUB"
train$Name[4] = "P & T KENTUCKY BRED"
train$Name[5] = "J.W. DANT BONDED"
train$Name[6] = "S. S. PIERCE KENTUCKY"
train$Name[8] = "EARLY TIMES"
train$Name[11] = "P & T PRIVATE STOCK"
train$Name[13] = "OLD TAYLOR (86°)"
train$Name[14] = "“21” BRANDS BONDED"
train$Name[16] = "OLD CHARTER"
train$Name[19] = "VIRGINIA GENTLEMAN (Qts. Only)"
train$Name[21] = "I. W. HARPER BONDED"
train$Name[23] = "OLD GRANDAD BONDED"
train$Name[30] = "CORBY'S RESERVE"
train$Name[34] = "S. S. PIERCE PRIVATE STOCK"
train$Name[35] = "BELLOW'S PARTNER'S CHOICE"
train$Name[36] = "CALVERT RESERVE"
train$Name[37] = "SCHENLEY RESERVE"
train$Name[39] = "\"21” BRANDS CLUB SPECIAL"
train$Name[40] = "FOUR ROSES"
train$Name[41] = "MELROSE RARE"
train$Name[42] = "LORD CALVERT"
train$Name[43] = ""
train$Name[44] = "MT VERNON 7 YEAR (86 Proof)"
train$Name[45] = "BELLOW'S CLUB"
train$Name[46] = "S. S. PIERCE KENTUCKY"
train$Name[48] = "MICHTER'S POT STILL (JUG)"
train$Name[49] = "WILD TURKEY (8 Year)"
train$Name[51] = "MacNAUGHTON'S"
train$Name[52] = "CANADIAN CLUB"
train$Name[53] = "SCHENLEY'S 0. F. C."
train$Name[54] = "SEAGRAM'S V. O."





train$price1[1] = "4.43"
train$price1[3] = "4.79"
train$price1[5] = "4.89"
train$price1[16] = "6.20"
train$price1[20] = "6.59"
train$price1[26] = "6.99"
train$price1[30] = "4.30"
train$price1[36] = "4.79"
train$price1[40] = "4.95"
train$price1[41] = "5.15"
train$price1[42] = "5.40"
train$price1[43] = ""
train$price1[44] = "4.29"
train$price1[45] = "4.79"
train$price1[49] = "8.73"
train$price1[50] = ""
train$price1[52] = "6.30"
train$price1[53] = "6.30"
train$price1[54] = "6.40"







train$price2[1] = "50.50"
train$price2[3] = "54.61"
train$price2[8] = "59.17"
train$price2[13] = "63.73"
train$price2[16] = "70.68"
train$price2[37] = "54.61"
train$price2[40] = "56.43"
train$price2[42] = "61.56"
train$price2[43] = ""
train$price2[45] = "54.61"
train$price2[50] = ""
train$price2[53] = "71.82"
train$price2[54] = "72.96"






train$price3[13] = "-"
train$price3[14] = "-"
train$price3[17] = "7.75"
train$price3[19] = "6.59"
train$price3[20] = "8.17"
train$price3[22] = "8.19"
train$price3[41] = "6.35"
train$price3[42] = "6.75"
train$price3[43] = ""
train$price3[44] = "5.29"
train$price3[45] = "5.89"
train$price3[49] = "-"
train$price3[50] = ""





train$price4[14] = "-"
train$price4[20] = "93.15"
train$price4[21] = "93.37"
train$price4[25] = "99.73"
train$price4[31] = "60.31"
train$price4[41] = "72.39"
train$price4[42] = "76.95"
train$price4[43] = ""
train$price4[44] = "60.31"
train$price4[45] = "67.15"
train$price4[50] = ""
train$price4[53] = "-"

train = train[-43,]
train = train[-49,]

adist(paste(train$Name, collapse = " "), paste(output$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(output$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(output$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])

adist(paste(train$price3, collapse = " "), paste(output$price3, collapse = " "))
length(strsplit(paste(train$price3, collapse = " "), "")[[1]])

adist(paste(train$price4, collapse = " "), paste(output$price4, collapse = " "))
length(strsplit(paste(train$price4, collapse = " "), "")[[1]])

