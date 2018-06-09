data = readRDS("FullBoxes.rds")
doc.2362 = data$UCD_Lehmann_2362.jpg
#left bottom right top text confidence

doc.2362$text = gsub('\\.', '', doc.2362$text)
doc.2362$text = gsub('\\:', '', doc.2362$text)
doc.2362$text = gsub('\\\\', '', doc.2362$text)
doc.2362$text = gsub('\\|', '', doc.2362$text)
doc.2362$text = gsub('\\~', '', doc.2362$text)
doc.2362$text = gsub('\\#', '', doc.2362$text)
doc.2362$text = gsub('\\,', '', doc.2362$text)
doc.2362$text = gsub('\\-', '', doc.2362$text)
doc.2362$text = gsub('\\®', '', doc.2362$text)
doc.2362$text = gsub('\\}', '', doc.2362$text)
doc.2362$text = gsub('\\!', '', doc.2362$text)
doc.2362$text = gsub('\\£', '', doc.2362$text)
doc.2362$text = gsub('\\¥', '', doc.2362$text)
doc.2362$text = gsub('\\+', '', doc.2362$text)
doc.2362$text = gsub('\\=', '', doc.2362$text)
doc.2362$text = gsub('\\©', '', doc.2362$text)
doc.2362$text = gsub('\\>', '', doc.2362$text)
doc.2362$text = gsub('\\?', '', doc.2362$text)
doc.2362$text = gsub('\\>', '', doc.2362$text)
doc.2362$text = gsub('\\§', '', doc.2362$text)

doc.2362 = doc.2362[!(doc.2362$text ==""),]

left = subset(doc.2362, bottom > 4130 & left < 1270)
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 40) {
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
  first_true[i] = which(grepled[[i]] == TRUE)[1]
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

database.left = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.left$Name = name

for(i in 1:length(stringsplit)) {
  database.left$price1[i] = price[[i]][1]
  database.left$price2[i] = price[[i]][2]
}

database.left = na.omit(database.left)







middle = subset(doc.2362, bottom > 4130 & left > 1270 & left < 2500)
middle$bottom.diff = ave(middle$bottom, FUN=function(x) c(0,diff(x)))

middle$index = 0
t = 1
middle$index[1] = t
for (i in 1:nrow(middle)) {
  middle$index[i] = t
  if (middle$bottom.diff[i] > 40) {
    t = t+1
    middle$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(middle$index)) {
  splitted[[i]] = middle[middle$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(middle$index))
for (i in 1: max(middle$index)) {
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
  first_true[i] = which(grepled[[i]] == TRUE)[1]
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

database.middle = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.middle$Name = name

for(i in 1:length(stringsplit)) {
  database.middle$price1[i] = price[[i]][1]
  database.middle$price2[i] = price[[i]][2]
}

database.middle = na.omit(database.middle)






right = subset(doc.2362, bottom > 4130 & left > 2500)
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 34) {
    t = t+1
    right$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(right$index)) {
  splitted[[i]] = right[right$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(right$index))
for (i in 1: max(right$index)) {
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
  first_true[i] = which(grepled[[i]] == TRUE)[1]
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

database.right = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.right$Name = name

for(i in 1:length(stringsplit)) {
  database.right$price1[i] = price[[i]][1]
  database.right$price2[i] = price[[i]][2]
}

database.right = na.omit(database.right)

output = do.call("rbind", list(database.left, database.middle, database.right))

train = output

train$Name[1] = "BULLOCH & LADE"
train$Name[2] = "HARVEY'S"
train$Name[3] = "JOHN BEGG BLUE CAP"
train$Name[4] = "GILBEY'S SPEY ROYAL"
train$Name[5] = "KING WILLIAM"
train$Name[6] = "KING GEORGE"
train$Name[8] = "HANKY BANNISTER"
train$Name[9] = "DEERSTALKER"
train$Name[10] = "GROUSE"
train$Name[11] = "PETER DAWSON SPECIAL"
train$Name[12] = "CATTO'S GREY SEAL"
train$Name[13] = "GLEN GARRY (S. S. Pierce)"
train$Name[14] = "QUEEN ANNE"
train$Name[15] = "HUDSON'S BAY"
train$Name[20] = "GRAND MACNISH"
train$Name[22] = "MARTIN'S V.V.O"
train$Name[23] = "BALLANTINE"
train$Name[24] = "CUTTY SARK"
train$Name[25] = "WHITE HORSE"
train$Name[26] = "VAT 69"
train$Name[28] = "AMBASSADOR DELUXE"
train$Name[29] = "LONG JOHN"
train$Name[30] = "WHITE HEATHER"
train$Name[34] = "DEWAR'S WHITE LABEL"
train$Name[35] = "J & B"
train$Name[38] = "MACKIES ANCIENT"
train$Name[39] = "BARRISTER'S CHOICE (12 Year)"
train$Name[40] = "BERRY BROS. ST JAMES"
train$Name[42] = "S. S. PIERCE'S LIQUEUR (12 Year)"
train$Name[44] = "JOHNNIE WALKER BLACK LABEL"
train$Name[45] = "KINGS RANSOM"
train$Name[46] = "BUCHANAN DE LUXE"
train$Name[47] = "HIGHLAND NECTAR"
train$Name[48] = "BELL'S ROYAL VAT (12 Years)"
train$Name[49] = "CHIVAS REGAL (12 Years)"
train$Name[54] = "ROBERTSON'S B.E.B."
train$Name[55] = "GRANT'S (12 Years)"
train$Name[56] = "SMITH GLENLIVET (12 Years)"
train$Name[58] = "GRANTS (20 Years)"
train$Name[60] = "BALLANTINE (30 Years)"





train$price1[1] = "5.75"
train$price1[2] = "5.77"
train$price1[3] = "5.79"
train$price1[4] = "5.79"
train$price1[5] = "5.80"
train$price1[6] = "5.85"
train$price1[12] = "6.20"
train$price1[13] = "6.25"
train$price1[14] = "6.29"
train$price1[15] = "6.29"
train$price1[22] = "6.45"
train$price1[23] = "6.47"
train$price1[24] = "6.47"
train$price1[25] = "6.47"
train$price1[26] = "6.49"
train$price1[28] = "6.55"
train$price1[29] = "6.55"
train$price1[34] = "6.65"
train$price1[35] = "6.66"
train$price1[36] = "7.59"
train$price1[37] = "6.99"
train$price1[38] = "7.73"
train$price1[39] = "7.75"
train$price1[41] = "7.95"
train$price1[42] = "7.99"
train$price1[43] = "7.99"
train$price1[44] = "8.22"
train$price1[46] = "8.49"
train$price1[47] = "8.50"
train$price1[48] = "8.60"
train$price1[49] = "8.60"
train$price1[55] = "8.91"
train$price1[56] = "10.79"
train$price1[58] = "12.95"
train$price1[60] = "15.45"





train$price2[1] = "64.87"
train$price2[2] = "65.78"
train$price2[3] = "66.00"
train$price2[4] = "66.01"
train$price2[5] = "66.12"
train$price2[6] = "66.69"
train$price2[14] = "71.70"
train$price2[15] = "71.71"
train$price2[21] = "73.53"
train$price2[22] = "73.53"
train$price2[23] = "73.86"
train$price2[24] = "73.87"
train$price2[25] = "73.76"
train$price2[26] = "73.99"
train$price2[28] = "74.67"
train$price2[29] = "74.67"
train$price2[34] = "75.81"
train$price2[35] = "75.92"
train$price2[37] = "79.60"
train$price2[38] = "88.16"
train$price2[42] = "91.09"
train$price2[44] = "93.71"
train$price2[46] = "97.00"
train$price2[47] = "96.90"
train$price2[49] = "98.04"
train$price2[55] = "101.58"
train$price2[56] = "122.88"
train$price2[58] = "147.63"
train$price2[60] = "176.18"







adist(paste(train$Name, collapse = " "), paste(output$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(output$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(output$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])

