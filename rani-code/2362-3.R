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

#change names
train = combined
train$Name[1] = "BULLOCH & LADE"
train$Name[2] = "HARVEY'S"
train$Name[3] = "JOHN'S BEGG BLUE CAP"
train$Name[4] = "GILBEY'S SPEY ROYAL"
train$Name[5] = "KING WILLIAM"
train$Name[6] = "GLENLOGIE ROYAL VAT"
train$Name[7] = "HANKEY BANNISTER"
train$Name[8] = "DEERSTALKER"
train$Name[9] = "GROUSE"
train$Name[10] = "PETER DAWSON SPECIAL"
train$Name[11] = "CATTO'S GREY SEAL"
train$Name[12] = "GLEN GARRY(S.S. Pierce)"
train$Name[13] = "QUEEN ANNE"
train$Name[14] = "HUDSON'S BAY"
train$Name[15] = "USHER'S GREEN STRIPE"
train$Name[16] = "BELL'S SPECIAL RESERVE"
train$Name[17] = "HOUSE OF LORDS"
train$Name[18] = "BLACK & WHITE"
train$Name[19] = "GRAND MACNISH"
train$Name[20] = "HIGHLAND QUEEN"
train$Name[21] = "MARTIN'S V.V.O."
#old smuggler is skipped
train$Name[22] = "BALLANTINE"
train$Name[23] = "CUTTY SARK"
train$Name[24] = "WHITE HORSE"
train$Name[25] = "VAT 69"
train$Name[26] = "JOHNNY WALKER RED LABEL"
train$Name[27] = "AMBASSADOR DELUXE"
train$Name[28] = "LONG JOHN"
train$Name[29] = "WHITE HEATHER"
train$Name[30] = "TEACHER'S HIGHLAND CREAM"
train$Name[31] = "CATTO (8 Year)"
train$Name[32] = "HAIG & HAIG FIVE STAR"
train$Name[33] = "DEWAR'S WHITE LABEL"
train$Name[34] = "J & B"
train$Name[35] = "GRANTS (8 year)"
train$Name[36] = "PETER DAWSON OLD CURIO"
train$Name[37] = "THE ANTIQUARY"
train$Name[38] = "MACKIES ANCIENT"
train$Name[39] = "BARRISTER'S CHOICE (12 Year)"
train$Name[40] = "BERRY BROS. ST. JAMES"
train$Name[41] = "CATTO (12 Years)"
train$Name[42] = "S.S PIERCE'S LIQUEUR (12 Year)"
train$Name[43] = "DEWAR'S ANCESTOR"
train$Name[44] = "JOHNNIE WALKER BLACK LABEL"
train$Name[45] = "KINGS RANSOM"
train$Name[46] = "BUCHANAN DE LUXE"
train$Name[47] = "HIGHLAND NECTAR"
train$Name[48] = "BELL'S ROYAL VAT (12 Years)"
train$Name[49] = "CHIVAS REGAL (12 Years)"
train$Name[50] = "HAIG & HAIG PINCH BOTTLE"
train$Name[51] = "MARTIN'S DE LUXE (12 Years)"
train$Name[52] = "AMBASSADOR (12 Years)"
train$Name[53] = "OLD RARITY"
train$Name[54] = "ROBERTSON'S B.E.B."
train$Name[55] = "GRANTS (12 Years)"
train$Name[56] = "SMITH GLENLIVET (12 Years)"
train$Name[57] = "BALLANTINE (17 Years)"
train$Name[58] = "GRANT'S (20 Years)"
train$Name[59] = "AMBASSADOR (20 Years)"
train$Name[60] = "BALLANTINE (30 Years)"

#change prices to price 1
train = combined
train$price1[1] = "5.75"
train$price1[2] = "5.77"
train$price1[3] = "5.79"
train$price1[4] = "5.79"
train$price1[5] = "5.80"
#skip king george 
train$price1[6] = "5.85"
train$price1[7] = "5.98"
train$price1[8] = "5.99"
train$price1[9] = "5.99"
train$price1[10] = "6.00"
train$price1[11] = "6.20"
train$price1[12] = "6.25"
train$price1[13] = "6.29"
train$price1[14] = "6.29"
train$price1[15] = "6.30"
train$price1[16] = "6.44"
train$price1[17] = "6.37"
train$price1[18] = "6.40"
train$price1[19] = "6.45"
train$price1[20] = "6.45"
train$price1[21] = "6.45"
#old smuggler is skipped
train$price1[22] = "6.47"
train$price1[23] = "6.47"
train$price1[24] = "6.47"
train$price1[25] = "6.49"
train$price1[26] = "6.54"
train$price1[27] = "6.55"
train$price1[28] = "6.55"
train$price1[29] = "6.55"
train$price1[30] = "6.60"
train$price1[31] = "6.60"
train$price1[32] = "6.60"
train$price1[33] = "6.65"
train$price1[34] = "6.66"
train$price1[35] = "6.96"
train$price1[36] = "7.59"
train$price1[37] = "6.99"
train$price1[38] = "7.73"
train$price1[39] = "7.75"
train$price1[40] = "7.80"
train$price1[41] = "7.95"
train$price1[42] = "7.99"
train$price1[43] = "7.99"
train$price1[44] = "8.22"
train$price1[45] = "8.30"
train$price1[46] = "8.49"
train$price1[47] = "8.50"
train$price1[48] = "8.60"
train$price1[49] = "8.60"
train$price1[50] = "8.60"
train$price1[51] = "8.60"
train$price1[52] = "8.69"
train$price1[53] = "8.69"
train$price1[54] = "8.74"
train$price1[55] = "8.91"
train$price1[56] = "10.79"
train$price1[57] = "12.00"
train$price1[58] = "12.95"
train$price1[59] = "13.85"
train$price1[60] = "15.45"

#change prices to price 2
train = combined
#barrister's choice skipped
train$price2[1] = "65.66"
train$price2[2] = "65.78"
train$price2[3] = "66.00"
train$price2[4] = "66.01"
train$price2[5] = "66.12"
#skip king george 
train$price2[6] = "66.69"
train$price2[7] = "68.18"
train$price2[8] = "68.50"
train$price2[9] = "68.30"
train$price2[10] = "68.40"
train$price2[11] = "70.50"
train$price2[12] = "71.25"
train$price2[13] = "71.70"
train$price2[14] = "71.71"
train$price2[15] = "71.82"
train$price2[16] = "73.42"
train$price2[17] = "72.60"
train$price2[18] = "72.96"
train$price2[19] = "73.53"
train$price2[20] = "73.53"
train$price2[21] = "73.53"
#old smuggler is skipped
train$price2[22] = "73.86"
train$price2[23] = "73.87"
train$price2[24] = "73.76"
train$price2[25] = "73.99"
train$price2[26] = "74.56"
train$price2[27] = "74.67"
train$price2[28] = "74.67"
train$price2[29] = "74.67"
train$price2[30] = "75.74"
train$price2[31] = "75.00"
train$price2[32] = "75.24"
train$price2[33] = "75.81"
train$price2[34] = "75.92"
train$price2[35] = "79.34"
train$price2[36] = "86.50"
train$price2[37] = "79.60"
train$price2[38] = "88.16"
train$price2[39] = "88.00"
train$price2[40] = "88.92"
train$price2[41] = "90.00"
train$price2[42] = "91.09"
train$price2[43] = "91.09"
train$price2[44] = "93.71"
train$price2[45] = "94.62"
train$price2[46] = "97.00"
train$price2[47] = "96.90"
train$price2[48] = "98.04"
train$price2[49] = "98.04"
train$price2[50] = "98.04"
train$price2[51] = "98.04"
train$price2[52] = "99.06"
train$price2[53] = "99.07"
train$price2[54] = "99.64"
train$price2[55] = "101.58"
train$price2[56] = "122.88"
train$price2[57] = "136.83"
train$price2[58] = "147.63"
train$price2[59] = "157.90"
train$price2[60] = "176.18"



