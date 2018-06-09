data = readRDS("FullBoxes.rds")
doc.2635 = data$UCD_Lehmann_2635.jpg
#left bottom right top text confidence

doc.2635$text = gsub('\\.', '', doc.2635$text)
doc.2635$text = gsub('\\:', '', doc.2635$text)
doc.2635$text = gsub('\\\\', '', doc.2635$text)
doc.2635$text = gsub('\\|', '', doc.2635$text)
doc.2635$text = gsub('\\~', '', doc.2635$text)
doc.2635$text = gsub('\\#', '', doc.2635$text)
doc.2635$text = gsub('\\,', '', doc.2635$text)
doc.2635$text = gsub('\\-', '', doc.2635$text)
doc.2635$text = gsub('\\®', '', doc.2635$text)
doc.2635$text = gsub('\\}', '', doc.2635$text)
doc.2635$text = gsub('\\!', '', doc.2635$text)
doc.2635$text = gsub('\\£', '', doc.2635$text)
doc.2635$text = gsub('\\¥', '', doc.2635$text)
doc.2635$text = gsub('\\+', '', doc.2635$text)
doc.2635$text = gsub('\\=', '', doc.2635$text)
doc.2635$text = gsub('\\©', '', doc.2635$text)
doc.2635$text = gsub('\\>', '', doc.2635$text)
doc.2635$text = gsub('\\?', '', doc.2635$text)
doc.2635$text = gsub('\\>', '', doc.2635$text)
doc.2635$text = gsub('\\§', '', doc.2635$text)
doc.2635$text = gsub('\\/', '', doc.2635$text)

doc.2635 = doc.2635[!(doc.2635$text ==""),]
doc.2635 = doc.2635[doc.2635$left > 0, ]

left = subset(doc.2635, bottom > 3460& bottom < 4480 & left < 1700)
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 39) {
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








right = subset(doc.2635, bottom > 3500 & left > 1700)
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 39) {
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

output = rbind(database.left,database.right)
train = output

#change names

#skip first wine in left column
train$Name[1] = "FORTNUM & MASON THREE STAR"
train$Name[2] = "WILDMAN'S VERITABLE"
train$Name[3] = "BIQUIT THREE STAR"
train$Name[4] = "HINE FIVE STAR"
train$Name[5] = "MONNET THREE STAR"
train$Name[6] = "COURVOISER THREE STAR"
train$Name[7] = "MARTELL THREE STAR"
train$Name[8] = "HENNESSY THREE STAR"
train$Name[9] = "DENIS MOUNIE KING EDWARD"
train$Name[10] = "J & B V.S.O.P."
train$Name[11] = "J. ROBIN GRAND RESERVE"
train$Name[12] = "COURVOISER V.S.O.P."
train$Name[13] = "BISQUIT V.S.E.P."
train$Name[14] = "BRITISH MARKET 20 YEAR OLD UNBLENDED"
train$Name[15] = "REMY MARTIN V.S.O.P."
train$Name[16] = "HENNESSY V.S.O.P."
train$Name[17] = "MARTELL V.S.O.P."
train$Name[18] = "HINE IMPERIALE"
#skips all until monnet anniversaire
train$Name[19] = "MONNET ANNIVERSAIRE"
train$Name[20] = "MARTELL CORDON BLEU"
train$Name[21] = "COURVIOSIER NAPOLEON"
train$Name[22] = "HINE TRIOMPHE"
train$Name[23] = "HENNESSY X.O."
train$Name[24] = "BRIAND GRAND FINE CHAMPAGNE"
train$Name[25] = "MARTELL ARGENT"
train$Name[26] = "MARTELL EXTRA"
train$Name[27] = "HENNESSY EXTRA"
train$Name[28] = "COURVOISIER SPECIAL GRANDE FINE CHAMPAGNE"
train$Name[29] = "MARTELL ARGENT IN BACCARAT"
train$Name[30] = "REMY MARTIN LOUIS XIII IN BACCARAT"
train$Name[31] = "COURVOISER V.S.O.P. IN BACCARAT"
train$Name[32] = "S.S. PIERCE 2 STAR (U.S.)"
train$Name[33] = "CHRISTIAN BROS. (U.S.)"
#skip bellow's calvados
train$Name[34] = "DOMECQ FUNDADOR (Spain)"
train$Name[35] = "ASBACH URALT (Germnay)"
#skip marc de champagne
train$Name[36] = "METAX SEVEN STAR (Greece)"
train$Name[37] = "DOMECQ CARLOS I (Spain)"

#change price1
#skip DENIS MOUNIE GOLD LEAF
train$price1[1] = "6.49"
train$price1[2] = "7.45"
train$price1[3] = "7.30"
train$price1[4] = "7.36"
train$price1[5] = "7.54"
train$price1[6] = "7.57"
train$price1[7] = "7.58"
train$price1[8] = "7.58"
train$price1[9] = "8.27"
train$price1[10] = "8.35"
train$price1[11] = "8.45"
train$price1[12] = "8.67"
train$price1[13] = "8.71"
train$price1[14] = "8.95"
train$price1[15] = "8.98"
train$price1[16] = "9.30"
train$price1[17] = "9.36"
train$price1[18] = "9.50"
#skips all until monnet anniversaire
train$price1[19] = "10.95"
train$price1[20] = "11.27"
train$price1[21] = "11.48"
train$price1[22] = "12.13"
train$price1[23] = "14.25"
train$price1[24] = "14.75"
train$price1[25] = "17.39"
train$price1[26] = "22.44"
train$price1[27] = "22.95"
train$price1[28] = "24.75"
train$price1[29] = "46.25"
train$price1[30] = "46.50"
train$price1[31] = "25.93"
train$price1[32] = "4.70"
train$price1[33] = "4.99"
#skip bellow's calvados
train$price1[34] = "6.64"
train$price1[35] = "7.19"
#skip marc de champagne
train$price1[36] = "9.12"
train$price1[37] = "15.01"

#change price2
#skip first wine in left column
train$price2[1] = "75.00"
train$price2[2] = "85.00"
train$price2[3] = "83.17"
train$price2[4] = "83.90"
train$price2[5] = "85.96"
train$price2[6] = "86.30"
train$price2[7] = "86.41"
train$price2[8] = "86.41"
train$price2[9] = "91.30"
train$price2[10] = "94.24"
train$price2[11] = "96.30"
train$price2[12] = "98.94"
train$price2[13] = "99.26"
train$price2[14] = "104.00"
train$price2[15] = "102.37"
train$price2[16] = "106.06"
train$price2[17] = "106.70"
train$price2[18] = "108.30"
#skips all until monnet anniversaire
train$price2[19] = "124.83"
train$price2[20] = "128.45"
train$price2[21] = "130.87"
train$price2[22] = "138.27"
train$price2[23] = "162.45"
train$price2[24] = "165.00"
train$price2[25] = "198.22"
train$price2[26] = "255.82"
train$price2[27] = "261.64"
train$price2[28] = "275.00"
train$price2[29] = "-"
train$price2[30] = "-"
train$price2[31] = "="
train$price2[32] = "53.58"
train$price2[33] = "56.89"
#skip bellow's calvados
train$price2[34] = "75.70"
train$price2[35] = "78.37"
#skip marc de champagne
train$price2[36] = "104.40"
train$price2[37] = "-"




adist(paste(train$Name, collapse = " "), paste(output$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(output$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(output$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])

