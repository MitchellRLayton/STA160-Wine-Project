data = readRDS("FullBoxes.rds")
doc.3392 = data$UCD_Lehmann_3392.jpg
#left bottom right top text confidence

doc.3392$text = gsub('\\.', '', doc.3392$text)
doc.3392$text = gsub('\\:', '', doc.3392$text)
doc.3392$text = gsub('\\\\', '', doc.3392$text)
doc.3392$text = gsub('\\|', '', doc.3392$text)
doc.3392$text = gsub('\\~', '', doc.3392$text)
doc.3392$text = gsub('\\#', '', doc.3392$text)
doc.3392$text = gsub('\\,', '', doc.3392$text)
doc.3392$text = gsub('\\-', '', doc.3392$text)
doc.3392$text = gsub('\\®', '', doc.3392$text)
doc.3392$text = gsub('\\}', '', doc.3392$text)
doc.3392$text = gsub('\\!', '', doc.3392$text)
doc.3392$text = gsub('\\£', '', doc.3392$text)
doc.3392$text = gsub('\\¥', '', doc.3392$text)
doc.3392$text = gsub('\\+', '', doc.3392$text)
doc.3392$text = gsub('\\=', '', doc.3392$text)
doc.3392$text = gsub('\\©', '', doc.3392$text)
doc.3392$text = gsub('\\>', '', doc.3392$text)
doc.3392$text = gsub('\\?', '', doc.3392$text)
doc.3392$text = gsub('\\>', '', doc.3392$text)
doc.3392$text = gsub('\\§', '', doc.3392$text)
doc.3392$text = gsub('\\/', '', doc.3392$text)
doc.3392$text = gsub('\\—', '', doc.3392$text)
doc.3392$text = gsub('\\%', '', doc.3392$text)
doc.3392$text = gsub('\\]', '', doc.3392$text)

doc.3392 = doc.3392[!(doc.3392$text ==""),]
doc.3392 = doc.3392[doc.3392$left > 0, ]

table = subset(doc.3392, left > 100)
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





#database$Name <- unlist(database$Name, use.names = FALSE)

train = database
train = train[-c(1,2,5,8,12,21,25),]
train$Name[2] = "614 SOAVE 1961 (Cantina Sociale) White"
train$Name[3] = "619 TYROLEAN RED AUSLESE 1963"
train$price1[3] = "1.89"
train$price2[3] = "20.40"
train$Name[4] = "629 TYROLEAN WHITE AUSLESE 1963"
train$Name[5] = "1205 GRAHAM'S WESTMINSTER TAWNY PORT"
train$price1[8] = "1.59"
train$Name[9] = "821 MEURSAULT(Estate bottled, Joseph Matrot)"
train$price2[9] = "28.00"
train$Name[10] = "345 POUILLY FUISSE(Estate bottled, Plumet)"
train$Name[11] = "310 CHASSAGNE MONTRACHET, LES RUCHOTTES(Claude Ramonet)"
train$price2[12] = "48.50"
train$Name[13] = "344 MEURSAULT, CLOS DES PERRIERES(Estate bottled, A.Grivault)"
train$price1[13] = "4.59"
train$price2[13] = "49.55"
train$Name[14] = "369 LE MONTRACHET(Estate bottled, Milan-Mathey)"
train$price2[14] = "91.80"
train$Name[15] = "390 MUSIGNY BLANC(Estate bottled, Comte de Vogue)"
train$Name[16] = "422 CHATEAU DE CHILLON 1962"
train$Name[17] = "408 MON SOLEIL(Lake Geneva White)"
train$Name[18] = "427 DIAMANT ROSE(Lake Geneva Rose)"
train$price1[18] = "1.99"
train$price2[18] = "22.95"
train$Name[20] = "241 DOMAINE DE CHEVALIER 1961(Graves)"
train$price1[20] = "4.49"
train$price2[20] = "48.50"
train$Name[21] = "220 CHATEAU HAUT BRION BLANC 1960(Graves)"
train$price2[21] = "53.90"
train$price1[22] = "7.49"


adist(paste(train$Name, collapse = " "), paste(database$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(database$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(database$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])




