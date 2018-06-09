data = readRDS("FullBoxes.rds")
doc.1198 = data$UCD_Lehmann_1198.jpg
#left bottom right top text confidence

doc.1198$text = gsub('\\.', '', doc.1198$text)
doc.1198$text = gsub('\\:', '', doc.1198$text)
doc.1198$text = gsub('\\\\', '', doc.1198$text)
doc.1198$text = gsub('\\|', '', doc.1198$text)
doc.1198$text = gsub('\\~', '', doc.1198$text)
doc.1198$text = gsub('\\#', '', doc.1198$text)
doc.1198$text = gsub('\\,', '', doc.1198$text)
doc.1198$text = gsub('\\-', '', doc.1198$text)
doc.1198$text = gsub('\\®', '', doc.1198$text)
doc.1198$text = gsub('\\}', '', doc.1198$text)
doc.1198$text = gsub('\\!', '', doc.1198$text)

doc.1198 = doc.1198[!(doc.1198$text ==""),]

table = subset(doc.1198, bottom < 3900 & bottom > 1900)
table$bottom.diff = ave(table$bottom, FUN=function(x) c(0,diff(x)))

table$index = 0
t = 1
table$index[1] = t
for (i in 1:nrow(table)) {
  table$index[i] = t
  if (table$bottom.diff[i] > 60) {
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
  first_true[i] = which(grepled[[i]] == TRUE)[3]
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

database = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database$Name = name

for(i in 1:length(stringsplit)) {
  database$price1[i] = price[[i]][1]
  database$price2[i] = price[[i]][2]
}

database = na.omit(database)

train = database

train$Name = c("793 BEAUJOLAIS, 1966, Louis Latour","W-247 ALOXE CORTON LES CHAILLOTS, 1964, Estate-bottled, Louis Latour",
               "W-254 CORTON, CHATEAU GRANCEY, 1964, Estate-bottled, Louis Latour","446 GRANDS ECHEZEAUX, 1964, Estate-bottled, Domaine de la Romanee Conti",
               "747 BOURGOGNE BLANC, 1964, Estate-bottled, Domaine Leflaive","339 POUILLY FUISSE, 1966, Louis Latour",
               "740 CHABLIS FOURCHAUME, 1966, Estate-bottled, J. Moreau","751 MEURSAULT GENEVRIERES, 1964, Estate-bottled, Ropiteau-Mignon",
               "427 PAVILLON ROUGE, 1965, Wildman","762 CHATEAUNEUF DU PAPE, LA BERNADINE, 1966",
               "573 COTE ROTIE, 1964, Estate-bottled, Chapoutier","500 ROSE MAISON, 1966, Estate-bottled, Chapoutier",
               "406 TAVEL, 1966, Wildman","515 LIEBFRAUMILCH MEISTERKRONE, 1966, Langenbach","546 BERNCASTELER MEISTERKRONE, 1966, Langenbach")

train$price1 = c("2.78","4.63","6.99","13.20","3.25","3.38","4.17","5.95","2.13","3.33","3.85","2.59","2.87","2.18","2.50")

train$price2 = c("30.00","50.00","75.50","142.50","35.00","36.50","45.00","64.00","23.00","36.00","41.60","27.97","31.00","23.50","27.00")

adist(paste(train$Name, collapse = " "), paste(database$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(database$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(database$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])






