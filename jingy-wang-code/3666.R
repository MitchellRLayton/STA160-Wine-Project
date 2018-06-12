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

train = database
train$Name = unlist(train$Name)
train = train[-c(1,2,4,5,7,8,10,11,12,16,17,20,21,
                 28,31,32,34,37,39,41,43,47,49,50),]

train$Name[1] = "624 ERBACHER KOHLIG SPATLRESE CABINET, Reinhartshausen"
train$price1[1] = "3.99"
train$price2[1] = "43.10"
train$Name[2] = "585 STEINBERGER CABINET SPATLESE, Staatsdomaine"
train$Name[3] = "721 RAUENTHALER GEHRN SPATLESE CABINET, Staatsdomaine"
train$price1[3] = "4.49"
train$price2[3] = "48.50"
train$Name[4] = "846 SCHLOSS VOLLRADS KABINETT, Graf Mtuschka-Greiffenclau"
train$Name[6] = "835 SCHLOSS JOHANNISBEREG ROSALACK FEINE SPATLESE von Metternich"
train$price1[6] = "6.99"
train$price2[6] = "75.50"
train$Name[7] = "562 FORSTER JESUITENGARTEN, Bassermann-Jordan"
train$Name[8] = "587 FORSTER JESUITENGARTEN SPATLESE, Bassermann-Jordan"
train$price1[8] = "3.99"
train$price2[8] = "43.95"
train$Name[9] = "551 MOSELBLUMCHEN, Kendermann"
train$Name[16] = "599 BERNCASTELER SCHLOSSBERG SPATLESE, Dr.H.Thannisch"
train$price1[16] = "3.49"
train$price2[16] = "37.70"
train$price2[19] = "37.70"
train$Name[17] = "592 BRAUNEBERGER JUFFER SPATLESE von Schorlemer"
train$Name[18] = "602 MAXIMIN GRUENHAUSER HERRENBERG ABTSBERG, von Schubert"
train$Name[19] = "632 WEHLENER SONNENUHR, Joh.Hos.Prum"
train$Name[20] = "628 PIESPORTER GOLDTROPFCHEN SPATLESE, Bischoefliches Konvikt"
train$price1[20] = "3.99"
train$price2[20] = "43.10"
train$Name[21] = "618 MAXIMIN GRUENHAUSER HERRENBERG SPATLESE, von Schubert"
train$price1[21] = "4.25"
train$price2[21] = "45.90"
train$Name[22] = "725 PIESPORTER GOLDTROPFCHEN AUSLESE, Bischoefliches Konvikt"
train$price1[22] = "4.75" 
train$price2[22] = "51.30"
train$price2[23] = "59.30"
train$price1[24] = "5.75"
train$price2[24] = "62.10"
train$Name[25] = "590 BERNCASTELER DOCTOR SPATLESE CABINET, Dr.H.Thanisch"
train$price1[25] = "6.49"
train$price2[25] = "70.10"
train$Name[26] = "714 BERNCASTELER DOCTOR AUSLESE, Dr.H.Thanisch"
train$price1[26] = "8.99"
train$price2[26] = "97.20"

write.table(train, file = "3666.txt")
