data = readRDS("FullBoxes.rds")
doc.0208 = data$UCD_Lehmann_0208.jpg
#left bottom right top text confidence

bottom = subset(doc.0208, left > 2400 & bottom > 3000 & bottom < 5100)
bottom$bottom.diff = ave(bottom$bottom, FUN=function(x) c(0,diff(x)))

bottom$text = gsub('\\_', '', bottom$text)
bottom$text = gsub('\\.', '', bottom$text)
bottom$text = gsub('\\{', '', bottom$text)
bottom$text = gsub('\\\\', '', bottom$text)
bottom$text = gsub('\\£', '', bottom$text)
bottom$text = gsub('\\=', '', bottom$text)
bottom$text = gsub('\\:', '', bottom$text)
bottom$text = gsub('\\§', '', bottom$text)
bottom$text = gsub('\\;', '', bottom$text)
bottom$text = gsub('\\|', '', bottom$text)

#doc.0237 = doc.0237[!doc.0237$text == "",]

bottom$index = 0
t = 1
bottom$index[1] = t
for (i in 1:nrow(bottom)) {
  bottom$index[i] = t
  if (bottom$bottom.diff[i] > 40) {
    t = t+1
    bottom$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(bottom$index)) {
  splitted[[i]] = bottom[bottom$index == i, ]
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

frame

#After extracting each line, put info in dataframe

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
if(grepled[[i]][1] == TRUE && grepled[[i]][l[i]] == FALSE) {
  stringsplit[[i]] = rev(stringsplit[[i]])
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
train$Name = c("CONSTANTINO TAWNY","SANDEMAN ONE STAR RUBY","COCKBURN No. 25",
               "J & B REGENT'S TAWNY","HARVEY'S GOLD CAP","SANDEMAN THREE STAR TAWNY",
               "ROBERTSON'S DRY HUMOR","SANDEMAN PARTNER'S","HARVEY'S DIRECTOR'S BIN",
               "COCKBURN CREAM","DOMECQ GUITAR","BOBADILLA VICTORIA","DUFF GORDON NINA","DOMECQ DOUBLE CENTURY",
               "JUSTERINI & BROOKS AMONTILLADO","DUFF GORDON NO.28","SANDEMAN THREE STAR AMONTILLADO",
               "WILLIAM'S & HUMBERT AMONTILLADO","DOMECQ LA INA (Dry, fine)","HARVEY'S AMONTILLADO",
               "FORTNUM & MASON AMONTILLADO","GONZALEZ & BYPASS TIO PEPE","WILLIAM'S & HUMBERT DRY SACK","BOBADILLA CREAM",
               "J & B PALL MALL CREAM","HARVEY'S BRISTOL MILK","HARVEY'S BRISTOL DRY","HARVEY'S BRISTOL CREAM","SANDEMAN AMBROSETTE (Rare,old)")

#skip HARVEY'S HUNTING

train$price1 = c("2.83","3.20","3.25","3.28","3.35","3.60","3.69","4.73","6.71",
                 "6.98","2.34","2.78","2.89","2.99","3.03","3.19","3.20","3.27","3.39",
                 "3.42","3.49","3.88","4.85","4.85","5.88","5.97","6.01","6.71","7.67")

train$price2 = c("32.30","34.56","35.10","35.39","38.88","39.85","48.06","51.08","80.52",
                 "75.40","25.27","31.69","31.21","32.29","32.69","34.45","34.56","35.32","36.61",
                 "36.94","39.75","42.29","52.38","55.29","63.50","64.48","64.91","80.52","82.84")

adist(paste(train$Name, collapse = " "), paste(database$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(database$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(database$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])




