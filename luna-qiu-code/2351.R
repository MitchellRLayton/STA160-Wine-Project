data = readRDS("FullBoxes.rds")
doc.2351 = data$UCD_Lehmann_2351.jpg
#left bottom right top text confidence

doc.2351$text = gsub('\\.', '', doc.2351$text)
doc.2351$text = gsub('\\:', '', doc.2351$text)
doc.2351$text = gsub('\\\\', '', doc.2351$text)
doc.2351$text = gsub('\\|', '', doc.2351$text)
doc.2351$text = gsub('\\~', '', doc.2351$text)
doc.2351$text = gsub('\\#', '', doc.2351$text)
doc.2351$text = gsub('\\,', '', doc.2351$text)
doc.2351$text = gsub('\\-', '', doc.2351$text)
doc.2351$text = gsub('\\®', '', doc.2351$text)
doc.2351$text = gsub('\\}', '', doc.2351$text)
doc.2351$text = gsub('\\!', '', doc.2351$text)
doc.2351$text = gsub('\\£', '', doc.2351$text)
doc.2351$text = gsub('\\¥', '', doc.2351$text)
doc.2351$text = gsub('\\+', '', doc.2351$text)
doc.2351$text = gsub('\\=', '', doc.2351$text)
doc.2351$text = gsub('\\©', '', doc.2351$text)
doc.2351$text = gsub('\\>', '', doc.2351$text)
doc.2351$text = gsub('\\—', '', doc.2351$text)

doc.2351 = doc.2351[!(doc.2351$text ==""),]

left = subset(doc.2351, left < 1700 & bottom > 1300 & bottom < 3000)
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 60) {
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

stringsplit = stringsplit[lapply(stringsplit,length)>4]
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
  first_true[is.na(first_true)] = 0
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
  if(length(stringsplit[[i]]) > mode(p)) {
    name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - mode(p))]
    name[[i]] = paste(name[[i]], collapse = " ")
    price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - mode(p)):length(stringsplit[[i]])]
    price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]])
  }
  else {
    name[[i]] = stringsplit[[i]]
    price[[i]] = 0
  }
}

database.left = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.left$Name = name

for(i in 1:length(stringsplit)) {
  database.left$price1[i] = price[[i]][1]
  database.left$price2[i] = price[[i]][2]
}

database.left = na.omit(database.left)








right = subset(doc.2351, left > 1700 & bottom > 1300 & bottom < 3000)
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 30) {
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

stringsplit = stringsplit[lapply(stringsplit,length)>4]
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
  first_true[is.na(first_true)] = 0
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
  if(length(stringsplit[[i]]) > mode(p)) {
  name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - mode(p) + 1)]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - mode(p) + 1):length(stringsplit[[i]])]
  price[[i]] = sub("([[:digit:]]{2,2})$", ".\\1", price[[i]]) 
  }
  else {
    name[[i]] = stringsplit[[i]]
    price[[i]] = 0
  }
}

database.right = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.right$Name = name

for(i in 1:length(stringsplit)) {
  database.right$price1[i] = price[[i]][1]
  database.right$price2[i] = price[[i]][2]
}

database.right = na.omit(database.right)




train.left = database.left
train.right = database.right

train.right$Name[1:4] = c("557 LIEBFRAUMILCH 1958, Lob",
                          "526 PIESPORTER PICHTER SPATLES 1958",
                          "582 LIEBFRAUMILCH SPATLESE 1958",
                          "559 LIEBFRAUMILCH SPATLESE 1959") 
train.right$price2[4:5] = c("29.95", "24.65")
train.right$price1[1:4] = c(".75", "1.09", "1.09", "1,35")
train.right$Name[9:10] = c("307 MEURSAULT, Clos des Perrieres, 1957",
                           "493 PROVENCE ROSE 1959")

train.left$Name = c("183 MEDOC 1957, Boyer", "485 CHATEAU RIPEAU 1955", "235 CHATEAU LAFRON ROCHET 1955",
                    "110 CHATEAU CHASSE SPLEEN 1955", "634 CHATEAU FIGAEC 1953", "626 CHATEAU LEOVILLE BARTON 1955", 
                    "132 CHATEAU BRANAIRE DUCRU 1953", "604 CHATEAU LATOUR 1950", "163 CHATEAU CHEVAL BLANC 1950",
                    "579 CHATEAU LATOUR 1948", "207 CHATEAU MOUTON ROTHSCHILD 1955", "589 CHATEAU LATOUR 1949",
                    "417 BEAUJOLAIS RINGUET 1959", "303 SAVIGNY LAVIERES 1957", "317 MOULIN A VENT 1959",
                    "319 NUITS ST. GEORGES, VAUCRAINS, 1957", "278 CLOS DE VOUGEOT 1957", "370 TAVEL 1957, Buffiere",
                    "441 BONNE AUBERGE ROSE 1959")

train.left$price1 = c(".74",".95",".99",".99","1.09","1.09","1.35","1.55","1.85","2.09","2.75","3.35",
                      ".89","1.09","1.19","1.85","2.35",".84",".84")

train.left$price2 = c("16.50","20.50","21.95","21.95","24.65","24.65","29.95","36.50","41.75",
                      "48.75","61.00","73.50","19.95","24.65","26.50","41.75","51.75","18.95","18.95")

#left
adist(paste(train.left$Name, collapse = " "), paste(database.left$Name, collapse = " "))
length(strsplit(paste(train.left$Name, collapse = " "), "")[[1]])

adist(paste(train.left$price1, collapse = " "), paste(database.left$price1, collapse = " "))
length(strsplit(paste(train.left$price1, collapse = " "), "")[[1]])

adist(paste(train.left$price2, collapse = " "), paste(database.left$price2, collapse = " "))
length(strsplit(paste(train.left$price2, collapse = " "), "")[[1]])


#right
adist(paste(train.right$Name, collapse = " "), paste(database.right$Name, collapse = " "))
length(strsplit(paste(train.right$Name, collapse = " "), "")[[1]])

adist(paste(train.right$price1, collapse = " "), paste(database.right$price1, collapse = " "))
length(strsplit(paste(train.right$price1, collapse = " "), "")[[1]])

adist(paste(train.right$price2, collapse = " "), paste(database.right$price2, collapse = " "))
length(strsplit(paste(train.right$price2, collapse = " "), "")[[1]])









