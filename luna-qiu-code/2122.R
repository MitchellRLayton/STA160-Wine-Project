data = readRDS("FullBoxes.rds")
doc.2122 = data$UCD_Lehmann_2122.jpg
#left bottom right top text confidence

doc.2122$text = gsub('\\.', '', doc.2122$text)
doc.2122$text = gsub('\\:', '', doc.2122$text)
doc.2122$text = gsub('\\\\', '', doc.2122$text)
doc.2122$text = gsub('\\|', '', doc.2122$text)
doc.2122$text = gsub('\\~', '', doc.2122$text)
doc.2122$text = gsub('\\#', '', doc.2122$text)
doc.2122$text = gsub('\\,', '', doc.2122$text)
doc.2122$text = gsub('\\-', '', doc.2122$text)
doc.2122$text = gsub('\\®', '', doc.2122$text)
doc.2122$text = gsub('\\}', '', doc.2122$text)
doc.2122$text = gsub('\\!', '', doc.2122$text)
doc.2122$text = gsub('\\=', '', doc.2122$text)
doc.2122$text = gsub('\\]', '', doc.2122$text)
doc.2122$text = gsub('\\>', '', doc.2122$text)
doc.2122$text = gsub('\\/', '', doc.2122$text)
doc.2122$text = gsub('\\+', '', doc.2122$text)

doc.2122 = doc.2122[!(doc.2122$text ==""),]

left = subset(doc.2122, left < 1700)
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 45) {
    t = t+1
    left$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(left$index)) {
  splitted[[i]] = left[left$index == i, ]
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



left.2 = subset(doc.2122, left > 1700 & left < max(left)/2 & bottom < 1625)
left.2$bottom.diff = ave(left.2$bottom, FUN=function(x) c(0,diff(x)))

left.2$index = 0
t = 1
left.2$index[1] = t
for (i in 1:nrow(left.2)) {
  left.2$index[i] = t
  if (left.2$bottom.diff[i] > 45) {
    t = t+1
    left.2$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(left.2$index)) {
  splitted[[i]] = left.2[left.2$index == i, ]
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

database.left.2 = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.left.2$Name = name

for(i in 1:length(stringsplit)) {
  database.left.2$price1[i] = price[[i]][1]
  database.left.2$price2[i] = price[[i]][2]
}

database.left.2 = na.omit(database.left.2)



left.3 = subset(doc.2122, left > 1700 & left < max(left)/2 & bottom > 3200)
left.3$bottom.diff = ave(left.3$bottom, FUN=function(x) c(0,diff(x)))

left.3$index = 0
t = 1
left.3$index[1] = t
for (i in 1:nrow(left.3)) {
  left.3$index[i] = t
  if (left.3$bottom.diff[i] > 45) {
    t = t+1
    left.3$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(left.3$index)) {
  splitted[[i]] = left.3[left.3$index == i, ]
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

database.left.3 = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.left.3$Name = name

for(i in 1:length(stringsplit)) {
  database.left.3$price1[i] = price[[i]][1]
  database.left.3$price2[i] = price[[i]][2]
}

database.left.3 = na.omit(database.left.3)






right = subset(doc.2122, left > max(left)/2 & left < 4000 & bottom > 1150)
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 25) {
    t = t+1
    right$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(right$index)) {
  splitted[[i]] = right[right$index == i, ]
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
}

database.right = na.omit(database.right)




right.2 = subset(doc.2122, left > 4000 & bottom > 1150)
right.2$bottom.diff = ave(right.2$bottom, FUN=function(x) c(0,diff(x)))

right.2$index = 0
t = 1
right.2$index[1] = t
for (i in 1:nrow(right.2)) {
  right.2$index[i] = t
  if (right.2$bottom.diff[i] > 25) {
    t = t+1
    right.2$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(right.2$index)) {
  splitted[[i]] = right.2[right.2$index == i, ]
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

database.right.2 = data.frame("Name" = numeric(length(name)), "price1" = numeric(length(price)), "price2" = numeric(length(price)))
database.right.2$Name = name

for(i in 1:length(stringsplit)) {
  database.right.2$price1[i] = price[[i]][1]
}

database.right.2 = na.omit(database.right.2)






train.left = database.left
train.left.2 = database.left.2
train.left.3 = database.left.3
train.right = database.right
train.right.2 = database.right.2

train.left$Name[4] = "Domecq Guitar"
train.left$Name[12] = "Pando Amontillado"
train.left$Name[13] = "J & B Amontillado"
train.left$Name[14] = "Domecq La Ina"
train.left$Name[15] = "Harvey's Amontillado"
train.left$Name[20] = "Dry Sack"
train.left$Name[22] = "Bobadilla Cream"
train.left$Name[23] = "Harvey's Bristol Milk"
train.left$Name[25] = "Berry Bros. Finest Cream"
train.left$Name[27] = "Sandeman Royal Esmeralda"

train.left$price1[4] = "1.99"
train.left$price2[4] = "21.50"
train.left$price1[27] = "15.11"
train.left$price2[20] = "48.60"
train.left$price2[22] = "55.29"

train.left$Name[44] = "Carpano (Italy)"
train.left$Name[43] = "Martini & Rossi (Italy)"
train.left$Name[42] = "Cinzano (Italy)"
train.left$price1[42] = "2.08"
train.left$price2[42] = "22.38"
train.left$Name[41] = "Tribuno (U.S.)"
train.left$price1[41] = "1.24"
train.left$price2[41] = "13.42"
train.left$Name[40] = "Lejon (U.S.)"
train.left$price1[40] = "1.19"
train.left$Name[39] = "S.S.Pierce (U.S.)"

train.left$Name[38] = "Noilly Prat (France)"
train.left$Name[37] = "Cinzano (France)"
train.left$Name[28:34] = c("S.S.Pierce (U.S.)", "Miramar (U.S.)",
                           "Lejon (U.S.)", "Nob Hill (U.S.)",
                           "Tribuno (U.S.)", "Heublein (U.S.)",
                           "Clarac (France)")
train.left$Name[36] = "B & G (France)"
train.left$price1[28:34] = c("1.15", "1.15", "1.19", "1.25",
                             "1.29", "1.38", "1.49")
train.left$price1[36] = "2.11"
train.left$price2[29:31] = c("11.85", "12.85", "13.75")
train.left$price2[33:34] = c("15.73", "16.09")

train.left.2$Name[2] = "S.S. Piece Carlos"
train.left.2$Name[4] = "Delaforce Specially Selected Medium Tawny"
train.left.2$Name[8] = "Sandeman Three Star Tawny"
train.left.2$Name[11:13] = c("Sandeman 1934 Vintage", "Harvey's Hunting",
                             "Pintos 1890 Vintage")
train.left.2$price1[4] = "2.49"
train.left.2$price2[4] = "28.75"

train.left.3$Name = c("Tribunet (U.S.)", "Kina Rok(France)", 
                      "DUbonnet (U.S.)", "Lillet (France)",
                      "St. Raphael (France)", "Byrrh (France)")
train.left.3$price2[1] = "14.39"
train.left.3$price1[1] = "1.26"
train.left.3$price1[6] = "2.63"

train.right$Name[3:6] = c("Garnier (U.S.)", "Bardinet (U.S.)", 
                          "Apricot by Cointreau (U.S.)", "Dolfi (France)")
train.right$price1[4:6] = c("4.02", "4.08", "6.60")
train.right$Name[8:9] = c("Nuyens (U.S.)", "Garnier (U.S.)")
train.right$price1[8:9] = c("3.75", "3.93")

train.right$Name[11:14] = c("Bols (U.S.)", "Berry Bros(Holland)",
                            "Nuyens (U.S.)", " Dekuyper(U.S.)")
train.right$price1[11] = "4.43"

train.right$Name[16:31] = c("Bardinet(U.S.)", "Cacao by Cointreau(U.S.)",
                            "Zwack White Cacao (Hungary)", "Bols (U.S.)",
                            "Dolfi (France)",
                            "Cherry Marnier (France)", "Cherry Heering (Denmark)",
                            "Nuyens (U.S.)", "Dolfi(France)", "Allash (Germany)",
                            "Gilka (U.S.)", "Hildick Black Label (U.S.)", 
                            "Laird Three Star (U.S.)", 
                            "Calvados, Arc de Triomphe (Normandy)",
                            "Calvados, 25 Year Old (Normandy)",
                            "Calvados, 40 Year Old (Normandy)")
train.right$price1[16:18] = c("4.18", "4.20", "6.49")
train.right$price1[20] = "6.60"
train.right$price1[22:31] = c("7.85", "4.15", "6.60", "6.74"
                              ,"7.30", "4.35", "4.37", "5.79", "7.62", "9.67")

train.right.2$Name = c("Dekuyper (U.S.)", "Nuyens (U.S.)",
                       "Bardinet (U.S.)", "Garnier (U.S.)",
                       "Menthe by Cointreau (U.S.)",
                       "Bols (U.S.)", "Marie Brizard (France)",
                       "Dolfi (France)", "Cusenier Freezomint (France)",
                       "Berry Bros. (Holland)", "Garnier (U.S.)",
                       "Dettling (Swiss)", "Dopff (Alsace)", 
                       "Basler (Switzerland)", "Dolfi (France)",
                       "Bohemian in Decanter", "Amer Picon (France)", 
                       "Benedictine (France)", "B & B (France)",
                       "Bols Advocaat (U.S.)", "Bols Cafe (U.S.)",
                       "Bols Banana (U.S.)", "Cassis, Sarrazin (France)",
                       "COintreau (U.S.)", "Cointreau & Cognac (U.S.)",
                       "Cordial Medoc (France)", "Chartreuse Yellow (France)",
                       "Chartreuse Green (France)", 
                       "Dolfi Framberry in Kiosk (Alsace)",
                       "DOlfi Goldwasser (Alsace)", 
                       "Dolfi Tangerine (France)", "Drambuie (Scotland)", 
                       "Grand Marnier (France)", "Pernod 100 (U.S.)",
                       "Southern Comfort (U.S.)", "Strega (Italy)",
                       "Vielle Cure (France)", "S.S. Pierce Dry Martini",
                       "S.S. Pierce Manhattan", "Hiram Walker Manhattan",
                       "Hiram Walker Martini", "Heublien Extra Dry Martini",
                       "Heublien Manhattan", "Heublien Daiquiri")

train.right.2$price1 = c("3.74", "3.75", "3.85", "3.86", "3.99",
                         "4.04", "4.07", "6.60", "6.65", "7.16", 
                         "5.40", "7.84", "8.93", "8.82", "9.42",
                         "9.95", "6.72", "7.98", "7.98", "3.69",
                         "4.55", "4.24", "3.99", "4.95", "6.85",
                         "7.89", "7.68", "8.76", "9.00", "6.60",
                         "6.60", "8.75", "7.97", "6.96", "5.94",
                         "5.65", "6.60", "1.99", "1.99", "3.44",
                         "3.69", "3.81", "3.81", "3.89")





# left
adist(paste(train.left$Name, collapse = " "), paste(database.left$Name, collapse = " "))
length(strsplit(paste(train.left$Name, collapse = " "), "")[[1]])

adist(paste(train.left$price1, collapse = " "), paste(database.left$price1, collapse = " "))
length(strsplit(paste(train.left$price1, collapse = " "), "")[[1]])

adist(paste(train.left$price2, collapse = " "), paste(database.left$price2, collapse = " "))
length(strsplit(paste(train.left$price2, collapse = " "), "")[[1]])

# left.2
adist(paste(train.left.2$Name, collapse = " "), paste(database.left.2$Name, collapse = " "))
length(strsplit(paste(train.left.2$Name, collapse = " "), "")[[1]])

adist(paste(train.left.2$price1, collapse = " "), paste(database.left.2$price1, collapse = " "))
length(strsplit(paste(train.left.2$price1, collapse = " "), "")[[1]])

adist(paste(train.left.2$price2, collapse = " "), paste(database.left.2$price2, collapse = " "))
length(strsplit(paste(train.left.2$price2, collapse = " "), "")[[1]])

# left.3
adist(paste(train.left.3$Name, collapse = " "), paste(database.left.3$Name, collapse = " "))
length(strsplit(paste(train.left.3$Name, collapse = " "), "")[[1]])

adist(paste(train.left.3$price1, collapse = " "), paste(database.left.3$price1, collapse = " "))
length(strsplit(paste(train.left.3$price1, collapse = " "), "")[[1]])

adist(paste(train.left.3$price2, collapse = " "), paste(database.left.3$price2, collapse = " "))
length(strsplit(paste(train.left.3$price2, collapse = " "), "")[[1]])


# right
adist(paste(train.right$Name, collapse = " "), paste(database.right$Name, collapse = " "))
length(strsplit(paste(train.right$Name, collapse = " "), "")[[1]])

adist(paste(train.right$price1, collapse = " "), paste(database.right$price1, collapse = " "))
length(strsplit(paste(train.right$price1, collapse = " "), "")[[1]])


# right.2
adist(paste(train.right.2$Name, collapse = " "), paste(database.right.2$Name, collapse = " "))
length(strsplit(paste(train.right.2$Name, collapse = " "), "")[[1]])

adist(paste(train.right.2$price1, collapse = " "), paste(database.right.2$price1, collapse = " "))
length(strsplit(paste(train.right.2$price1, collapse = " "), "")[[1]])
