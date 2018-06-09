data = readRDS("FullBoxes.rds")
doc.0260 = data$UCD_Lehmann_0260.jpg
#left bottom right top text confidence

doc.0260$text = gsub('\\.', '', doc.0260$text)
doc.0260$text = gsub('\\[', '', doc.0260$text)

table = subset(doc.0260, left > 2400 & bottom < 3500)
table

table$bottom.diff = ave(table$bottom, FUN=function(x) c(0,diff(x)))

table$index = 0
t = 1
table$index[1] = t
for (i in 1:nrow(table)) {
  table$index[i] = t
  if (table$bottom.diff[i] > 25) {
    t = t+1
    table$index[i] = t
  }
}

splitted = NULL
for (i in 1: max(table$index)) {
  splitted[[i]] = table[table$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(table$index))
for (i in 1: max(table$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: length(splitted)) {
  if (diff[i] < 680) {
    splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
    diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
    splitted[[i+1]] = NULL
  }
}

frame = NULL
for (i in 1:length(splitted)) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$table.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], table.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

frame

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
  name[[i]] = stringsplit[[i]][1:(length(stringsplit[[i]]) - mode(p))]
  name[[i]] = paste(name[[i]], collapse = " ")
  price[[i]] = stringsplit[[i]][(length(stringsplit[[i]]) - mode(p) + 1):length(stringsplit[[i]])]
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
train = train[-c(1),]
train$Name = unlist(train$Name)
train$Name = c("Ayala 1947","Bollinger Brut","Bollinger Brut",
               "Clicquot 1949", "Duminy Brut", "Duminy 1949",
               "Charles Heidsieck Brut", "Charles Heidsieck 1953",
               "J & B Sarcey 1949", "Krug Brut Reserve", "Krug 1949",
               "Lanson 1952", "Moet White Seal", "Moet & Chandon 1952",
               "Dom Perignon 1949", "Mumm's Extra Dry", 
               "Mumm's Cordon Rouge", "Mumm's Cordon Rouge 1952",
               "Perrier-Jouet Brut", "Perrier-Jouet 1952",
               "Pol Roger Dry Special", "Pol Roger 1949",
               "Piper Heidsieck Pink", "Piper Heidsieck Brut", 
               "Piper Heidsieck 1952", "Pommery & Greno 1952",
               "Louis Roederer Brut", "Louis Roederer 1952",
               "Louis Roederer Crystal 1952", "Taittinger 1949",
               "Taittinger 1952 Blanc de Blanc")
train$price1 = c("6.95","6.94","8.79","8.98","4.90","5.90",
                 "6.37","8.56","6.89","6.95","10.00","8.90",
                 "5.99","8.49","11.00","6.28","6.94","8.40","6.70",
                 "7.85","5.90","9.40","7.95", "6.75", "8.70", 
                 "8.49","6.85", "8.17", "9.77", "8.69", "11.29")

train$price2 = c("75.45", "74.95", "94.95", "97.06", "53.50",
                 "64.90", "72.28", "91.89", "74.25", "75.06",
                 "108.00", "96.12", "64.70", "91.69", "118.80",
                 "69.10", "76.33", "92.42", "72.50", "85.00", 
                 "63.72", "101.52", "87.45", "74.52", "95.70", 
                 "91.69", "73.93", "88.15", "105.52", "95.94", 
                 "124.64")




adist(paste(train$Name, collapse = " "), paste(database$Name, collapse = " "))
length(strsplit(paste(train$Name, collapse = " "), "")[[1]])

adist(paste(train$price1, collapse = " "), paste(database$price1, collapse = " "))
length(strsplit(paste(train$price1, collapse = " "), "")[[1]])

adist(paste(train$price2, collapse = " "), paste(database$price2, collapse = " "))
length(strsplit(paste(train$price2, collapse = " "), "")[[1]])

