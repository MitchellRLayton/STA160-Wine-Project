#read data
#OCR is not perfect. Just accept it
data = readRDS("FullBoxes.rds")
#read file
doc.0190 = data$UCD_Lehmann_0190.jpg
#left bottom right top text confidence

plot(doc.0190$bottom)
plot(doc.0190$left)
plot(doc.0190$right)



#read left table
left = subset(doc.0190, doc.0190$left < max(doc.0190$left)/2 & doc.0190$bottom > 3500)
plot(left$bottom)

#find difference between bottom
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

#get rid of unecessary text
left$text = gsub('\\_', '', left$text)
left$text = gsub('\\.', '', left$text)

#find each line by indexing
left$index = 0
t = 1
left$index[1] = t
for (i in 1:nrow(left)) {
  left$index[i] = t
  if (left$bottom.diff[i] > 50) {
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

#if name of wine is on multiple lines, combines dataframes
for (i in 1: max(left$index)) {
if (diff[i] < 600) {
  splitted[[i]] = rbind(splitted[[i]], splitted[[i+1]])
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
  splitted[[i+1]] = NULL
}
}

frame = NULL
for (i in 1:27) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

#left bottom right top text confidence
#read right table
right = subset(doc.0190, doc.0190$left > max(doc.0190$left)/2 & doc.0190$bottom > 3470 & doc.0190$bottom < 5250)
plot(right$bottom)

#find difference between bottom
right$bottom.diff = ave(right$bottom, FUN=function(x) c(0,diff(x)))

#get rid of unecessary text
right$text = gsub('\\_', '', right$text)
right$text = gsub('\\.', '', right$text)

#find each line by indexing
right$index = 0
t = 1
right$index[1] = t
for (i in 1:nrow(right)) {
  right$index[i] = t
  if (right$bottom.diff[i] > 50) {
    t = t+1
    right$index[i] = t
  }
}

#split data by index. i.e. find each line in table
splitted.right = NULL
for (i in 1: max(right$index)) {
  splitted.right[[i]] = right[right$index == i, ]
}

#find if name of wine is on multiple lines
diff.right = numeric(max(right$index))
for (i in 1: max(right$index)) {
  diff.right[i] = splitted.right[[i]]$right[nrow(splitted.right[[i]])] - splitted.right[[i]]$left[1]
}

#if name of wine is on multiple lines, combines dataframes
for (i in 1: max(right$index)) {
  if (diff.right[i] < 600) {
    splitted.right[[i]] = rbind(splitted.right[[i]], splitted.right[[i+1]])
    diff.right[i] = splitted.right[[i]]$right[nrow(splitted.right[[i]])] - splitted.right[[i]]$left[1]
    splitted.right[[i+1]] = NULL
  }
}

frame.right = NULL
for (i in 1:27) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted.right[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted.right[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame.right[i] = paste(splitted.right[[i]]$text, collapse = ' ')
}


