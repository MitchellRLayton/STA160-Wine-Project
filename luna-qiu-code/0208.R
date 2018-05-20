data = readRDS("FullBoxes.rds")
doc.0208 = data$UCD_Lehmann_0208.jpg
#left bottom right top text confidence

top = subset(doc.0208, left > 2500 & bottom < 2600 & bottom > 1860)

plot(top$bottom)

#find difference between bottom
top$bottom.diff = ave(top$bottom, FUN=function(x) c(0,diff(x)))
top$height = top$top - top$bottom

top$bottom.diff[1] = 0

#find each line by indexing
top$index = 0
t = 1
top$index[1] = t
for (i in 1:nrow(top)) {
  top$index[i] = t
  if (top$bottom.diff[i] > 70) {
    t = t+1
    top$index[i] = t
  }
}

top = subset(top, height > 50)
top$text = gsub('\\.', ' ', top$text)

#split data by index. i.e. find each line in table
splitted = NULL
for (i in 1: max(top$index)) {
  splitted[[i]] = top[top$index == i, ]
}

#find if name of wine is on multiple lines
diff = numeric(max(top$index))
for (i in 1: max(top$index)) {
  diff[i] = splitted[[i]]$right[nrow(splitted[[i]])] - splitted[[i]]$left[1]
}

frame = NULL
for (i in 1:7) {
  #change bottom.diff in first row to 0 so that subset doesn't remove them
  splitted[[i]]$bottom.diff[1] = 0
  #remove rows with high bottom.diff. these are unecessary
  #but it doesn't work. idk
  subset(splitted[[i]], bottom.diff < 25)
  #gives back dataframe with each line containing info for each wine
  frame[i] = paste(splitted[[i]]$text, collapse = ' ')
}

#if line has < 5 characters, delete
#put . back in prices
#if line starts with number, delete. It is wine no.


middle = subset(doc.0208, left > 2400 & bottom < 3800 & bottom > 3000)

plot(middle$bottom)


left = subset(doc.0208, left > 2400)
left$bottom.diff = ave(left$bottom, FUN=function(x) c(0,diff(x)))

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

