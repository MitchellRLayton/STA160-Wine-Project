train = read.csv("df_0237t.csv", sep = "\t")

penpro = read.csv("PenPro_0237.csv", sep = "\t")
penpro$Name = paste(penpro$BottleNo, penpro$Name)

#row = penpro
#column = train
bla = matrix(numeric(nrow(penpro)*nrow(train)), nrow = nrow(penpro), ncol = nrow(train))
minimum = NULL
for (i in 1: nrow(penpro))
{
  for(j in 1: nrow(train)) {
    bla[i,j] = adist(paste(penpro$Name[i], collapse = " "), paste(train$name[j], collapse = " "))
  }
  minimum[i] = min(bla[i,])
}
sum(minimum)

length(strsplit(paste(penpro$Name, collapse = " "), "")[[1]])


bla = matrix(numeric(nrow(penpro)*nrow(train)), nrow = nrow(penpro), ncol = nrow(train))
minimum = NULL
for (i in 1: nrow(penpro))
{
  for(j in 1: nrow(train)) {
    bla[i,j] = adist(paste(penpro$BottlePrice[i], collapse = " "), paste(train$bottle_price[j], collapse = " "))
  }
  minimum[i] = min(bla[i,])
}
sum(minimum)

length(strsplit(paste(penpro$BottlePrice, collapse = " "), "")[[1]])

bla = matrix(numeric(nrow(penpro)*nrow(train)), nrow = nrow(penpro), ncol = nrow(train))
minimum = NULL
for (i in 1: nrow(penpro))
{
  for(j in 1: nrow(train)) {
    bla[i,j] = adist(paste(penpro$CasePrice[i], collapse = " "), paste(train$case_price[j], collapse = " "))
  }
  minimum[i] = min(bla[i,])
}
sum(minimum)

length(strsplit(paste(penpro$CasePrice, collapse = " "), "")[[1]])


