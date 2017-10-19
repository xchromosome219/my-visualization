library(Vennerable)
x <- c("906T","MK10","MK1011","MK1020","MK1021","MK1024","MK1025","MK1059","MK1061","MK1074","MK109","MK1091","MK1092","MK1098","MK1100","MK1117","MK1119","MK1123","MK1134","MK1136","MK114","MK1143","MK1144","MK1196","MK1198","MK1202","MK1203","MK1204","MK1205","MK1244","MK1246","MK1248","MK1249","MK1342","MK1364","MK1370","MK1413","MK1416","MK1421","MK1459","MK1467","MK1473","MK1499","MK1558","MK1560","MK1571","MK1582","MK162","MK1650","MK205","MK258","MK267","MK27","MK28","MK44","MK508","MK53","MK554","MK565","MK601","MK613","MK616","MK679","MK687","MK736","MK74","MK753","MK764","MK771","MK79","MK920","MK928","MK936","MK945","MK946","MK959","MK966","MK970","MK972","591T","714T","742T","896T","927T","MK1007","MK1022","MK1028","MK1029","MK1058","MK1094","MK1099","MK111","MK1120","MK1189","MK1191","MK1201","MK1210","MK1220","MK1335","MK1337","MK1338","MK1343","MK1344","MK1361","MK1402","MK1422","MK1449","MK1452","MK1460","MK1472","MK1483","MK1492","MK152","MK1559","MK37","MK513","MK533","MK571","MK630","MK738","MK754","MK80","MK807","MK814","MK832","MK926","MK963","MK968","MK969","MK984","MK1016","MK103","MK1095","MK1097","MK1106","MK1125","MK117","MK1206","MK1207","MK1219","MK1299","MK132","MK1363","MK1482","MK1514","MK1556","MK504","MK55","MK58","MK67","MK686","MK729","MK734","MK929","MK933","MK1012","MK1013","MK1019","MK1063","MK1101","MK1103","MK1105","MK1141","MK156","MK1569","MK857")
for ( val in x ){
file <- scan(val, what="", sep="\n")
y <- strsplit(file, "[[:space:]]+")
names(y) <- sapply(y, `[[`, 1)
y <- lapply(y, `[`, -1)
w <- Venn(Sets = y)

nms <- combn(names(y),2,FUN = paste0,collapse="+",simplify = FALSE)
ll <- combn(y,2,simplify = FALSE)
out <- lapply(ll,function(x) (intersect(x[[1]],x[[2]])))
aaaa <- setNames(out,nms)
abab <- as.matrix(aaaa)
write.table(abab,paste(val,"_list.txt",sep = ""),col.names = FALSE,sep = "------",quote = FALSE)

output <- sub("*.txt","\\1",val)
png(filename =  paste(output,".png",sep = ""),width = 10,height = 10,units = 'in',res = 300)
plot(w)
grid.text(val, x= 0.3, y=0.9, gp=gpar(col="red", cex=3))
dev.off()
}
