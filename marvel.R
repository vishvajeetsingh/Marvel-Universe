
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

start.time <- Sys.time()
df<-read.csv('data.csv',sep=',')
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#The dataset is pretty large and sparse : `(27290, 1823)` (`dim(df)`)and all the cells (but those from the first column) contain 2 types of factor : `True` and `False`. From the description : `All the cells contain a boolean value: true if there is a version of that character from that universe or false otherwise.`
#(The first column contain the names of the characters)

#As we are interested of the `Marvel Universe` in which the character exists, we need to select only the `True` case.

#A _brute force_ method would be to:
  
#1. loop over columns and rows
#2. find the column which is `True`
#3. get the corresponding column name (corresponding to the `Marvel`) and row id (corresponding to the `Marvel Character`)

#However this method is not really efficient (I tested it) so better to use `R` built-in methods

#1. convert the cells from character to integer (`True`=1, `False`=0) 
#2. use `colSums` and `rowSums` to get the summary :
#3. colSums()` will return in how many `Marvel Universe` we can find a gvien `Character`
#4. rowSums()` will return how many `Marvel Characters` exist in the `Universe`


#change factor to characters
dd<-as.data.frame(df %>% mutate_if(is.factor, as.character))

#keep the first column as factor (for later use)
dd$Character<-as.factor(dd$Character)

#define a function to map True->1, False->0
convert<-function(x){
  x[x == 'True'] <-1
  x[x == 'False'] <-0
  x	
}

#apply the function to the whole DF
dd[2:ncol(dd)]<-lapply(dd[2:ncol(dd)], convert)

#rechange to numeric (somehow the function writes '1' as character ... I need to fix this)
dd<-as.data.frame(dd %>% mutate_if(is.character,as.numeric))

#define dataframe with names  
#number of Marvel Universe had some other cosmetics columns
charNames<-dd[,1]
charAppearance<-rowSums(dd[,2:ncol(dd)])
g<-data.frame(charNames,charAppearance)
g$charAppearancePercentage<-(g$charAppearance/1822)*100
g$charAppearanceLabel<-paste0(round(g$charAppearancePercentage,1),"")
head(g)

#define dataframe with marvel Universe name  
#number of Marvel Characters and some other cosmetics columns
titleNames<-colnames(dd[,2:ncol(dd)])
titleAppearanceChar<-colSums(dd[,2:ncol(dd)])
f<-data.frame(titleNames,titleAppearanceChar)
f$titleAppearanceCharPercentage<-(f$titleAppearanceChar/27290)*100
f$titleAppearanceLabel<-paste0(round(f$titleAppearanceCharPercentage,1),"")
head(f)

#### most frequent `Character`
ggplot(filter(g,charAppearance>50),aes(x=reorder(charNames,-charAppearance),y=charAppearance,fill=charAppearancePercentage)) +
  geom_bar(width = 0.85, stat="identity") + xlab('') + ylab('') + 
  theme(legend.position=c(.9, .8),axis.text.x = element_text(angle=90, hjust=1),text = element_text(size=10)) +
  scale_fill_continuous(low="orange", high="red", limits=c(0,25)) +
  geom_text(aes(label=charAppearanceLabel), position=position_dodge(width=0.9), hjust=.2,vjust=-0.25,size=2.5) +
  guides(fill=guide_legend(title="Appearance Percentage"))

#This plot gives the distribution (limited to a number of appearance >50 for visualization purposes) of all characters. For example, `Peter Parker` appears in more than 400 `Universes`, which corresponds to ~ 24% of all the `Universes` (from this dataset)

##### most populated `Universe`

ggplot(data=filter(f,titleAppearanceChar>100),aes(x=reorder(titleNames,-titleAppearanceChar),y=titleAppearanceChar,fill=titleAppearanceCharPercentage)) +
  geom_bar(stat='identity') + theme(axis.text.x = element_text(angle=90, hjust=1))+ xlab('') + ylab('') + 
  theme(legend.position=c(.9, .8),axis.text.x = element_text(angle=90, hjust=1),text = element_text(size=10)) +
  scale_fill_continuous(low="orange", high="red", limits=c(0,80)) +
  geom_text(aes(label=titleAppearanceLabel), position=position_dodge(width=0.9), hjust=.2,vjust=-0.25,size=2.5) +
  guides(fill=guide_legend(title="Universe population Percentage")) + scale_y_log10()

#This plot gives the distribution of all `Universe` (limited to a number of `Characters` existing in it greater than 100 for visualization purposes). `Earth.616` is the main `Universe` with more than 20000 `Characters` (~75%) ( _notice the log scale_ )

  
##### most frequent `Character`
DT::datatable(g)
##### most populated `Universe`
DT::datatable(f)


##### most frequent `Character`
colfunc <- colorRampPalette(c("pink", "white"))
op <- par(mar=c(1, 1, 1, 1),mfrow=c(1, 1),bg="black")
wordcloud(g$charNames,g$charAppearance,min.freq = 15,colors=colfunc(4),scale = c(2, 0.5))

##### most populated `Universe`
colfunc <- colorRampPalette(c("steelblue", "white"))
op <- par(mar=c(1, 1, 1, 1),mfrow=c(1, 1),bg="black")
wordcloud(f$titleNames,f$titleAppearanceChar,min.freq = 35,colors=colfunc(10),scale = c(2, 1))

#We've seen that `Earth.XXX` is the most populated `Universe` and therefore makes a very skewed distribution. In this section I will just disentangle all the `Universe` from the `Earth-like` and look at some distribution.

subGenre<-c('X2015','Age.of.X','Avengers.Fairy.Tales','Brilliant.City','Eath','Earth.Glaxo.Inc','Earth.Mars.Colony.2011','Earth.Mesozoic24','Earth.N','Earth.Shi','Earth.TRN','Elsewhen','Eurth','Immortus.Servant','Mojoverse','Multiverse','Omniverse','Otherworld','Panoptichron','Sachs...Violens','Saturnyne','Special.Executive','Technet','Temporal.Limbo','Utopian.Parallel','War.Toy','Weirdworld')

#all but Earth.XXX
f1<-as.data.frame(f %>% filter(titleNames %in% subGenre))
#all Earth.XXX
f2<-as.data.frame(f %>% filter(!titleNames %in% subGenre))

##### most populated `Universe` but not from `Earth.XXX`
ggplot(data=f1,aes(x=reorder(titleNames,-titleAppearanceChar),y= titleAppearanceChar,fill=titleAppearanceChar)) + geom_bar(stat='identity') + 
theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1),text = element_text(size=12))+
scale_fill_continuous(low="steelblue", high="black", limits=c(0,60)) +
xlab('') + ylab('') + guides(fill=guide_legend(title="Universe population number"))

##### most populated `Universe` from `Earth.XXX` but not `Earth.616`
ggplot(data=filter(f2,titleNames!='Earth.616' & titleAppearanceChar>50),aes(x=reorder(titleNames,-titleAppearanceChar),y= titleAppearanceChar,fill=titleAppearanceChar)) + geom_bar(stat='identity') + 
theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1),text = element_text(size=10)) + 
scale_y_log10() +
scale_fill_continuous(low="steelblue", high="black", limits=c(0,1000)) +
xlab('') + ylab('') + guides(fill=guide_legend(title="Universe population number"))

