#----------------------------------------------------------------------------
# Part 1 : Downloading Twitter data using R
#----------------------------------------------------------------------------

# install.packages("twitteR")
# install.packages("reshape")
library(twitteR)
library(reshape)
api_key <- '--'
api_secret <- '--'
access_token <- '--'
access_token_secret <- '--'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

## Error in which(sapply(tweets, function(x) x$getIsRetweet())) : 
##   argument to 'which' is not logical
# This error because of APT cannot return tweets>=n, need adjust since and until

set.seed(20180302); ht <- '#blockchain'; 
tweets.raw <- searchTwitter(ht, n = 50000, lang = 'en');
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE)); 
df$hashtag <- ht; 
df$created <- as.POSIXlt(df$created); 
# To transform the emojis, you first need to transform the tweet data into ASCII:
df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte'); 
df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id); 
df <- rename(df, c(retweetCount = 'retweets'));
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweets, hashtag));
nrow(df.a); head(df.a);
setwd('C:/Users/skyof/Desktop/PRISMOJI/PRISMOJI-First step'); 
write.csv(df.a, paste0('tweets.cleaned_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);
tweets <- df; tweets$z <- 1; tweets$created <- as.POSIXlt(tweets$created); nrow(tweets); min(tweets$created); max(tweets$created); median(tweets$created)

head(df.a)

#----------------------------------------------------------------------------
# Part 2 ✍: Using Unicode pattern matching to extract emojis from tweets
#----------------------------------------------------------------------------
# install.packages('DataCombine')
library(tidyverse)
library(DataCombine)

emoticons <- read.csv("emoji_dictionary.csv", header = T)
emojis=emoticons %>% mutate(Name_Number=paste0(Name,Number))

emojireplace <- FindReplace(data = df, Var = "text", 
                            replaceData = emojis,
                            from = "R_Encoding", to = "Name_Number", 
                            exact = FALSE)


library(stringr)

emoji_final<-data.frame("emoji1",1)
names(emoji_final)<-c("EMOJI","Count")
for(i in emojis$Name_Number){
  a=sum(str_count( emojireplace$text,i))
  de<-data.frame(i,a)
  names(de)<-c("EMOJI","Count")
  emoji_final <- rbind(emoji_final, de)
}

emoji_final_clean=emoji_final%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final)))%>%
  mutate(dens=1000*Count/nrow(emoji_final))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!rank>=11)

emoji_final_clean <- FindReplace(data = emoji_final_clean, Var = "EMOJI", 
                            replaceData = emojis,
                            from = "Name_Number", to = "Name", 
                            exact = FALSE)
emoji_final_clean <- FindReplace(data = emoji_final_clean, Var = "Codepoint", 
                                 replaceData = emojis,
                                 from = "Name_Number", to = "Codepoint", 
                                 exact = FALSE)
emoji_final_clean


#Top 20
emoji_final_clean_top20=emoji_final%>%  arrange(desc(Count))%>% 
  mutate(rank= as.numeric(row.names(emoji_final)))%>%
  mutate(dens=1000*Count/nrow(emoji_final))%>%
  mutate(Codepoint=EMOJI)%>%
  filter(!rank>=21)


emoji_final_clean_top20 <- FindReplace(data = emoji_final_clean_top20, Var = "EMOJI", 
                                 replaceData = emojis,
                                 from = "Name_Number", to = "Name", 
                                 exact = FALSE)
emoji_final_clean_top20 <- FindReplace(data = emoji_final_clean_top20, Var = "Codepoint", 
                                 replaceData = emojis,
                                 from = "Name_Number", to = "Codepoint", 
                                 exact = FALSE)

emoji_final_clean_top20

#----------------------------------------------------------------------------
# Part 3 📊: Visualizing emojis(basic)
#----------------------------------------------------------------------------


df.plot <- emoji_final_clean; 
# Eliminate the space
df.plot$EMOJI=gsub(' ','',df.plot$EMOJI)
xlab= 'Rank'; 
ylab <- 'Overall Frequency in Tweets';

setwd('C:/Users/skyof/Desktop/PRISMOJI/PRISMOJI-First step/AlanChi-emoji');
imgs <- lapply(paste0(df.plot$EMOJI, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); 
df.plot$xsize <- k; df.plot$ysize <- k; 
#df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot$ysize <- 0.55*k;
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = '#86C166') +
  xlab(xlab) + ylab(ylab)+
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1


#Top 20
df.plot <- emoji_final_clean_top20; 
# Eliminate the space
df.plot$EMOJI=gsub(' ','',df.plot$EMOJI)
xlab= 'Rank'; 
ylab <- 'Overall Frequency in Tweets';

setwd('C:/Users/skyof/Desktop/PRISMOJI/PRISMOJI-First step/AlanChi-emoji');
imgs <- lapply(paste0(df.plot$EMOJI, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); 
df.plot$xsize <- k; df.plot$ysize <- k; 
#df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot$ysize <- 0.55*k;
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = '#86C166') +
  xlab(xlab) + ylab(ylab)+
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1


