#K-Means Clustering on Crime Dataset
#loading Packages
library(animation)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#Loading Dataset
data <-read.csv("01_District_wise_crimes_committed_IPC_2001_2012.csv",header=T)
summary(data)

top.n.custs <- function (data,cols,n=5) 
{ 
  idx.to.remove <-integer(0) 
  for (c in cols)
  { 
    col.order <-order(data[,c],decreasing=T) 
    idx <-head(col.order, n) 
    idx.to.remove <-union(idx.to.remove,idx) 
  }
  return(idx.to.remove) 
}
top.custs <-top.n.custs(data,cols=3:33,n=5)
length(top.custs) 
data[top.custs,] 
data.rm.top<-data[-c(top.custs),] 

set.seed(76964057) 
k <-kmeans(data.rm.top[,-c(1,2,3)], centers=5) 
k$centers 
table(k$cluster)

#K-Means Clustering on Crime Dataset and Graph Plotting and HeatMap Generation
df <- data %>% select(-c(STATE.UT,DISTRICT, YEAR,IMPORTATION.OF.GIRLS.FROM.FOREIGN.COUNTRIES))
glimpse(df)
summary(df)
rescale_df <- df %>%
  mutate(MURDER_scal = scale(MURDER),
         ATM_scal = scale(ATTEMPT.TO.MURDER),
         CHNATM_scal = scale(CULPABLE.HOMICIDE.NOT.AMOUNTING.TO.MURDER),
         RAPE_scal = scale(RAPE),
         CR_scal = scale(CUSTODIAL.RAPE),
         OR_scal = scale(OTHER.RAPE),
         KA_scal = scale(KIDNAPPING...ABDUCTION),
         KAAOWAG_scal = scale(KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS),
         KAAOO_scal = scale(KIDNAPPING.AND.ABDUCTION.OF.OTHERS),
         DACOITY_scal = scale(DACOITY),
         PAAFD_scal = scale(PREPARATION.AND.ASSEMBLY.FOR.DACOITY),
         ROBBERY_scal = scale(ROBBERY),
         BURG_scal = scale(BURGLARY),
         THEFT_scal = scale(THEFT),
         AT_scal = scale(AUTO.THEFT),
         OT_scal = scale(OTHER.THEFT),
         RIOTS_scal = scale(RIOTS),
         CBOT_scal = scale(CRIMINAL.BREACH.OF.TRUST),
         CHEAT_scal = scale(CHEATING),
         CF_scal = scale(COUNTERFIETING),
         ARSON_scal = scale(ARSON),
         HGH_scal = scale(HURT.GREVIOUS.HURT),
         DD_scal = scale(DOWRY.DEATHS),
         AOWWITOHM_scal = scale(ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY),
         ITMOW_scal = scale(INSULT.TO.MODESTY.OF.WOMEN),
         CBHOHR_scal = scale(CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES),
         CDBN_scal = scale(CAUSING.DEATH.BY.NEGLIGENCE),
         OIC_scal = scale(OTHER.IPC.CRIMES),
         TIC_scal = scale(TOTAL.IPC.CRIMES)) %>% select(-c(MURDER,ATTEMPT.TO.MURDER,CULPABLE.HOMICIDE.NOT.AMOUNTING.TO.MURDER,RAPE,CUSTODIAL.RAPE,OTHER.RAPE,
            KIDNAPPING...ABDUCTION,KIDNAPPING.AND.ABDUCTION.OF.WOMEN.AND.GIRLS,KIDNAPPING.AND.ABDUCTION.OF.OTHERS,
            DACOITY,PREPARATION.AND.ASSEMBLY.FOR.DACOITY,ROBBERY,BURGLARY,THEFT,AUTO.THEFT,OTHER.THEFT,RIOTS,CRIMINAL.BREACH.OF.TRUST,
            CHEATING,COUNTERFIETING,ARSON,HURT.GREVIOUS.HURT,DOWRY.DEATHS,ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY,
            INSULT.TO.MODESTY.OF.WOMEN,CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES,CAUSING.DEATH.BY.NEGLIGENCE,
            OTHER.IPC.CRIMES,TOTAL.IPC.CRIMES))

set.seed(2345)
kmeans.ani(rescale_df[1:2], 3)
pc_cluster <-kmeans(rescale_df, 5)
kmean_withinss <- function(k) 
{
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}
kmean_withinss(2)
# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
wss
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
elbow

# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

pc_cluster_2 <-kmeans(rescale_df, 7)
pc_cluster_2$cluster
pc_cluster_2$centers
pc_cluster_2$size
center <-pc_cluster_2$centers
center

# create dataset with the cluster number
cluster <- c(1: 7)
center_df <- data.frame(cluster, center)

# Reshape the data
center_reshape <- gather(center_df, features, values, MURDER_scal: ATM_scal)
head(center_reshape)

# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()