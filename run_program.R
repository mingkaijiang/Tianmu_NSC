### This repository contains scripts to process the non-structural carbohydrate data 
### collected from Tianmu mountain.
### Developer: Mingkai Jiang (jiangmingkai@zju.edu.cn)

##########################################################################
#### Step 1: basic set-up

### create a folder called "data", and put the NSC_TEST_RESULT_ZHIWANG_11.04.csv file into this folder


#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

##########################################################################
#### Step 2: read input data
myDF <- read.csv("data/NSC_TEST_RESULT_ZHIWANG_11.04.csv", header=T)

myDF$STAR_MEAN <- NULL
myDF$SURG_MEAN <- NULL

subDF1 <- myDF[,c("TEST_ID", "TREE_ID", "SPECIES", "YEAR", "MONTH", "SITE_NUM",
                  "TISSUE_NUM", "TISSUE", "STAR_R1", "STAR_R2", "STAR_R3")]
subDF2 <- myDF[,c("TEST_ID", "TREE_ID", "SPECIES", "YEAR", "MONTH", "SITE_NUM",
                  "TISSUE_NUM", "TISSUE", "SURG_R1", "SURG_R2", "SURG_R3")]


rsDF1 <- reshape2::melt(subDF1, id.vars=c("TEST_ID", "TREE_ID", "SPECIES", "YEAR", "MONTH", "SITE_NUM",
                                          "TISSUE_NUM", "TISSUE"),
                        variable.name="REPLICATE")

rsDF2 <- reshape2::melt(subDF2, id.vars=c("TEST_ID", "TREE_ID", "SPECIES", "YEAR", "MONTH", "SITE_NUM",
                                          "TISSUE_NUM", "TISSUE"),
                        variable.name="REPLICATE")


rsDF1$REPLICATE <- gsub("STAR_", "", rsDF1$REPLICATE)
rsDF2$REPLICATE <- gsub("SURG_", "", rsDF2$REPLICATE)


colnames(rsDF1)[colnames(rsDF1) == "value"] ="STARCH"
colnames(rsDF2)[colnames(rsDF2) == "value"] ="SUGAR"


myDF2 <- merge(rsDF1, rsDF2, by=c("TEST_ID", "TREE_ID", "SPECIES", "YEAR", "MONTH", "SITE_NUM",
                                  "TISSUE_NUM", "TISSUE", "REPLICATE"))


sumDF <- summaryBy(STARCH+SUGAR~SPECIES+MONTH+SITE_NUM+TISSUE, data=myDF2,
                  FUN=c(mean,sd), na.rm=T, keep.names=T)

sumDF$MONTH <- gsub("7", "JUL", sumDF$MONTH)
sumDF$MONTH <- gsub("8", "AUG", sumDF$MONTH)
sumDF$MONTH <- gsub("9", "SEP", sumDF$MONTH)


#plotDF1 <- subset(sumDF, SPECIES == "Ginkgo biloba")
#
#p1 <- ggplot(plotDF1, aes(x=MONTH, y=STARCH.mean))+
#    geom_errorbar(mapping = aes(x=MONTH, ymin=STARCH.mean-STARCH.sd, 
#                                ymax=STARCH.mean+STARCH.sd), 
#                  position=position_dodge(width=0.5), width=0.2) +
#    geom_point(data=plotDF1, aes(MONTH, STARCH.mean),
#               size=3)+
#    facet_wrap(. ~ SITE_NUM+TISSUE, scales="free") +
#    theme_linedraw() +
#    theme(panel.grid.minor=element_blank(),
#          axis.text.x=element_text(size=12),
#          axis.title.x=element_blank(),
#          axis.text.y=element_text(size=12),
#          axis.title.y=element_text(size=14),
#          legend.text=element_text(size=12),
#          legend.title=element_text(size=14),
#          panel.grid.major=element_blank(),
#          legend.position="none",
#          legend.box = 'horizontal',
#          legend.box.just = 'left',
#          legend.key.width = unit(1.5,"cm"),
#          plot.title = element_text(size=14, face="bold.italic", 
#                                    hjust = 0.5))+
#    ylab(expression(paste(Starch * " (mg C " * g^-1 * ")")))+
#    ylim(50,250)+
#    scale_x_discrete(limits=c("JUL", "AUG", "SEP"))+
#    xlab(expression(paste("Month"))); p1
#
#
#p2 <- ggplot(plotDF1, aes(x=MONTH, y=SUGAR.mean))+
#    geom_errorbar(mapping = aes(x=MONTH, ymin=SUGAR.mean-SUGAR.sd, 
#                                ymax=SUGAR.mean+SUGAR.sd), 
#                  position=position_dodge(width=0.5), width=0.2) +
#    geom_point(data=plotDF1, aes(MONTH, SUGAR.mean),
#               size=3)+
#    facet_wrap(. ~ SITE_NUM+TISSUE, scales="free") +
#    theme_linedraw() +
#    theme(panel.grid.minor=element_blank(),
#          axis.text.x=element_text(size=12),
#          axis.title.x=element_blank(),
#          axis.text.y=element_text(size=12),
#          axis.title.y=element_text(size=14),
#          legend.text=element_text(size=12),
#          legend.title=element_text(size=14),
#          panel.grid.major=element_blank(),
#          legend.position="none",
#          legend.box = 'horizontal',
#          legend.box.just = 'left',
#          legend.key.width = unit(1.5,"cm"),
#          plot.title = element_text(size=14, face="bold.italic", 
#                                    hjust = 0.5))+
#    ylab(expression(paste(Sugar * " (mg C " * g^-1 * ")")))+
#    ylim(0,150)+
#    scale_x_discrete(limits=c("JUL", "AUG", "SEP"))+
#    xlab(expression(paste("Month"))); p2



p1 <- ggplot(sumDF, aes(x=MONTH, y=STARCH.mean, group=SPECIES))+
    geom_errorbar(mapping = aes(x=MONTH, ymin=STARCH.mean-STARCH.sd, 
                                ymax=STARCH.mean+STARCH.sd, col=SPECIES), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(MONTH, STARCH.mean, fill=SPECIES),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ SITE_NUM+TISSUE, scales="free") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text.x=element_text(size=12),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom",
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.key.width = unit(1.5,"cm"),
          plot.title = element_text(size=14, face="bold.italic", 
                                    hjust = 0.5))+
    ylab(expression(paste(Starch * " (mg C " * g^-1 * ")")))+
    ylim(50,300)+
    scale_x_discrete(limits=c("JUL", "AUG", "SEP"))+
    xlab(expression(paste("Month"))); p1



p2 <- ggplot(sumDF, aes(x=MONTH, y=SUGAR.mean, group=SPECIES))+
    geom_errorbar(mapping = aes(x=MONTH, ymin=SUGAR.mean-SUGAR.sd, 
                                ymax=SUGAR.mean+SUGAR.sd, col=SPECIES), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(MONTH, SUGAR.mean, fill=SPECIES),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ SITE_NUM+TISSUE, scales="free") +
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text.x=element_text(size=12),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom",
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.key.width = unit(1.5,"cm"),
          plot.title = element_text(size=14, face="bold.italic", 
                                    hjust = 0.5))+
    ylab(expression(paste(Sugar * " (mg C " * g^-1 * ")")))+
    ylim(0,150)+
    scale_x_discrete(limits=c("JUL", "AUG", "SEP"))+
    xlab(expression(paste("Month"))); p2



#legend_row <- get_legend(p1 + theme(legend.position="bottom",
#                                    legend.box = 'horizontal',
#                                    legend.box.just = 'left'))
#
#plots_row <- plot_grid(p1, p2, 
#                       labels=c("(a)", "(b)"),
#                       ncol=2, align="vh", axis = "l",
#                       label_x=0.86, label_y=0.95,
#                       label_size = 18)


pdf(paste0("output/NSC_overview.pdf"), 
    width=16, height=16)
#plot_grid(plots_row,
#          legend_row,
#          ncol=1, rel_heights=c(1,0.2))

plot(p1)
plot(p2)
dev.off()


