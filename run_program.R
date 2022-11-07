### Script to analyze data collected from ChangXing, for the ABARES basalt project

#########################################################################
#### Step 1: basic set-up

### create a folder called "data", and put the NSC_TEST_RESULT_ZHIWANG_11.04.csv file into this folder


#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")


#########################################################################
### read input data on LMA
lmaDF <- read.csv("data/ChangXing_data_LMA_20221107.csv", header=T)

sumDF <- summaryBy(LMA_g_cm2~Species+Harvest_date+Fertilizer_treatment, data=lmaDF, FUN=c(mean,sd),
                   na.rm=T, keep.names=T)


### plot
p1 <- ggplot(sumDF, aes(x=Species, y=LMA_g_cm2.mean, group=Species))+
    geom_errorbar(mapping = aes(x=Species, ymin=LMA_g_cm2.mean-LMA_g_cm2.sd, 
                                ymax=LMA_g_cm2.mean+LMA_g_cm2.sd, col=Species), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(Species, LMA_g_cm2.mean, fill=Species),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ Harvest_date+Fertilizer_treatment, scales="free") +
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
    ylab(expression(paste(LMA * " (g DM " * cm^-2 * ")")))+
    ylim(0.004, 0.008)+
    xlab(expression(paste("Species"))); p1



#########################################################################
### read input data on LMA
biomassDF <- read.csv("data/ChangXing_data_Biomass_20221107.csv", header=T)

biomassDF$Total <- with(biomassDF, (Leaf_biomass_g_DM+Stem_biomass_g_DM+Root_biomass_g_DM))

sumDF <- summaryBy(Leaf_biomass_g_DM+Stem_biomass_g_DM+Root_biomass_g_DM+Total~Species+Harvest_date+Fertilizer_treatment, 
                   data=biomassDF, FUN=c(mean,sd),
                   na.rm=T, keep.names=T)


### plot
p2 <- ggplot(sumDF, aes(x=Species, y=Total.mean, group=Species))+
    geom_errorbar(mapping = aes(x=Species, ymin=Total.mean-Total.sd, 
                                ymax=Total.mean+Total.sd, col=Species), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(Species, Total.mean, fill=Species),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ Harvest_date+Fertilizer_treatment, scales="free") +
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
    ylab(expression(paste("Total biomass (g DM " * plant^-1 * ")")))+
    ylim(0, 12)+
    xlab(expression(paste("Species"))); p2


p3 <- ggplot(sumDF, aes(x=Species, y=Leaf_biomass_g_DM.mean, group=Species))+
    geom_errorbar(mapping = aes(x=Species, ymin=Leaf_biomass_g_DM.mean-Leaf_biomass_g_DM.sd, 
                                ymax=Leaf_biomass_g_DM.mean+Leaf_biomass_g_DM.sd, col=Species), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(Species, Leaf_biomass_g_DM.mean, fill=Species),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ Harvest_date+Fertilizer_treatment, scales="free") +
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
    ylab(expression(paste("Leaf biomass (g DM " * plant^-1 * ")")))+
    ylim(0, 7)+
    xlab(expression(paste("Species"))); p3


p4 <- ggplot(sumDF, aes(x=Species, y=Stem_biomass_g_DM.mean, group=Species))+
    geom_errorbar(mapping = aes(x=Species, ymin=Stem_biomass_g_DM.mean-Stem_biomass_g_DM.sd, 
                                ymax=Stem_biomass_g_DM.mean+Stem_biomass_g_DM.sd, col=Species), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(Species, Stem_biomass_g_DM.mean, fill=Species),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ Harvest_date+Fertilizer_treatment, scales="free") +
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
    ylab(expression(paste("Stem biomass (g DM " * plant^-1 * ")")))+
    ylim(0, 4)+
    xlab(expression(paste("Species"))); p4


p5 <- ggplot(sumDF, aes(x=Species, y=Root_biomass_g_DM.mean, group=Species))+
    geom_errorbar(mapping = aes(x=Species, ymin=Root_biomass_g_DM.mean-Root_biomass_g_DM.sd, 
                                ymax=Root_biomass_g_DM.mean+Root_biomass_g_DM.sd, col=Species), 
                  position=position_dodge(width=0.5), width=0.2) +
    geom_point(data=sumDF, aes(Species, Root_biomass_g_DM.mean, fill=Species),
               size=3, pch=21, position=position_dodge(width=0.5))+
    facet_wrap(. ~ Harvest_date+Fertilizer_treatment, scales="free") +
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
    ylab(expression(paste("Root biomass (g DM " * plant^-1 * ")")))+
    ylim(0, 2)+
    xlab(expression(paste("Species"))); p5



pdf("output/results_on_20221107.pdf")
for (i in c(1:5)) {
    plot(get(paste0("p",i)))
}

dev.off()
