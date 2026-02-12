library(rio)
library(survival)
library(dplyr)
library(survminer)
library(ggsurvfit)
library(jskm)

options(datatable.na.strings=c('NULL',''));
demographics <- import("output/csv/patients.csv") %>% mutate(BIRTHDATE=as.Date(BIRTHDATE), DEATHDATE=as.Date(DEATHDATE), 
                                                             timetoevent=coalesce(DEATHDATE,max(DEATHDATE, na.rm=TRUE)) - BIRTHDATE,
                                                             timetoevent=(as.numeric(timetoevent)/365.25),
                                                             censor=!is.na(DEATHDATE), 
                                                             survival=Surv(timetoevent,event=censor))
survivalmodel <-survfit(survival~STATE, demographics)
View(demographics)
table(is.na(demographics$DEATHDATE), demographics$STATE)

plot(survivalmodel,col = c('red','darkgreen'))

survfit2(survival~STATE, demographics) %>%
  ggsurvfit() +
  ylab("Fraction Alive")+
  xlab("Time Since Birth in Years")+
  scale_color_manual(values =c ('red','darkgreen'))

survfit2(survival~STATE, demographics) %>%
  ggsurvfit() + ylab("Fraction Alive") +
  xlab("Years Since Birth") + 
  scale_color_manual(values =c('red', 'darkgreen')) +
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable()

survfit2(Surv(timetoevent,event=censor)~STATE, data=demographics) %>%
  ggsurvplot(data=demographics)
  
require("survival")
fit<- survfit(Surv(timetoevent,event=censor)~STATE, data=demographics)

# Customized survival curves
ggsurvplot(fit, data = lung,
           surv.median.line = "hv", # Add medians survival
           
           # Change legends: title & labels
           legend.title = "Sex",
           legend.labs = c("Male", "Female"),
           # Add p-value and tervals
           pval = TRUE,
           
           conf.int = TRUE,
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           
           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw() # Change ggplot2 theme
)

# Change font size, style and color
#++++++++++++++++++++++++++++++++++++
## Not run: 
# Change font size, style and color at the same time
ggsurvplot(fit, data = lung,  main = "Survival curve",
           font.main = c(16, "bold", "darkblue"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"))


##TABLES
table(demographics$RACE, demographics$ETHNICITY, demographics$GENDER) %>% prop.table %>% data.frame() #prop.table give you proportions

##Plot to compare race by age

ggplot(demographics, aes(x=RACE, y=timetoevent, fill = RACE, color=GENDER)) + #aes--> means that a particular feture in the data will be use for a particular role
  geom_boxplot(outliers = FALSE, notch = TRUE, color="darkblue")+
  geom_jitter(width = 0.1)+
  geom_violin(color="blue", alpha=0.5)
             