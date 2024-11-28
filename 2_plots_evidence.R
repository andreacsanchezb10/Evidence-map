# Load necessary libraries
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdendro)
library(cluster)
library(grid)
library(stringr)

##### PLOTS ------ 
#### Data distribution per year of publication ----
year_total<- data_clean%>%
  group_by( publication_year)%>%
  summarise(n_studies=n_distinct(study_id)) %>%
  ungroup()%>%
  mutate(y_metric_recla_2="Total")%>%
  mutate(y_metric_recla_2=str_to_sentence(y_metric_recla_2),
         publication_year=as.numeric(publication_year))
sum(year_total$n_studies)

year_stage<- data_clean%>%
  group_by( publication_year,y_metric_recla_2)%>%
  summarise(n_studies=n_distinct(study_id)) %>%
  ungroup()%>%
  mutate(y_metric_recla_2=str_to_sentence(y_metric_recla_2),
         publication_year=as.numeric(publication_year))%>%
  mutate(y_metric_recla_2= factor(y_metric_recla_2, levels = c("Awareness","Interest","Adoption" ,"Intensity of adoption", "Dis-adoption")))
sum(year_total$n_studies)
sort(unique(year_stage$y_metric_recla_2))

p_year_publication<-ggplot(year_stage, aes(x = publication_year, y = n_studies,
                                           fill=y_metric_recla_2)) +
  geom_bar(stat="identity")+ 
           #position=position_dodge(), width = 0.6)+
  geom_line(data= year_total, linewidth=0.9)+       # Add lines
  scale_fill_manual(values=c("#009e73", "#f0e442", "#DF5C24", "#265DAB","#7B3A96","black"),
                    name="Stages")+
  labs(
    x = "Publication year",
    y = "Number of studies") +
  labs(color = "")+
  scale_x_continuous(expand = c(0, 1),limits = c(1980,2025),
                     breaks = c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2023)) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,23),
                     breaks = c(0,5,10,15,20)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y =element_text(color="black",size=11, family = "sans"),
    axis.text.x=element_text(color="black",size=11, family = "sans"),
    axis.title =element_text(color="black",size=13, face = "bold",family = "sans"),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    legend.position = c(0.15,0.8),
    legend.background = element_rect(fill="transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_text(color="black",size=13,face = "bold", family = "sans"),
    legend.text=element_text(color="black",size=11, family = "sans"),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))
p_year_publication
#11*8.21

#### Data distribution per stage and metric ----
table(data_clean$y_metric_recla)
y_metric_recla_total<- data_clean%>%
  group_by(y_metric_recla_2, y_metric_recla)%>%
  summarise(n_studies=n_distinct(study_id),
            n_models= n_distinct(study_model_id)) %>%
  ungroup()

stages<- data_clean%>%
  group_by(y_metric_recla_2)%>%
  summarise(n_studies=n_distinct(study_id),
            n_models= n_distinct(study_model_id)) %>%
  ungroup()

y_metric_recla_type<- data_clean%>%
  group_by(y_metric_recla_2, y_change)%>%
  summarise(n_studies=n_distinct(study_id),
            n_models= n_distinct(study_model_id)) %>%
  ungroup()


#### Data distribution per stage and DFS ----
systems<- data_clean%>%
  group_by(m_dp_recla)%>%
  summarise(n_studies=n_distinct(study_id),
            n_models= n_distinct(study_model_id)) %>%
  ungroup()
head(systems)

stages_systems <- data_clean %>%
  group_by(y_metric_recla_2, m_dp_recla) %>%
  summarise(n_studies = n_distinct(study_id)) %>%
  ungroup() %>%
  #rename("from" = "y_metric_recla_2",
  #      "to" = "m_dp_recla") %>%
  mutate(y_metric_recla_2 = str_to_sentence(y_metric_recla_2)) %>%
  arrange(factor(m_dp_recla, levels = c("Rotational grazing", "Agro-aquaculture", "Agro-silvopasture", "Agroforestry", "Combined practices",
                                                 "Cover crops", "Crop rotation", "Embedded seminatural habitats", "Fallow",
                                                 "Intercropping")))

levels_metric_recla_2 <- c("Dis-adoption", "Intensity of adoption", "Adoption", "Interest" ,"Awareness")
levels_m_dp_recla<- c("Agro-aquaculture","Rotational grazing","Fallow","Agro-silvopasture", "Combined practices",
                               "Embedded seminatural habitats","Cover crops", "Intercropping","Crop rotation","Agroforestry")

p_sytems_stages<-
  ggplot(stages_systems, aes(x = n_studies, y = factor(m_dp_recla, levels_m_dp_recla),
                             fill=factor(y_metric_recla_2, levels = levels_metric_recla_2))) +
  geom_bar(stat="identity")+
  scale_fill_manual(
    values=c("#860dcf", "#265DAB" ,"#DF5C24","#f0e442","#009e73"),
    #values=c("grey50","grey50","grey50", "grey50","grey50" ),
    
    name="Adoption stages")+
  labs(x = "Number of studies",
       y = "") +
  scale_x_continuous(expand = c(0, 0),limits = c(0,100),
                     breaks = c(0,20,40,60,80,100)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y =element_text(color="black",size=11, family = "sans"),
    axis.text.x=element_text(color="black",size=11, family = "sans"),
    axis.title =element_text(color="black",size=13, face = "bold",family = "sans"),
    panel.border = element_blank(),
    axis.line = element_line(color="grey50", size= 1),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_line(color="grey50", size= 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))
p_sytems_stages
#10.42*8.21 landscape

studies_multiple_interventions <- data_clean %>%
  group_by(study_id) %>%
  summarise(n_interventions = n_distinct(m_dp_recla)) %>%
  ungroup() %>%
  filter(n_interventions > 1)
length(unique(studies_multiple_interventions$study_id)) #42 studies analysed more than 1 diversified practice

studies_multiple_stages <- data_clean %>%
  group_by(study_id) %>%
  summarise(n_interventions = n_distinct(y_metric_recla_2)) %>%
  ungroup() %>%
  filter(n_interventions > 1)
length(unique(studies_multiple_stages$study_id)) #42 studies analysed more than 1 diversified practice

#### Data distribution per country -----
# Define breaks and labels
library(ggnewscale)

breaks <- c(0,1,6,11,26,51,76)  # Adjusted to ensure it covers all data
labels <- c("0","1-5", "6-10", "11-25", "26-50", "51-75","76-100")
country<- data_clean%>%
  group_by(country,m_un_region)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_models = n_distinct(study_model_id))%>%
  ungroup()%>%
  mutate(n_studies_range = cut(n_studies, 
                               breaks = c(breaks, Inf), 
                               labels = labels, 
                               right = FALSE))

country_subregion<- data_clean%>%
  group_by(country,m_un_region,m_un_subregion)%>%
  dplyr::summarise(n_studies = n_distinct(study_id),
                   n_models = n_distinct(study_model_id))%>%
  ungroup()

world_map <- ggplot2::map_data("world")%>%filter(region != "Antarctica")%>%
  left_join(country, by =  c("region" ="country"))%>%
  mutate_all(~replace(., is.na(.), 0))

world<-
  ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(group = group),colour="grey25",fill="white",
               size = 0.3, show.legend = TRUE) +
  #coord_fixed(projection = "mollweide") +
  geom_polygon(data = subset(world_map, m_un_region %in% "Northern America"), 
               aes(x = long, y = lat, group = group, fill = n_studies_range),
               colour = "grey25", size = 0.3) +
  scale_fill_manual(values = c("#E9FADA","#A4B795"))+
  new_scale_fill() +
  geom_polygon(data = subset(world_map, m_un_region %in% "Latin America and the Caribbean"), 
               aes(x = long, y = lat, group = group, fill = n_studies_range),
               colour = "grey25", size = 0.3)+
  scale_fill_manual(values = c("#F7F0E1"))+
  new_scale_fill() +
  geom_polygon(data = subset(world_map, m_un_region %in% "Africa"), 
               aes(x = long, y = lat, group = group, fill = n_studies_range),
               colour = "grey25", size = 0.3)+ 
  scale_fill_manual(values = c("#F3E7F0", "#E4B5DA", "#B989AE"))+
  new_scale_fill() +
  geom_polygon(data = subset(world_map, m_un_region %in% "Asia"), 
               aes(x = long, y = lat, group = group, fill = n_studies_range),
               colour = "grey25", size = 0.3) +
  scale_fill_manual(values = c("#F1E0D9", "#EEBBA7"))+
  new_scale_fill() +
  geom_polygon(data = subset(world_map, m_un_region %in% "Europe"), 
               aes(x = long, y = lat, group = group, fill = n_studies_range),
               colour = "grey25", size = 0.3) +
  scale_fill_manual(values = c("#D4D3F3"))+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  labs(x = NULL, y = NULL)+
  coord_map("mollweide",xlim=c(-180,180))
world

# Data distribution by region x adoption stage and region x systems
level_mapping <- c("Adoption" = 1,"Intensity of adoption" = 2,"Interest" = 3,"Awareness" = 4,"Dis-adoption" = 5,
                   "Agroforestry" =6,
                   "Crop rotation"  =7,
                   "Intercropping"=8,
                   "Cover crops"  =9, 
                   "Embedded seminatural habitats"=10,
                   "Combined practices"=11,
                   "Agro-silvopasture"  =12,
                   "Fallow" =13,
                   "Rotational grazing"=14,
                   "Agro-aquaculture"=15)
level_region <- c("Africa"=1, "Asia"=2, "Northern America"=3, "Europe"=4,"Latin America and the Caribbean"=6)

regions_stage<-data_clean%>%
  group_by(m_un_region,y_metric_recla_2)%>%
  summarise(n_studies= n_distinct(study_id))%>%
  ungroup()%>%
  complete(m_un_region, y_metric_recla_2, fill = list(n_studies = 0))%>%
  mutate(y_metric_recla_2 = str_to_sentence(y_metric_recla_2) )%>% 
  mutate(order_level = level_mapping[y_metric_recla_2],
         order_region = level_region[m_un_region])%>%
  mutate(n_studies_range = cut(n_studies, 
                               breaks = c(breaks, Inf), 
                               labels = labels, 
                               right = FALSE))%>%
  mutate(combined_fill =  paste(m_un_region, n_studies_range, sep = "_"))

regions_systems<-data_clean%>%
  group_by(m_un_region,m_dp_recla)%>%
  summarise(n_studies= n_distinct(study_id))%>%
  ungroup()%>%
  complete(m_un_region, m_dp_recla, fill = list(n_studies = 0))%>%
  mutate(m_dp_recla = str_to_sentence(m_dp_recla) )%>% 
  mutate(order_level = level_mapping[m_dp_recla],
         order_region = level_region[m_un_region])%>%
  mutate(n_studies_range = cut(n_studies, 
                               breaks = c(breaks, Inf), 
                               labels = labels, 
                               right = FALSE))%>%
  mutate(combined_fill =  paste(m_un_region, n_studies_range, sep = "_"))


colors <- c("Africa_0"= "white" , "Africa_1-5"="#F3E7F0","Africa_6-10" ="#E4B5DA","Africa_11-25"="#B989AE",
            "Africa_26-50"="#975188","Africa_51-75"= "#8E427D","Africa_76-100"="#843272",
            "Asia_0"="white", "Asia_1-5"="#F1E0D9","Asia_6-10"="#EEBBA7","Asia_11-25"="#D49E88",
            "Asia_26-50" ="#CA866A", "Asia_51-75" ="#BD6845","Asia_76-100"="#b5562f",
            "Europe_0"="white","Europe_1-5"="#D4D3F3","Europe_6-10" ="#A29FE5","Europe_11-25"="#807DDC",
            "Europe_26-50"="#5E5AD3", "Europe_51-75"="#3D38CA","Europe_76-100"="#1B15C1",
            "Latin America and the Caribbean_0"="white","Latin America and the Caribbean_1-5"="#F7F0E1",
            "Latin America and the Caribbean_6-10"="#fae8c0",  "Latin America and the Caribbean_11-25"="#F6DCA6",
            "Latin America and the Caribbean_26-50"="#F4D38B","Latin America and the Caribbean_51-75"= "#F2C561","Latin America and the Caribbean_76-100" ="#EEA80F",
            "Northern America_0"="white", "Northern America_1-5"="#E9FADA","Northern America_6-10"= "#C9E1B6",
            "Northern America_11-25"="#B7CCA6","Northern America_26-50" = "#A4B795",
            "Northern America_51-75"="#808E75",     "Northern America_76-100"="#5b6454")

p_regions_stages<-
  ggplot(regions_stage,aes(x=reorder(y_metric_recla_2, order_level), y=reorder(m_un_region, -order_region),
                           fill=combined_fill,color="black"))+
  geom_tile(color = "black") +
  scale_fill_manual(values = colors)+
  geom_text(aes(label=n_studies), color="black",size=3,family = "sans")+
  scale_y_discrete(labels=c("Latin\nAmerica","Europe","Northern\nAmerica","Asia","Africa"))+
 # scale_x_discrete(labels=c("Awareness","Interest","Adoption","Intensity of adoption","Dis-adoption"#                          ))+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(color="black", size=12, family = "sans",angle=90,hjust=1, vjust = 0.4),
        axis.text.y =element_text(color="black", size=12, family = "sans",face = "bold"),
        axis.ticks= element_blank(),
        legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0,b=0.55,l=0.5), "cm"))   
p_regions_stages

p_regions_systems<-
  ggplot(regions_systems,aes(x=reorder(m_dp_recla, order_level), y=reorder(m_un_region, -order_region),
                             fill=combined_fill,color="black"))+
  geom_tile(color = "black") +
  scale_fill_manual(values = colors) +
  geom_text(aes(label=n_studies), color="black",size=3,family = "sans")+
  
  scale_y_discrete(labels=c("Latin\nAmerica","Europe","Northern\nAmerica","Asia","Africa"))+
  scale_x_discrete(labels=c("Agroforestry","Crop rotation","Intercropping","Cover crops",
                            "Embedded\nseminatural habitats","Combined practices","Agro-silvopasture",
                            "Fallow","Rotational grazing","Agro-aquaculture" ))+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(color="black", size=12, family = "sans",angle=90,hjust=1, vjust = 0.4),
        axis.text.y =element_blank(),
        axis.ticks= element_blank(),
        legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))   
p_regions_systems

# Data distribution by regions
regions<-data_clean%>%
  group_by(m_un_region)%>%
  summarise(n_studies= n_distinct(study_id),
            n_models= n_distinct(study_model_id))%>%
  ungroup()%>%
  mutate(total="Total number of studies")

p_regions<-
  ggplot(regions, aes(x=total, y = reorder(m_un_region, n_studies), fill = m_un_region)) +
  #ggplot(regions, aes(x = n_studies, y = reorder(m_un_region, n_studies), fill = m_un_region)) +
  geom_tile(color = "black") +
  #geom_bar(stat = "identity",width = 1,colour="black") +
  labs(x = "Number of studies", y = NULL) +
  scale_y_discrete(expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  #scale_x_continuous(
   # limits = c(0, 105),expand = c(0, 0),
    #breaks = c(0,25,50,75,100),
    #labels = c("0", "25", "50", "75", "100")) +
  scale_fill_manual(
    values = c("#843272","#CA866A","#A29FE5", "#fae8c0",
               "#A4B795"), name="")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color="black", size=13, family = "sans", face = "bold", vjust = -1,
                                    margin = margin(r = 20)),
        axis.text.x = element_text(color="black", size=12, family = "sans"),
        axis.text.y =element_blank(),
        axis.ticks= element_blank(),
        #axis.ticks.x=element_line(colour = "black"),
        legend.position = "none",
        #axis.line.x = element_line(colour = "black"),
        axis.line =element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        strip.placement.x = "outside",
        plot.margin = unit(c(t=0.7,r=0.5,b=3.7,l=0), "cm"))   
p_regions

gridExtra::grid.arrange(p_regions_stages, p_regions_systems,p_regions,
                        ncol = 3,
                        widths = c(1.3, 2,0.4))

#15*6 LANDSCAPE pdf


#legend world graph
label_regions_stage_systems <- data.frame(
  m_un_region = rep(c("Northern America", "Africa", "Asia", "Europe", "Latin America and the Caribbean"), each = 7),
  breaks = rep(c(0, 1, 6, 11, 26, 51, 76),times=5),
  n_studies_range =rep(c("0", "1-5", "6-10", "11-25", "26-50", "51-75", "76-100"),times=5))%>%
  mutate(combined_fill = paste(m_un_region, n_studies_range, sep = "_"))

ggplot(label_regions_stage_systems, aes(x =m_un_region , reorder(n_studies_range, breaks), fill = combined_fill)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors) +
  scale_y_discrete(position = "right") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y.right =  element_text(color="black", size=12, family = "sans",angle=0,vjust = 0.4),
        axis.text.x=element_blank(),
        panel.grid = element_blank())

#
#### Data distribution per factor category per region ----
# Define breaks and labels
breaks <- c(0,1,6,11,26,51,76)  # Adjusted to ensure it covers all data
labels <- c("0","1-5", "6-10", "11-25", "26-50", "51-75","76-100")
#354 models
models<- data_clean %>%
  select(study_model_id) %>%
  distinct(study_model_id) %>%
  summarise(n_models = n_distinct(study_model_id), .groups = "drop")%>%
  mutate(factor_category="Total number of models",
         m_un_region="Total",
         total_models=n_models,
         percentage= n_models,
         n_percentage_range= "total")

factor_models <- data_clean %>%
  select(study_model_id, factor_category) %>%
  distinct(study_model_id, factor_category) %>%
  group_by(factor_category) %>%
  summarise(n_models = n_distinct(study_model_id), .groups = "drop")%>%
  mutate(total_models= 354,
         m_un_region="Total",
         percentage=(n_models / total_models) * 100)%>%
  mutate(percentage = round(percentage,0))

region_models <- data_clean %>%
  select(study_model_id, m_un_region) %>%
  distinct(study_model_id, m_un_region) %>%
  group_by(m_un_region) %>%
  summarise(n_models = n_distinct(study_model_id), .groups = "drop")%>%
  mutate(total_models= sum(n_models))%>%
  mutate(factor_category="Total number of models")%>%
  mutate(percentage= n_models,
         n_percentage_range= "total")

factors_regions<-data_clean%>%
  group_by(m_un_region,factor_category)%>%
  summarise(n_models= n_distinct(study_model_id))%>%
  ungroup()%>%
  complete(m_un_region, factor_category, fill = list(n_models = 0))%>%
  left_join(region_models %>% select(n_models,m_un_region)%>% rename(total_models = n_models), by = "m_un_region")%>%
  mutate(percentage = (n_models / total_models) * 100)%>%  # Calculate percentage with one decimal
  mutate(percentage = round(percentage,0))%>%
  rbind(factor_models)%>%
  mutate(n_percentage_range = cut(percentage, 
                                  breaks = c(breaks, Inf), 
                                  labels = labels, 
                                  right = FALSE))%>%
  rbind(region_models)%>%
  rbind(models)%>%
  mutate(combined_fill =  paste(m_un_region, n_percentage_range, sep = "_"))%>%
  mutate(percentage=if_else(factor_category!="Total number of models",paste0(percentage,"%"), as.character(percentage)))



fill <- c("Africa_0"= "white" , "Africa_1-5"="#F3E7F0","Africa_6-10" ="#E4B5DA","Africa_11-25"="#B989AE",
          "Africa_26-50"="#975188","Africa_51-75"= "#8E427D","Africa_76-100"="#843272",
          "Asia_0"="white", "Asia_1-5"="#F1E0D9","Asia_6-10"="#EEBBA7","Asia_11-25"="#D49E88",
          "Asia_26-50" ="#CA866A", "Asia_51-75" ="#BD6845","Asia_76-100"="#b5562f",
          "Europe_0"="white","Europe_1-5"="#D4D3F3","Europe_6-10" ="#A29FE5","Europe_11-25"="#807DDC",
          "Europe_26-50"="#5E5AD3", "Europe_51-75"="#3D38CA","Europe_76-100"="#1B15C1",
          "Latin America and the Caribbean_0"="white","Latin America and the Caribbean_1-5"="#F7F0E1",
          "Latin America and the Caribbean_6-10"="#fae8c0",  "Latin America and the Caribbean_11-25"="#F6DCA6",
          "Latin America and the Caribbean_26-50"="#F4D38B","Latin America and the Caribbean_51-75"= "#F2C561","Latin America and the Caribbean_76-100" ="#EEA80F",
          "Northern America_0"="white", "Northern America_1-5"="#E9FADA","Northern America_6-10"= "#C9E1B6",
          "Northern America_11-25"="#B7CCA6","Northern America_26-50" = "#A4B795",
          "Northern America_51-75"="#808E75",     "Northern America_76-100"="#5b6454",
          "Total_0"= "white" , "Total_1-5"="#F8F2F2","Total_6-10" ="#E9E4E4","Total_11-25"="#DAD6D7",
          "Total_26-50"="#BDBABB","Total_51-75"= "#969798","Total_76-100"="#818284",
          
          "Total_total"="grey10","Africa_total"="grey10","Asia_total"="grey10","Europe_total"="grey10","Northern America_total"="grey10","Latin America and the Caribbean_total"="grey10")

colour <- c("Africa_0"= "black" , "Africa_1-5"="black","Africa_6-10" ="black","Africa_11-25"="black",
            "Africa_26-50"="black","Africa_51-75"= "black","Africa_76-100"="black",
            "Asia_0"="black", "Asia_1-5"="black","Asia_6-10"="black","Asia_11-25"="black",
            "Asia_26-50" ="black", "Asia_51-75" ="black","Asia_76-100"="black",
            "Europe_0"="black","Europe_1-5"="black","Europe_6-10" ="black","Europe_11-25"="black",
            "Europe_26-50"="black", "Europe_51-75"="black","Europe_76-100"="black",
            "Latin America and the Caribbean_0"="black","Latin America and the Caribbean_1-5"="black",
            "Latin America and the Caribbean_6-10"="black",  "Latin America and the Caribbean_11-25"="black",
            "Latin America and the Caribbean_26-50"="black","Latin America and the Caribbean_51-75"= "black","Latin America and the Caribbean_76-100" ="black",
            "Northern America_0"="black", "Northern America_1-5"="black","Northern America_6-10"= "black",
            "Northern America_11-25"="black","Northern America_26-50" = "black",
            "Northern America_51-75"="black",     "Northern America_76-100"="black",
            "Total_0"= "black" , "Total_1-5"="black","Total_6-10" ="black","Total_11-25"="black",
            "Total_26-50"="black","Total_51-75"= "black","Total_76-100"="black",
            
            "Total_total"="grey90","Africa_total"="grey90","Asia_total"="grey90","Europe_total"="grey90","Northern America_total"="grey90","Latin America and the Caribbean_total"="grey90")
p_factor_regions
sort(unique(factors_regions$factor_category))
p_factor_regions<-
  ggplot(factors_regions,aes(
    x=factor(m_un_region, levels = c("Total","Africa","Asia", "Europe", "Latin America and the Caribbean","Northern America")),
    y=factor(factor_category, levels =c("Total number of models","Other",
                                        
                                        "P&I context\n(Value chain)","P&I context\n(General)",
                                        "P&I context\n(Land tenure)",
                                        "P&I context\n(Financial risk management)","Farmers' behaviour",
                                        "Social capital","Farm management", "Biophysical context","Physical capital", 
                                        "P&I context\n(Access to knowledge)","Financial capital",
                                        "Natural capital","Human capital")),
    fill=combined_fill, color = combined_fill))+
  geom_tile() +
  scale_fill_manual(values = fill) +
  scale_color_manual(values = colour) +
  
  geom_text(aes(label=percentage), color="black",size=4,family = "sans")+
  scale_x_discrete(labels=c("Total","Africa","Asia","Europe", "Latin\nAmerica","Northern\nAmerica"),
                   position = "top")+
  scale_y_discrete(labels=c("Total number of models","Other",
                            "P&I context\n(Value chain)","P&I context\n(General)",
                            "P&I context\n(Land tenure)", 
                           "P&I context\n(Financial risk management)","Farmers' behaviour",
                           "Social capital","Farm management","Biophysical context","Physical capital", 
                           "P&I context\n(Access to knowledge)","Financial capital",
                           "Natural capital","Human capital"))+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(color="black", size=12, family = "sans",hjust=0.5, vjust = 0.5),
        axis.text.y =element_text(color="black", size=11, family = "sans",hjust=1),
        axis.ticks= element_blank(),
        legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))   
p_factor_regions
#12.55*8.21 landscape
## arreglar el color de
factors_regions<-data_clean%>%
  group_by(m_un_region,factor_category)%>%
  summarise(n_models= n_distinct(study_model_id))%>%
  ungroup()%>%
  complete(m_un_region, factor_category, fill = list(n_models = 0))%>%
  left_join(region_models %>% select(n_models,m_un_region)%>% 
              rename(total_models = n_models), by = "m_un_region")%>%
  mutate(percentage = (n_models / total_models) * 100)%>%  # Calculate percentage with one decimal
  mutate(percentage = round(percentage,0))%>%
  rbind(factor_models)%>%
  mutate(n_percentage_range = cut(percentage, 
                                  breaks = c(breaks, Inf), 
                                  labels = labels, 
                                  right = FALSE))%>%
  rbind(region_models)%>%
  rbind(models)%>%
  mutate(combined_fill =  paste(m_un_region, n_percentage_range, sep = "_"))%>%
  mutate(percentage=if_else(factor_category!="Total number of models",paste0(percentage,"%"), as.character(percentage)))

world_map<-world_map%>%
  filter(region!="Australia")%>%
  filter(region!="New Zealand")

ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(group = group),colour="#4d4d4d",fill="#4d4d4d",
               size = 0.3, show.legend = TRUE) +
  coord_fixed() +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  labs(x = NULL, y = NULL)

#### Data distribution per factor category per stage ----
# Define breaks and labels
breaks <- c(0,1,6,11,26,51,76)  # Adjusted to ensure it covers all data
labels <- c("0","1-5", "6-10", "11-25", "26-50", "51-75","76-100")

stages_models <- data_clean %>%
  select(study_model_id, y_metric_recla_2) %>%
  distinct(study_model_id, y_metric_recla_2) %>%
  group_by(y_metric_recla_2) %>%
  summarise(n_models = n_distinct(study_model_id), .groups = "drop")%>%
  mutate(total_models= sum(n_models))%>%
  mutate(factor_category="Total number of models")%>%
  mutate(percentage= n_models,
         n_percentage_range= "total")

factors_stages<-data_clean%>%
  group_by(y_metric_recla_2,factor_category)%>%
  summarise(n_models= n_distinct(study_model_id))%>%
  ungroup()%>%
  complete(y_metric_recla_2, factor_category, fill = list(n_models = 0))%>%
  left_join(stages_models %>% select(n_models,y_metric_recla_2)%>% rename(total_models = n_models), by = "y_metric_recla_2")%>%
  mutate(percentage = (n_models / total_models) * 100)%>%  # Calculate percentage with one decimal
  mutate(percentage = round(percentage,0))%>%
  mutate(n_percentage_range = cut(percentage, 
                                  breaks = c(breaks, Inf), 
                                  labels = labels, 
                                  right = FALSE))%>%
  rbind(stages_models)%>%
  mutate(combined_fill =  paste(y_metric_recla_2, n_percentage_range, sep = "_"))%>%
  mutate(percentage=if_else(factor_category!="Total number of models",paste0(percentage,"%"), as.character(percentage)))
sort(unique(factors_stages$combined_fill))

fill <- c("awareness_0"= "white","awareness_1-5"="#D5EEE1","awareness_6-10" ="#ACDCC2", "awareness_11-25"="#82CBA4" ,
          "awareness_26-50"="#58BA85" ,"awareness_51-75"="#2FA867"  ,"awareness_76-100"="#059748" ,"awareness_total"="grey10",
          "interest_0"= "white","interest_1-5"="#FDFBE0","interest_6-10" ="#FAF6C0", "interest_11-25"="#F8F2A1" ,
          "interest_26-50"="#F5ED81" ,"interest_51-75"="#F3E962"  ,"interest_76-100"="#F0E442" ,"interest_total"="grey10",
          "adoption_0"= "white","adoption_1-5"="#FAE4DB","adoption_6-10" ="#F4C9B6", "adoption_11-25"="#EFAE92" ,
          "adoption_26-50"="#EA926D" ,"adoption_51-75"="#E47749"  ,"adoption_76-100"="#DF5C24" ,"adoption_total"="grey10",
          "intensity of adoption_0"= "white","intensity of adoption_1-5"="#DBE4F1","intensity of adoption_6-10" ="#B7C9E3", "intensity of adoption_11-25"="#93AED5" ,
          "intensity of adoption_26-50"="#6E93C7" ,"intensity of adoption_51-75"="#4A78B9"  ,"intensity of adoption_76-100"="#265DAB" ,"intensity of adoption_total"="grey10",
          "dis-adoption_0"= "white","dis-adoption_1-5"="#E9DEEE","dis-adoption_6-10" ="#D3BDDC", "dis-adoption_11-25"="#BD9DCB" ,
          "dis-adoption_26-50"="#A77CB9" ,"dis-adoption_51-75"="#915BA8"  ,"dis-adoption_76-100"="#7B3A96" ,"dis-adoption_total"="grey10")

colour <-  c("awareness_0"= "black","awareness_1-5"="black","awareness_6-10" ="black", "awareness_11-25"="black" ,
             "awareness_26-50"="black" ,"awareness_51-75"="black"  ,"awareness_76-100"="black" ,"awareness_total"="grey90",
             "interest_0"= "black","interest_1-5"="black","interest_6-10" ="black", "interest_11-25"="black" ,
             "interest_26-50"="black" ,"interest_51-75"="black"  ,"interest_76-100"="black" ,"interest_total"="grey90",
             "adoption_0"= "black","adoption_1-5"="black","adoption_6-10" ="black", "adoption_11-25"="black" ,
             "adoption_26-50"="black" ,"adoption_51-75"="black"  ,"adoption_76-100"="black" ,"adoption_total"="grey90",
             "intensity of adoption_0"= "black","intensity of adoption_1-5"="black","intensity of adoption_6-10" ="black", "intensity of adoption_11-25"="black" ,
             "intensity of adoption_26-50"="black" ,"intensity of adoption_51-75"="black"  ,"intensity of adoption_76-100"="black" ,"intensity of adoption_total"="grey90",
             "dis-adoption_0"= "black","dis-adoption_1-5"="black","dis-adoption_6-10" ="black", "dis-adoption_11-25"="black" ,
             "dis-adoption_26-50"="black" ,"dis-adoption_51-75"="black"  ,"dis-adoption_76-100"="black" ,"dis-adoption_total"="grey90")

p_factor_stages<-
  ggplot(factors_stages,aes(
    x=factor(y_metric_recla_2, levels = c("awareness","interest","adoption","intensity of adoption","dis-adoption")),
    y=factor(factor_category, levels =c("Total number of models","Other","P&I context\n(Value chain)","P&I context\n(General)",
                                        "P&I context\n(Land tenure)","Farm management", 
                                        "P&I context\n(Financial risk management)","Farmers' behaviour",
                                        "Social capital","Biophysical context","Physical capital", 
                                        "P&I context\n(Access to knowledge)","Financial capital",
                                        "Natural capital","Human capital")),
    fill=combined_fill, color = combined_fill))+
  geom_tile() +
  scale_fill_manual(values = fill) +
  scale_color_manual(values = colour) +
  
  geom_text(aes(label=percentage), color="black",size=4,family = "sans")+
  scale_x_discrete(labels=c("Awareness","Interest","Adoption","Intensity of adoption","Dis-adoption"),
                   position = "top")+
  scale_y_discrete(labels=c("Total number of models","Other","P&I context\n(Value chain)","P&I context\n(General)",
                            "P&I context\n(Land tenure)","Farm management", 
                            "P&I context\n(Financial risk management)","Farmers' behaviour",
                            "Social capital","Biophysical context","Physical capital", 
                            "P&I context\n(Access to knowledge)","Financial capital",
                            "Natural capital","Human capital"))+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(color="black", size=12, family = "sans",hjust=0.5, vjust = 0.5),
        axis.text.y =element_text(color="black", size=11, family = "sans",hjust=1),
        axis.ticks= element_blank(),
        legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0,b=0.5,l=0), "cm"))   
p_factor_stages
#12.55*8.21 landscape


#### Comparison profitability of diversified farming systems and adoption studies -----
profitability_dfs <- read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/Kamau/data/mean_profitability_dfs.csv", 
                              sep=",", header = TRUE)%>%
  select(NAME_LONG, MEAN, REGION_UN, SUBREGION)%>%
  filter(!is.na(MEAN))%>%
  mutate(NAME_LONG = ifelse(NAME_LONG == "C\x99te d'Ivoire","Ivory Coast",
                            if_else(NAME_LONG=="Republic of the Congo","Republic of Congo",
                                    if_else(NAME_LONG== "United States", "USA",
                                            if_else(NAME_LONG=="Flemish Region","Belgium",
                                                    if_else(NAME_LONG== "Russian Federation","Russia",
                                                            if_else(NAME_LONG== "Syrian Arab Republic","Syria",
                                                                    if_else(NAME_LONG== "Lao PDR","Laos",
                                                                            if_else(NAME_LONG== "Republic of Korea","South Korea",
                                                                                    if_else(NAME_LONG=="Dem. Rep. Korea", "North Korea",
                                                                                            if_else(NAME_LONG=="Kingdom of eSwatini","Swaziland",
                                                                                                    if_else(NAME_LONG=="Federation of Bosnia and Herzegovina","Bosnia and Herzegovina",
                                                                                                            NAME_LONG))))))))))))

sort(unique(profitability_dfs$NAME_LONG))
#204 countries

#Kamau et al
#thresholds
#Balance training omission, predicted area and threshold value Cloglog threshold = 0.0755
#10 percentile training presence Cloglog threshold = 0.1995
#Maximum training sensitivity plus specificity Cloglog threshold = 0.2978
#Equal training sensitivity and specificity Cloglog threshold = 0.3185
breaks <- c(0,0.01021082, 0.0755, 0.1995, 0.2978, 0.3185, Inf)  # Ensure no duplication of Inf
labels <- c("0","0.01-0.0755", "0.0755-0.1995", "0.1995-0.2978", "0.2978-0.3185", "0.3185+")

world_profitability <- ggplot2::map_data("world") %>%
  filter(region != "Antarctica") %>%
  mutate(region= if_else(subregion%in% c("Scotland","Northern Ireland","Wales","Great Britain","Isle of Wight"), subregion, region))%>%
  mutate(region= if_else(region%in% c("Great Britain","Isle of Wight"), "England", region))%>%
  left_join(country, by = c("region" = "country")) %>%
  left_join(profitability_dfs, by = c("region" = "NAME_LONG")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(profitability_range = cut(MEAN, 
                                   breaks = breaks,  # Removed the extra Inf
                                   labels = labels, 
                                   right = FALSE))%>%
  mutate(studies_profitability_range= paste(n_studies_range,"_",profitability_range,sep=""))



subregions_plot <-ggplot2::map_data("world") %>%
  filter(region != "Antarctica") %>%
  left_join(UN_region,by=c("region" = "Country_Name"))

subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Saint Pierre and Miquelon"), "Northern America", subregions_plot$m_un_subregion)


subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Venezuela","Falkland Islands"), "South America", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Antigua","Barbuda","Bonaire",
                                                                     "Curacao",
                                                                     "Dominican Republic","Grenadines","Saint Vincent",
                                                                     "Nevis","Saint Kitts" ,
                                                                     "Virgin Islands","Tobago",'Trinidad'), "Caribbean", subregions_plot$m_un_subregion)

subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Czech Republic","Russia"), "Eastern Europe", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Kosovo" ,"North Macedonia"),"Southern Europe", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Jersey","Guernsey"),"Western Europe", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Faroe Islands"),"Northern Europe", subregions_plot$m_un_subregion)

subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Brunei","Laos"), "South-eastern Asia", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("South Korea","Taiwan", "North Korea"), "Eastern Asia", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Syria" ,"Palestine","South Georgia"),"Western Asia", subregions_plot$m_un_subregion)

subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Republic of Congo","Central African Republic"), "Middle Africa", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Comoros" ,"Mayotte" ),"Eastern Africa", subregions_plot$m_un_subregion)

subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Gambia","Ivory Coast","Cape Verde"),"Western Africa", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Micronesia"),"Micronesia", subregions_plot$m_un_subregion)
subregions_plot$m_un_subregion <- ifelse(subregions_plot$region %in% c("Norfolk Island"),"Melanesia", subregions_plot$m_un_subregion)
subregions_plot<-subregions_plot%>%
  filter(!is.na(m_un_subregion))
group_by(m_un_subregion) %>%
  summarise(lat = mean(lat),
            long = mean(long)) %>%
  ungroup()


table(world_profitability$n_studies)
table(world_profitability$profitability_range)
sort(unique(world_profitability$studies_profitability_range))
sort(unique(world_profitability$n_studies_range))
profitability_fill<-  c("0_0"="grey99",
                        "0_0.01-0.0755"="grey99",
                        "0_0.0755-0.1995" ="#FEC9C0",
                        "0_0.1995-0.2978"="#FD9382",
                        "0_0.2978-0.3185"="#FC5C43",
                        "0_0.3185+"  ="#fb2604",
                        
                        "1-5_0"  ="#a9cadd",
                        "1-5_0.01-0.0755"="#a9cadd",
                        "1-5_0.0755-0.1995"="#C9BDBB",
                        "1-5_0.1995-0.2978"="#D49882",
                        "1-5_0.2978-0.3185" ="#E07448",
                        "1-5_0.3185+"="#EC5010",
                        
                        "6-10_0"="#76b7e1",
                        "6-10_0.01-0.0755"="#76b7e1",
                        "6-10_0.0755-0.1995"="#95B1B6",
                        "6-10_0.1995-0.2978"="#AB9E82",
                        "6-10_0.2978-0.3185" ="#C48C4D",
                        "6-10_0.3185+"="#d48028",
                        
                        "11-25_0"="#4ba5e3",
                        "11-25_0.01-0.0755"="#4ba5e3",
                        "11-25_0.0755-0.1995"="#60A4B1",
                        "11-25_0.1995-0.2978"="#81A381",
                        "11-25_0.2978-0.3185" ="#A7A352",
                        "11-25_0.3185+"="#cba82a",
                        
                        "26-50_0"="#0f7fe1",
                        "26-50_0.01-0.0755"="#0f7fe1",
                        "26-50_0.0755-0.1995"="#2b98ac",
                        "26-50_0.1995-0.2978"="#58a881",
                        "26-50_0.2978-0.3185" ="#8bbb57",
                        "26-50_0.3185+"="#c0cf34",
                        
                        "51-75_0"="#0f7fe1",
                        "51-75_0.01-0.0755"="#0f7fe1",
                        "51-75_0.0755-0.1995"="#2b98ac",
                        "51-75_0.1995-0.2978"="#58a881",
                        "51-75_0.2978-0.3185" ="#8bbb57",
                        "51-75_0.3185+"="#c0cf34")


plot_profitability_studies<-
  
  ggplot(data = world_profitability, aes(x = long, y = lat, group = group)) +
  
  geom_polygon(aes(group = group,fill= studies_profitability_range),colour="grey35",
               size = 0.3, show.legend = TRUE) +
 # coord_fixed() +
  # geom_polygon(aes(group = group),colour="grey25",fill="grey",
  #             size = 0.3, show.legend = TRUE) 
  scale_fill_manual(values=profitability_fill)+
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  labs(x = NULL, y = NULL)+
  coord_map("mollweide",xlim=c(-180,180))

plot_profitability_studies 


legend_profitability <- data.frame(
  studies_profitability_range = 
    c( "0_0.01-0.0755", "0_0.0755-0.1995","0_0.1995-0.2978", "0_0.2978-0.3185", "0_0.3185+",
       "1-5_0.01-0.0755", "1-5_0.0755-0.1995","1-5_0.1995-0.2978", "1-5_0.2978-0.3185", "1-5_0.3185+",
       "6-10_0.01-0.0755", "6-10_0.0755-0.1995", 
       "6-10_0.1995-0.2978", "6-10_0.2978-0.3185", "6-10_0.3185+",
       "11-25_0.01-0.0755", "11-25_0.0755-0.1995", 
       "11-25_0.1995-0.2978", "11-25_0.2978-0.3185", "11-25_0.3185+",
       "26-50_0.01-0.0755", "26-50_0.0755-0.1995", 
       "26-50_0.1995-0.2978", "26-50_0.2978-0.3185", "26-50_0.3185+"),
  y = rep(c(0, 1, 6, 11, 26), each = 5),
  x = rep(c( 0.01, 0.0755, 0.1995, 0.2978, 0.3185), times = 5))


legend_profitability_studies<-ggplot(legend_profitability, aes(x = factor(x), y = factor(y), fill = studies_profitability_range)) +
  geom_tile(colour="grey25",show.legend = F) +
  scale_fill_manual(values = profitability_fill) +
  labs(y = "Number of studies", x = "Suitability for profitable diversified farming systems") +
  scale_y_discrete(labels =  c("0" = "0", "1" = "1-5", "6" = "6-10", "11" = "11-25", "26" = "26-50"))+
  scale_x_discrete(labels =  c("0.0100" = "0-0.0755", "0.0755" = "0.0755-0.1995", 
                               "0.1995" = "0.1995-0.2978", "0.2978" = "0.2978-0.3185", "0.3185" = ">0.3185"))+
  theme(axis.title = element_text(color="black", size=16, family = "sans",face = "bold"),
        axis.text = element_text(color="black", size=14, family = "sans"),
        axis.ticks= element_blank(),
        legend.position = "none",
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))  

legend_profitability_studies                       
#13*5.5
#9.32*7.68
world_profitability.p<-world_profitability%>%
  filter(REGION_UN!="0")

ggplot(world_profitability.p, aes(x=MEAN, y=n_studies,color=studies_profitability_range,shape=REGION_UN )) +
  geom_point(size=7) +
  geom_text(aes(x=MEAN+0.02,label=region),check_overlap = TRUE)+
  labs(y = "Number of studies", x = "Suitability for profitable diversified farming systems") +
  scale_y_continuous(breaks= c(0,10,20,30,40,50,60,70),
                     expand = c(0.2,0))+
  scale_x_continuous(breaks= c(0,0.07,0.19,0.29,0.31,0.4,0.5,0.6,0.7),
                     limits = c(0, 0.7),
                     expand = c(0, 0)) +
  scale_shape_manual(values=c(19, 17, 18,15,7,8))+
  scale_color_manual(values = profitability_fill, guide="none") +
  
  theme(axis.title = element_text(color="black", size=16, family = "sans",face = "bold"),
        axis.text = element_text(color="black", size=14, family = "sans"),
        #axis.ticks= element_blank(),
        legend.position = c(0.9,0.5,0.1,1),
        axis.line = element_line(color="black"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))  


subregions<-data_clean%>%
  group_by(m_un_subregion)%>%
  summarise(#n_models= n_distinct(study_model_id),
    n_studies= n_distinct(study_id))%>%
  ungroup()%>%
  filter(!is.na(m_un_subregion))
subregions$m_un_subregion[subregions$m_un_subregion %in% "South-eastern Asia"] <- "South-Eastern Asia"

sbreaks <- c(0,1,6,11,26,51,76)  # Adjusted to ensure it covers all data
slabels <- c("0","1-5", "6-10", "11-25", "26-50", "51-75","76-100")
breaks <- c(0,0.01021082, 0.0755, 0.1995, 0.2978, 0.3185, Inf)  # Ensure no duplication of Inf
labels <- c("0","0.01-0.0755", "0.0755-0.1995", "0.1995-0.2978", "0.2978-0.3185", "0.3185+")

subregion_profitability_dfs<- profitability_dfs%>%
  mutate(SUBREGION= if_else(REGION_UN=="Oceania","Oceania",SUBREGION))%>%
  group_by(SUBREGION,REGION_UN)%>%
  mutate(mean_subregion= mean(MEAN))%>%
  ungroup()%>%
  select(REGION_UN,SUBREGION,mean_subregion)%>%
  distinct()%>%
  left_join(subregions, by = c("SUBREGION"="m_un_subregion"))

subregion_profitability_dfs$n_studies[subregion_profitability_dfs$SUBREGION %in% c("Oceania","Central Asia","Caribbean","Northern Europe")] <- 0
subregion_profitability_dfs<- subregion_profitability_dfs%>%
  mutate(profitability_range = cut(mean_subregion, 
                                   breaks = breaks,  # Removed the extra Inf
                                   labels = labels, 
                                   right = FALSE))%>%
  mutate(n_studies_range = cut(n_studies, 
                               breaks = c(sbreaks, Inf), 
                               labels = slabels, 
                               right = FALSE))%>%
  mutate(studies_profitability_range= paste(n_studies_range,"_",profitability_range,sep=""))%>%
  mutate(REGION_UN= if_else(SUBREGION=="Northern America", "Northern America",
                            if_else(REGION_UN=="Americas","Latin America and the Caribbean",
                                    REGION_UN)))

ggplot(subregion_profitability_dfs, aes(x=mean_subregion, y=n_studies,color=studies_profitability_range,shape=REGION_UN )) +
  geom_point(size=7) +
  geom_text(aes(x=mean_subregion+0.04,label=SUBREGION),check_overlap = TRUE)+
  labs(y = "Number of studies", x = "Suitability for profitable diversified farming systems") +
  scale_y_continuous(breaks= c(0,10,20,30,40,50,60,70),
                     expand = c(0.2,0))+
  scale_x_continuous(breaks= c(0,0.07,0.19,0.29,0.31,0.4,0.5,0.6,0.7),
                     limits = c(0, 0.7),
                     expand = c(0, 0)) +
  scale_shape_manual(values=c(19, 17, 18,15,7,8))+
  scale_color_manual(values = profitability_fill, guide="none") +
  
  theme(axis.title = element_text(color="black", size=16, family = "sans",face = "bold"),
        axis.text = element_text(color="black", size=14, family = "sans"),
        #axis.ticks= element_blank(),
        legend.position = c(0.9,0.5,0.1,1),
        axis.line = element_line(color="black"),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.major.x = element_line(color = "grey75",
                                          size = 0.5),
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))  

#13*5.5
#18*8.21
landacape

####################################################################



### Hierarchical analysis by different regions
region.factor_category<- data_clean%>%
  select(study_id, study_model_id, factor_category, m_un_region)

names(region.factor_category)
head(region.factor_category)
length(sort(unique(region.factor_category$study_model_id))) #354
length(sort(unique(region.factor_category$factor_category))) #14

region.factor_category_total<- region.factor_category%>%
  group_by(factor_category,m_un_region)%>%
  summarise(n_studies= n_distinct(study_id),
            n_models= n_distinct(study_model_id))%>%
  ungroup()

factor<- data_clean%>%
  mutate(x_metric_recla= if_else(is.na(x_metric_recla),"Other",x_metric_recla ))%>%
  group_by(study_model_id,)%>%
  summarise(n_factors= n_distinct(x_metric_raw))%>%
  ungroup()

region.factor_combinations<-region.factor_category%>%
  distinct(study_model_id, factor_category,m_un_region, .keep_all = TRUE)%>%
  select(study_model_id, m_un_region,factor_category)

sort(unique(factor_combinations$factor_category))
region.pivoted_data <- region.factor_combinations %>%
  spread(key = factor_category, value = factor_category) %>%
  mutate(across(-c(study_model_id,m_un_region), ~ replace(., !is.na(.), 1))) %>%
  mutate(across(-c(study_model_id,m_un_region), ~ replace(., is.na(.), 0)))%>%
  
  
  unique_regions <- unique(region.pivoted_data$m_un_region)
unique_regions
# Create a function to plot dendrograms for each region
plot_dendrogram <- function(region_name) {
  # Filter data for the current region
  region_data <- region.pivoted_data %>%
    filter(m_un_region == region_name)
  
  # Select relevant columns for analysis
  region_selected_data <- region_data[, 3:16]
  
  # Create a binary matrix for clustering
  region_binary_matrix <- as.matrix(region_selected_data)
  
  # Compute the distance matrix using the count of co-occurrences
  comb_dist <- dist(t(region_binary_matrix), method = "binary")
  
  # Perform hierarchical clustering
  hclust_res <- hclust(comb_dist, method = "ward.D2")
  
  # Create a plot and save it
  plot(as.dendrogram(hclust_res), 
       main = paste("Dendrogram for", region_name),
       xlab = "Distance", 
       ylab = "Clusters", 
       horiz = TRUE)
}

# Loop through each region and create a dendrogram plot
for (region in unique_regions) {
  plot_dendrogram(region)
}

sum(region_models$total_models)
# Count the number of study_model_id that include each factor_category and calculate the percentage
model_category_counts <- data_clean %>%
  select(study_model_id, factor_category, m_un_region) %>%
  distinct(study_model_id, factor_category, m_un_region, .keep_all = TRUE) %>%
  group_by(factor_category, m_un_region) %>%
  summarise(n_models = n_distinct(study_model_id), .groups = "drop") %>%
  ungroup() %>%
  rename(pair1 = factor_category) %>%
  mutate(pair2 = pair1) %>%
  # Join with the total models counts
  left_join(region_models, by = "m_un_region")%>% 
  mutate(percentage = (n_models / total_models) * 100) # Calculate percentage

filter(is.na(factor_category))

model_category_counts


model_factor_category_filtered <- data_clean %>%
  select(study_model_id, factor_category,m_un_region)%>%
  distinct(study_model_id, factor_category,m_un_region, .keep_all = TRUE)%>%
  group_by(study_model_id) %>%
  filter(n() > 1)


models_pairwise_combinations <- model_factor_category_filtered %>%
  summarise(pairs = list(combn(factor_category, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair1 = map_chr(pairs, 1),
         pair2 = map_chr(pairs, 2)) %>%
  select(-pairs)%>%
  left_join(model_factor_category_filtered %>% select(study_model_id, m_un_region), by = "study_model_id")%>%
  group_by(pair1,pair2,m_un_region)%>%
  summarise(n_models= n_distinct(study_model_id))%>%
  ungroup()%>%
  left_join(total_models_counts,by="m_un_region")%>%
  mutate(percentage = (n_models / total_models) * 100)%>%  # Calculate percentage with one decimal
  rbind(model_category_counts)%>%
  mutate(percentage = round(percentage,1))%>%
  complete(pair1, pair2,m_un_region, fill = list(percentage = 0))%>%
  mutate(n_percentage_range = cut(percentage, 
                                  breaks = c(breaks, Inf), 
                                  labels = labels, 
                                  right = FALSE))%>%
  mutate(combined_fill =  paste(m_un_region, n_percentage_range, sep = "_"))
sort(unique(models_pairwise_combinations$combined_fill))

regions <- unique(models_pairwise_combinations$m_un_region)
regions

colors <- c("Africa_0"= "white" , "Africa_1-5"="#F3E7F0","Africa_6-10" ="#E4B5DA","Africa_11-25"="#B989AE",
            "Africa_26-50"="#975188","Africa_51-75"= "#8E427D","Africa_76-100"="#843272",
            "Asia_0"="white", "Asia_1-5"="#F1E0D9","Asia_6-10"="#EEBBA7","Asia_11-25"="#D49E88",
            "Asia_26-50" ="#CA866A", "Asia_51-75" ="#BD6845","Asia_76-100"="#b5562f",
            "Europe_0"="white","Europe_1-5"="#D4D3F3","Europe_6-10" ="#A29FE5","Europe_11-25"="#807DDC",
            "Europe_26-50"="#5E5AD3", "Europe_51-75"="#3D38CA","Europe_76-100"="#1B15C1",
            "Latin America and the Caribbean_0"="white","Latin America and the Caribbean_1-5"="#F7F0E1",
            "Latin America and the Caribbean_6-10"="#fae8c0",  "Latin America and the Caribbean_11-25"="#F6DCA6",
            "Latin America and the Caribbean_26-50"="#F4D38B","Latin America and the Caribbean_51-75"= "#F2C561",
            "Latin America and the Caribbean_76-100" ="#EEA80F",
            "Northern America_0"="white", "Northern America_1-5"="#E9FADA","Northern America_6-10"= "#C9E1B6",
            "Northern America_11-25"="#B7CCA6","Northern America_26-50" = "#A4B795",
            "Northern America_51-75"="#808E75",     "Northern America_76-100"="#5b6454")

# Loop through each region and create a heatmap
for (region in regions) {
  # Filter data for the current region
  region_data <- models_pairwise_combinations %>%
    filter(m_un_region == region)
  
  # Create the heat map for each region
  p <- ggplot(region_data, aes(x = pair2 , y = pair1, fill = combined_fill)) +
    geom_tile(color = "black") +
    scale_fill_manual(values = colors)+
    geom_text(aes(label=percentage), color="black",size=3,family = "sans")+
    scale_x_discrete(position = "top")+
    labs(title = paste("Factor Categories Co-occurrences in", region),
         x = "Factor Category 1",
         y = "Factor Category 2") +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(color="black", size=12, family = "sans",angle=45,hjust=0),
          axis.text.y =element_text(color="black", size=12, family = "sans"),
          axis.ticks= element_blank(),
          legend.position = "none",
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin = unit(c(t=0.5,r=0,b=0.55,l=0.5), "cm")) 
  
  # Print the plot
  print(p)
}


###############################





#
region_selected_data <-  region.pivoted_data%>%
  filter(m_un_region=="Africa")%>%
  select(3:16)
most_common_2 <- find_most_common_combinations(region_selected_data, 2)
most_common_3 <- find_most_common_combinations(region_selected_data, 3)
most_common_4 <- find_most_common_combinations(region_selected_data, 14)


#### Data distribution per factor class ----
factor_category<- data_clean%>%
  select(study_id, study_model_id, factor_category)%>%
  mutate(factor_category= if_else(is.na(factor_category),"Other",factor_category ))

names(factor_category)
head(factor_category)
length(sort(unique(factor_category$study_model_id))) #354
length(sort(unique(factor_category$factor_category))) #14

factor_category_models<- factor_category%>%
  group_by(factor_category)%>%
  summarise(n= n_distinct(study_model_id))%>%
  ungroup()%>%
  mutate(variable="models")

factor_category_studies<- factor_category%>%
  group_by(factor_category)%>%
  summarise(n= n_distinct(study_id))%>%
  ungroup()%>%
  mutate(variable="studies")

factor_category_total<-rbind(factor_category_models,factor_category_studies)

factor_category_total<- factor_category%>%
  group_by(factor_category)%>%
  summarise(n_studies= n_distinct(study_id),
            n_models= n_distinct(study_model_id))%>%
  ungroup()

factor<- data_clean%>%
  mutate(x_metric_recla= if_else(is.na(x_metric_recla),"Other",x_metric_recla ))%>%
  group_by(study_model_id)%>%
  summarise(n_factors= n_distinct(x_metric_raw))%>%
  ungroup()

factor_combinations<-factor_category%>%
  distinct(study_model_id, factor_category, .keep_all = TRUE)%>%
  select(study_model_id, factor_category)
  
sort(unique(factor_combinations$factor_category))

pivoted_data <- factor_combinations %>%
  spread(key = factor_category, value = factor_category) %>%
  mutate(across(-study_model_id, ~ replace(., !is.na(.), 1))) %>%
  mutate(across(-study_model_id, ~ replace(., is.na(.), 0)))



# Assuming your data is already loaded into a dataframe called pivoted_data
# Select columns 2 to 16
selected_data <- pivoted_data[, 2:15]

head(selected_data)

# Create a binary matrix for clustering based on the selected data
binary_matrix <- as.matrix(selected_data)
binary_matrix
head(b)
# Compute the distance matrix using the count of co-occurrences
comb_dist <- dist(t(binary_matrix), method = "binary")

# Perform hierarchical clustering
hclust_res <- hclust(comb_dist, method = "ward.D2")

#Dendrogram plot
plot(as.dendrogram(hclust_res), horiz = TRUE)
#rect.hclust(hclust_res, k = 6, border = "red")
par(mfrow = c(1, 1))  # Reset margins after the plot if needed
par(mar = c(2.5, 0.5, 0.5,11))

# Cut the dendrogram into clusters and count the number of models per cluster
cutree_res <- cutree(hclust_res, k = 14)  # You can change the number of clusters (k) as needed




##Sunburst plot OLD

library(dplyr)
library(tidyr)
library(sunburstR)

# Custom order of categories (adjust this to your specific order)
desired_order <- c("Human capital","Natural capital", "Financial capital",
                   "Physical capital", "Biophysical context",
                   
                   "P&I context\n(Financial risk management)",
                   "Social capital",  "P&I context\n(Land tenure security)","Farmers' behaviour", 
                   "P&I context\n(Access to knowledge)","Other",
                   "Farm management","P&I context\n(NA)", "P&I context\n(Value chain)")

# Create category_order dataframe using the custom order
category_order <- selected_data %>%
  summarise(across(everything(), ~ sum(. == "1"))) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "count") 
  #mutate(category = factor(category, levels = desired_order)) %>%
  #arrange(category)

# Reorder columns in selected_data based on the custom order
ordered_selected_data <- selected_data %>%
  select(all_of(category_order$category))

# Create the paths in the reordered data for the sunburst plot
df_sunburst <- ordered_selected_data %>%
  mutate(path = apply(ordered_selected_data, 1, function(row) {
    paste(names(ordered_selected_data)[row == "1"], collapse = "-")
  })) %>%
  group_by(path) %>%
  summarise(count = n()) %>%
  ungroup()
head(df_sunburst)

sunburst_plot <- sunburst(df_sunburst, count = F,legend=T)
sunburst_plot
head(df_sunburst)
#arreglar, los colores no funcionan para p&i, creo que es por el &
sunburst(
  df_sunburst, 
  count = FALSE,
  legend = TRUE,
  colors = htmlwidgets::JS(
    "function(name, depth) {
      var colorMap = {
        'Human capital': '#7876B1FF', 
        'Natural capital': '#4DBBD5FF', 
        'Financial capital': '#ec98ff', 
        'Physical capital': '#3C5488FF', 
        'Biophysical context': '#fac400',
        'P&I context (Financial risk management)': '#A5D6A6FF', 
        'Social capital': '#6F99ADFF', 
        'P&I context (Land tenure security)': '#66BA6AFF',
        'Farmers behaviour': '#ff6142', 
        'P&I context (Access to knowledge)': '#20854EFF', 
        'Other': '#c7c7c7',
        'Farm management': '#8c564b', 
        'P&I context (NA)': '#4DBBD5FF', 
        'P&I context (Value chain)': '#7876B1FF'
      };
      return colorMap[name] || '#cccccc'; // default color if name not in colorMap
    }"
  )
)

#500*706

# Load plotly
library(plotly)

# Create the bar plot with overlapping bars, with study bars thinner
# Define a vector of colors for the model bars
model_colors <- c("#7f7f7f","#c7c7c7","#f7b6d2","#8c564b","#ff9896","#e377c2", "#ff7f0e","#9467bd", "#aec7e8",
                  "#c49c94","#c5b0d5","#d62728","#ffbb78","#98df8a","#2ca02c") # Customize these colors

# Ensure the color vector matches the number of categories
model_colors <- rep(model_colors, length.out = nrow(sorted_factor_category_total))

# First, sort the data based on the number of models in ascending order
sorted_factor_category_total <- factor_category_total %>%
  arrange(n_models)

# Create the bar plot with custom colors for model bars and a grey color for study bars
bar_plot <- plot_ly() %>%
  add_trace(data = sorted_factor_category_total,
            y = ~factor_category,
            x = ~n_models,
            type = 'bar',
            name = 'Number of Models',
            marker = list(color = model_colors,  # Set custom colors for each model bar
                          line = list(color = "white", width = 1)),
            width = 0.8 ) %>%
  add_trace(data = sorted_factor_category_total,
            y = ~factor_category,
            x = ~n_studies,
            type = 'bar',
            name = 'Number of Studies',
            marker = list(color = 'rgb(64,64,64)',  # Explicitly set the color to grey25 (as rgb)
                          line = list(color = 'rgb(64,64,64)', width = 2)),
            width = 0.4 ) %>%
  layout(
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = sorted_factor_category_total$factor_category,
      linecolor = "black",  # Set the y-axis line color to black
      tickfont = list(family = "sans", size = 13, color = "black")  # Customize the y-axis tick labels
    ),
    xaxis = list(
      title = "Count",
      linecolor = "black",  # Set the x-axis line color to black
      tickfont = list(family = "sans", size = 14, color = "black"),  # Customize the x-axis tick labels
      titlefont = list(family = "sans", size = 15, color = "black"),  # Customize the x-axis title
      tickvals = c(0, 50, 100, 150, 200, 250, 300),  # Specify the tick values
      ticktext = c("0", "50", "100", "150", "200", "250", "300")  # Specify the corresponding tick labels
    ),
    margin = list(l = 50, r = 10, t = 20, b = 150),
    bargap = 0.2,  # Adjust the gap between bars
    barmode = 'overlay'  # Overlap the bars
  )

# Display the plot
bar_plot

library(htmltools)
library(htmlwidgets)

# Save both plots to HTML elements
sunburst_widget <- saveWidget(sunburst_plot, "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/evidence_map_R_code/figures/sunburst.html", selfcontained = FALSE)
barplot_widget <- saveWidget(bar_plot, "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/evidence_map_R_code/figures/barplot.html", selfcontained = FALSE)
barplot_widget

# Read the HTML content of the saved files
sunburst_html <- paste(readLines("sunburst.html"), collapse = "\n")
barplot_html <- paste(readLines("barplot.html"), collapse = "\n")


combined_html <- htmltools::tagList(
  htmltools::tags$div(
    style = "display:flex; justify-content: space-around;",
    htmltools::tags$iframe(src = "sunburst.html", width = "700px", height = "700px"),
    htmltools::tags$iframe(src = "barplot.html", width = "700px", height = "700px")
  )
)

# Save the combined HTML to a file
save_html(combined_html, "C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/evidence_map_R_code/figures/combined_plot.html")

# Open the HTML file in your browser
browseURL("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/evidence_map_R_code/figures/combined_plot.html")




browseURL("C:/Users/andreasanchez/OneDrive - CGIAR/1_chapter_PhD/Environmental_evidence/evidence_map_R_code/figures/combined_plot.html")


















# Reorder factor_category based on the desired order
sort(unique(factor_category_total$factor_category))
desired_order <- c(
  "Practice characteristics","Financial capital" ,"Human capital" ,"Natural capital","Biophysical context",
  "Physical capital","P&I context\n(Risk management)" ,"P&I context\n(Access to knowledge)","Social capital", "P&I context\n(Value chain)" ,
  "P&I context\n(NA)" ,"Farm management" ,"Other","Farmers' behaviour","P&I context\n(Land tenure security)"  )

desired_order <- c(
"Practice characteristics","P&I context\n(Value chain)" ,"P&I context\n(NA)" ,"P&I context\n(Risk management)" ,
"P&I context\n(Access to knowledge)","Social capital", "Farm management" ,
"Other","Financial capital" ,"Human capital" ,"Natural capital","Biophysical context",
"Physical capital",
"Farmers' behaviour","P&I context\n(Land tenure security)"  )

factor_category_total$factor_category <- factor(factor_category_total$factor_category, levels = desired_order)

mycols = c(brewer.pal(3, 'Oranges'), brewer.pal(3, 'Greens'), 
           brewer.pal(2, 'Blues'), brewer.pal(2, 'PuRd'))

p0 + scale_fill_manual(values = mycols)


bar_plot<-
ggplot(factor_category_total) +
  
  geom_col( aes(x = n_models, y = factor_category), 
            fill="grey65", width = 0.7, alpha=0.7)+
  geom_col( aes(x = n_studies, y =factor_category ), 
            fill="grey20", width = 0.3)+
  labs(x = "Number of studies - Number of models")+
  scale_x_continuous(
    limit = c(0,310),expand = c(0,0),
    breaks = c(0,50,100,150,200,250,300),
    labels= c("0","50","100","150","200","250","300"))+
  scale_y_discrete(expand = c(0.05, 0)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color="black", size=13, family = "sans", face = "bold", vjust = -1,
                                    margin = margin(r = 20)),
        axis.text.x = element_text(color="black", size=12, family = "sans"),
        axis.ticks.y=element_line(colour = "white"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        strip.placement.x = "outside",
        plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm")) 
  
bar_plot






# Combine the plots using grid.arrange
library(gridExtra)

gridExtra::grid.arrange(dendrogram_plot, bar_plot, ncol = 2)







# Create a function to find the most common combinations of a given size
find_most_common_combinations <- function(data, size) {
  # Get the column names of the factor categories
  factor_cols <- colnames(data)
  
  # Generate all possible combinations of the given size
  combinations <- combn(factor_cols, size, simplify = FALSE)
  
  # Initialize a list to store the counts
  combination_counts <- list()
  
  # Count the occurrences of each combination
  for (comb in combinations) {
    # Create a string representation of the combination
    comb_str <- paste(comb, collapse = "-")
    
    # Sum the rows where all columns in the combination are 1
    count <- data %>%
      filter(across(all_of(comb), ~ . == 1)) %>%
      nrow()
    
    # Store the count in the list
    combination_counts[[comb_str]] <- count
  }
  
  # Convert the list to a dataframe and sort by count
  combination_counts_df <- tibble(
    combination = names(combination_counts),
    count = unlist(combination_counts)
  ) %>%
    arrange(desc(count))
  
  return(combination_counts_df)
}

# Find the most common combinations for sizes 2, 3, 4, and 5
most_common_2 <- find_most_common_combinations(selected_data, 2)
most_common_3 <- find_most_common_combinations(selected_data, 3)
most_common_4 <- find_most_common_combinations(selected_data, 4)
most_common_5 <- find_most_common_combinations(selected_data, 5)
most_common_6 <- find_most_common_combinations(selected_data, 6)
most_common_7 <- find_most_common_combinations(selected_data, 7)
most_common_8 <- find_most_common_combinations(selected_data, 8)
most_common_9 <- find_most_common_combinations(selected_data, 9)
most_common_10 <- find_most_common_combinations(selected_data, 10)
most_common_11 <- find_most_common_combinations(selected_data, 11)
most_common_12 <- find_most_common_combinations(selected_data, 12)
most_common_13 <- find_most_common_combinations(selected_data, 13)
most_common_14 <- find_most_common_combinations(selected_data, 14)

prueba<-rbind(most_common_3,most_common_4,most_common_5,most_common_6,
      most_common_7,most_common_8,most_common_9)%>%
  filter(count!=0)




# Combine all common combinations into a single dataframe
all_common_combinations <- bind_rows(
  most_common_2,
  most_common_3,
  most_common_4,
  most_common_5
)



###no se va a usar




# Generate all pairs of factor_category within each study_model_id
pairwise_combinations <- model_factor_category_filtered %>%
  summarise(pairs = list(combn(factor_category, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair1 = map_chr(pairs, 1),
         pair2 = map_chr(pairs, 2)) %>%
  select(-pairs)

pairwise_combinations <- model_factor_category_filtered %>%
  summarise(pairs = list(combn(factor_category, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair1 = map_chr(pairs, 1),
         pair2 = map_chr(pairs, 2)) %>%
  select(-pairs)%>%
  group_by(pair1,pair2)%>%
  summarise(n_models= n_distinct(study_model_id))

# Count the pairs to create the co-occurrence matrix
co_occurrence <- pairwise_combinations %>%
  count(pair1, pair2) %>%
  ungroup()%>%
  complete(pair1, pair2, fill = list(n = 0))%>% 
  pivot_wider(names_from = pair2, values_from = n, values_fill = list(n = 0))

# Convert to a matrix
co_occurrence_matrix <- as.matrix(co_occurrence[,-1])
co_occurrence_matrix
rownames(co_occurrence_matrix) <- co_occurrence$pair1
co_occurrence_matrix
# Make the matrix symmetric
co_occurrence_matrix <- co_occurrence_matrix + t(co_occurrence_matrix)
co_occurrence_matrix
# Count the number of study_model_id that include each factor_category
category_counts <- factor_category %>%
  distinct(study_model_id, factor_category, .keep_all = TRUE)%>%
  group_by(factor_category) %>%
  summarise(count = n_distinct(study_model_id))

# Match the counts to the correct diagonal positions
diag_names <- rownames(co_occurrence_matrix)
diag_counts <- category_counts %>%
  filter(factor_category %in% diag_names) %>%
  arrange(factor_category) %>%
  pull(count)

# Ensure diag_counts is in the same order as diag_names
diag_counts <- category_counts$count[match(diag_names, category_counts$factor_category)]

# Add the counts to the diagonal
diag(co_occurrence_matrix) <- diag_counts
co_occurrence_matrix
# Calculate total counts for sorting
total_counts <- rowSums(co_occurrence_matrix)

# Order factor categories by total counts
ordered_categories <- names(total_counts[order(total_counts, decreasing = TRUE)])
ordered_categories
co_occurrence_matrix <- co_occurrence_matrix[ordered_categories, ordered_categories]
co_occurrence_matrix
# Calculate marginal counts for the ordered matrix
marginal_counts <- rowSums(co_occurrence_matrix)
marginal_counts

#By study_id
# Filter out groups with fewer than two factor_category values
study_factor_category_filtered <- factor_category %>%
  distinct(study_id, factor_category, .keep_all = TRUE)%>%
  group_by(study_id) %>%
  filter(n() > 1)

# Generate all pairs of factor_category within each study_model_id
study_pairwise_combinations <- study_factor_category_filtered %>%
  summarise(pairs = list(combn(factor_category, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  mutate(pair1 = map_chr(pairs, 1),
         pair2 = map_chr(pairs, 2)) %>%
  select(-pairs)

# Count the pairs to create the co-occurrence matrix
study_co_occurrence <- study_pairwise_combinations %>%
  count(pair1, pair2) %>%
  complete(pair1, pair2, fill = list(n = 0)) %>%
  pivot_wider(names_from = pair2, values_from = n, values_fill = list(n = 0))

# Convert to a matrix
study_co_occurrence_matrix <- as.matrix(study_co_occurrence[,-1])
rownames(study_co_occurrence_matrix) <- study_co_occurrence$pair1

# Make the matrix symmetric
study_co_occurrence_matrix <- study_co_occurrence_matrix + t(study_co_occurrence_matrix)
study_co_occurrence_matrix
# Count the number of study_id that include each factor_category
study_category_counts <- factor_category %>%
  distinct(study_id, factor_category, .keep_all = TRUE)%>%
  group_by(factor_category) %>%
  summarise(count = n_distinct(study_id))

# Match the counts to the correct diagonal positions
study_diag_names <- rownames(study_co_occurrence_matrix)
study_diag_names

study_diag_counts <- study_category_counts %>%
  filter(factor_category %in% study_diag_names) %>%
  arrange(factor_category) %>%
  pull(count)

study_diag_counts
# Ensure diag_counts is in the same order as diag_names
study_diag_counts <- study_category_counts$count[match(study_diag_names, study_category_counts$factor_category)]
study_diag_counts

# Add the counts to the diagonal
diag(study_co_occurrence_matrix) <- study_diag_counts

# Calculate total counts for sorting
study_total_counts <- rowSums(study_co_occurrence_matrix)
study_total_counts

# Order factor categories by total counts
study_ordered_categories <- names(study_total_counts[order(study_total_counts, decreasing = TRUE)])
study_co_occurrence_matrix <- study_co_occurrence_matrix[study_ordered_categories, study_ordered_categories]

# Calculate marginal counts for the ordered matrix
study_marginal_counts <- rowSums(study_co_occurrence_matrix)
study_co_occurrence_matrix

study_diag_counts
co_occurrence_matrix





Heatmap(co_occurrence_matrix,
        name = "# of models",
        col = colorRamp2(c(0,50,100,150,200,250, max(co_occurrence_matrix)),
                         c("white","#fff6b0","#fedf8a","#feb96a","#fa9755","#e3534b", "#d53e4f")))
# lustering_distance_columns = "binary", # Use binary distance
clustering_method_columns = "average") # Use complete linkage)


#no esta bien
heatmap <- Heatmap(co_occurrence_matrix,
                   name = "# of models",
                   col = colorRamp2(c(0,50,100,150,200,250, max(co_occurrence_matrix)), c("white","#fff6b0","#fedf8a","#feb96a","#fa9755","#e3534b", "#d53e4f")),
                   cluster_rows = FALSE,
                   cluster_columns = FALSE,
                   show_row_names = TRUE,
                   show_column_names = TRUE,
                   row_names_side = "left",
                   column_names_side = "top",
                   cell_fun = function(j, i, x, y, width, height, fill) {
                     # Number of study_model_id
                     model_count <- co_occurrence_matrix[i, j]
                     # Number of study_id
                     study_count <- study_co_occurrence_matrix[i, j]
                     # Format the label as "number of models | number of studies"
                     label <- sprintf("%d|%d", model_count, study_count)
                     grid.text(label, x, y, just = "center", gp = gpar(fontsize = 8))
                   },
                   row_names_gp = gpar(fontsize = 10),  # Adjust row names font size
                   column_names_gp = gpar(fontsize = 10), # Adjust column names font size)
                   column_names_rot = 0,
                   
                   top_annotation = HeatmapAnnotation(
                     n_study = anno_barplot(study_diag_counts, 
                                            border = FALSE, 
                                            gp = gpar(fill = "blue"),
                                            axis_param = list(at = c(0, 50, 100, 175), labels = c(0, 50, 100, 175))),
                     annotation_height = unit(2, "cm")),
                   
                   
                   bottom_annotation =  HeatmapAnnotation(
                     n_model = anno_barplot(diag_counts,
                                            gp = gpar(col = "black", lwd = 2), 
                                            axis_param = list(at = c(0, 100, 200, 300), labels = c(0, 100, 200, 300),labels_rot=0),
                                            size = unit(2, "mm")),
                     annotation_width = unit(2, "cm"))
)

draw(heatmap, heatmap_legend_side = "right",
     annotation_legend_side = "left")



skey_stages_systems<- data_clean%>%
  select(y_metric_recla_2 ,m_dp_recla)%>%
  make_long(y_metric_recla_2 ,m_dp_recla)%>%
  mutate(node=str_to_sentence(node))

sort(unique(skey_stages_systems$node))

category_labels<-c("Disadoption", "Intensity of adoption", "Adoption" ,"Interest","Awareness" , 
                   "Rotational grazing","Intercropping", "Fallow" ,"Embedded seminatural habitats",  
                   "Crop rotation","Cover crops", "Combined systems","Agroforestry", "Agro-silvopasture","Agro-aquaculture" )   

skey_stages_systems$node <- factor(skey_stages_systems$node, levels = category_labels)

fills<- c( "#AC5B9D","#E24A33","#F79A63",  "#F2D29A", "#88CCA1",
           "#545454","#545454","#545454","#545454","#545454","#545454","#545454","#545454","#545454","#545454")
ggplot(skey_stages_systems, 
       aes(x = x, next_x = next_x, node = node,
           next_node = next_node,
           fill = node,
           colour=node),
       label = node) +
  geom_sankey(flow.alpha = 0.4,
              #node.color = "black",
              show.legend = FALSE)+
  scale_fill_manual(values= fills)+
  scale_colour_manual(values= fills)+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_discrete(expand = c(0, 0))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "transparent"))

ggsave("sankey_plot.png", width = 10, height = 6, units = "in", dpi = 300)

#10X6 portrait


#########OLD
# Create a binary matrix for clustering based on the selected data
binary_matrix <- as.matrix(selected_data)
binary_matrix

# Compute the distance matrix using the count of co-occurrences
comb_dist <- dist(t(binary_matrix), method = "binary")

# Perform hierarchical clustering
hclust_res <- hclust(comb_dist, method = "ward.D2")

plot(as.dendrogram(hclust_res), horiz = TRUE)
#rect.hclust(hclust_res, k = 6, border = "red")
par(mfrow = c(1, 1))  # Reset margins after the plot if needed
par(mar = c(2.5, 0.5, 0.5,11))

# Cut the dendrogram into clusters and count the number of models per cluster
cutree_res <- cutree(hclust_res, k = 14)  # You can change the number of clusters (k) as needed

# Prepare data for dendrogram plotting with custom labels
dendro_data <- as.dendrogram(hclust_res)
dendro_labels <- labels(dendro_data)
label_indices <- match(dendro_labels, colnames(selected_data))

# Create a label dataframe with factor combinations and their counts
label_df <- data.frame(
  label = dendro_labels,
  combination = sapply(label_indices, function(i) {
    comb <- colnames(selected_data)[i]
    comb_str <- paste(comb, collapse = "-")
    count <- sum(selected_data[, i] == 1)
    paste(comb_str, "(", count, ")", sep = "")
  })
)

# Add cluster information
label_df$cluster <- cutree_res[label_indices]

# Count the number of models per cluster
cluster_counts <- label_df %>%
  group_by(cluster) %>%
  summarize(count = n())

# Merge cluster counts back to label_df
label_df <- label_df %>%
  left_join(cluster_counts, by = "cluster")

# Prepare dendrogram data for ggplot
dendro_data <- dendro_data(hclust_res)
segment_data <- segment(dendro_data)
label_data <- label_df %>%
  mutate(x = 1:n())
segment(dendro_data)
# Plot the dendrogram with custom labels and cluster counts
dendrogram_plot
ggplot() +
  geom_segment(data = segment(dendro_data), 
               #aes(x = y, y = x, xend = yend, yend = xend)) 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label_df, aes(x =0.5:length(label), y = -0.01, label = label),
            angle = 0, vjust = -0.5, size = 3) 
#coord_flip() +
labs(y = "Height")+
  scale_y_continuous(expand = c(0.1, 0),
                     breaks= c(0,0.25,0.5,0.75,1),
                     labels = c("0","0.25","0.50","0.75","1") ) +
  scale_x_continuous(expand = c(0.05, 0.01),breaks= c(0,0.25,0.5,0.75,1),
                     labels = c("0","0.25","0.50","0.75","1") ) +
  
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(color="black", size=12, family = "sans"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_text(color="black", size=13, family = "sans", face = "bold", vjust = -1,
                                margin = margin(r = 20)),
    axis.title.y= element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "black", linetype=1),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))


dendrogram_plot


