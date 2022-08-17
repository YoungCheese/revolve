library(ggplot2)
library(sqldf)
library(plyr)
library(dplyr)
library(trend)
library(purrr)
library(ggsignif)
library(stringr)
library(reshape)
library(viridis)

base_directory <-paste('data', sep='') 

analysis = 'analysis'

output_directory = paste(base_directory,'/',analysis ,sep='')

#### CHANGE THE PARAMETERS HERE ####

experiments_type = c( 'plasticoding_static', 'lenienterunweighted' )
experiments_labels2 = c(  'Baseline: Tilted' ,  'Plastic: Tilted')

environments = list( c( 'tilted5'), c( 'tilted5') )

methods = c()
for (exp in 1:length(experiments_type))
{
  for (env in 1:length(environments[[exp]]))
  {
    methods = c(methods, paste(experiments_type[exp], environments[[exp]][env], sep='_'))
  }
}

initials =   c(  'b', 'p' )

experiments_labels = c( 'costplasticoding_static' ,  'Plastic')
experiments_labels2 = c(  'Baseline: Tilted' ,  'Plastic: Tilted')

runs = list( c(1:20),c(1:20)  )

gens = 200 
pop = 100

#### CHANGE THE PARAMETERS HERE ####

sig = 0.05
line_size = 30
show_markers = FALSE
show_legends = FALSE 
experiments_type_colors = c(  '#ff9933', '#cc0000' )  #  orange ; dark red

measures_names = c(
  'displacement_velocity_hill',
  'head_balance',
  'branching',
  #'branching_modules_count',
  'limbs',
  'extremities',
  'length_of_limbs',
  #'extensiveness',
  'coverage',
  'joints',
  #'hinge_count',
  #'active_hinges_count',
  #'brick_count',
  #'touch_sensor_count',
  #'brick_sensor_count',
  'proportion',
  #'width',
  #'height',
  'absolute_size',
  'sensors',
  'symmetry',
  'avg_period',
  'dev_period',
  'avg_phase_offset',
  'dev_phase_offset',
  'avg_amplitude',
  'dev_amplitude',
  'avg_intra_dev_params',
  'avg_inter_dev_params',
  'sensors_reach',
  'recurrence',
  'synaptic_reception',
  'unweighted_cost',
  'weighted_cost'
)

# add proper labels soon...
measures_labels = c(
  'Speed (cm/s)',
  'Balance',
  'Branching',
  #'branching_modules_count',
  'Rel number of limbs',
  'Number of Limbs',
  'Rel. Length of Limbs',
  #'Extensiveness',
  'Coverage',
  'Rel. Number of Joints',
  #'hinge_count',
  #'active_hinges_count',
  #'brick_count',
  #'touch_sensor_count',
  #'brick_sensor_count',
  'Proportion',
  #'width',
  #'height',
  'Size',
  'Sensors',
  'Symmetry',
  'Average Period',
  'Dev Period',
  'Avg phase offset',
  'Dev phase offset',
  'Avg Amplitude',
  'Dev amplitude',
  'Avg intra dev params',
  'Avg inter dev params',
  'Sensors Reach',
  'Recurrence',
  'Synaptic reception',
  'unweighted_blocks',
  'weighted_blocks'
)


more_measures_names = c(
  # 'novelty',
  'novelty_pop',
  'fitness'#,
  #'cons_fitness'
)

more_measures_labels = c(
  #'Novelty (+archive)',
  'Novelty',
  'Fitness'#,
  #'Number of slaves'
)

#### CHANGE THE PARAMETERS HERE ####



methods = c()
for (exp in 1:length(experiments_type))
{
  for (env in 1:length(environments[[exp]]))
  {
    methods = c(methods, paste(experiments_type[exp], environments[[exp]][env], sep='_'))
  }
}

measures_snapshots_all = NULL

for (exp in 1:length(experiments_type))
{
  for(run in runs[[exp]])
  {
    for (env in 1:length(environments[[exp]]))
    {

      measures   = read.table(paste(base_directory,paste(experiments_type[exp], environments[[exp]][env], run,"all_measures.tsv", sep='_'), sep='/'),
                              header = TRUE, fill=TRUE)
      for( m in 1:length(measures_names))
      { 
        measures[measures_names[m]] = as.numeric(as.character(measures[[measures_names[m]]]))
      }
      
      snapshots   = read.table(paste(base_directory,paste(experiments_type[exp], environments[[exp]][env], run, "snapshots_ids.tsv", sep='_'), sep='/'),
                               header = TRUE)
      
      measures_snapshots = sqldf('select * from snapshots inner join measures using(robot_id) order by generation')
      
      measures_snapshots$run = run
      measures_snapshots$displacement_velocity_hill =   measures_snapshots$displacement_velocity_hill*100
      measures_snapshots$run = as.factor(measures_snapshots$run)
      measures_snapshots$method = paste(experiments_type[exp], environments[[exp]][env],sep='_')
      measures_snapshots$method_label =  experiments_labels2[exp] 
      
      if ( is.null(measures_snapshots_all)){
        measures_snapshots_all = measures_snapshots
      }else{
        measures_snapshots_all = rbind(measures_snapshots_all, measures_snapshots)
      }
    }
  }
}


fail_test = sqldf(paste("select method,run,generation,count(*) as c from measures_snapshots_all group by 1,2,3 having c<",gens," order by 4"))


measures_snapshots_all = sqldf("select * from measures_snapshots_all where cons_fitness IS NOT NULL") 




# densities





measures_averages_gens_1 = list()
measures_averages_gens_2 = list()

measures_ini = list()
measures_fin = list()

for (met in 1:length(methods))
{
  
  measures_aux = c()
  query ='select run, generation'
  for (i in 1:length(measures_names))
  {
    query = paste(query,', avg(',measures_names[i],') as ', methods[met], '_',measures_names[i],'_avg', sep='') 
  }
  query = paste(query,' from measures_snapshots_all 
                    where method="', methods[met],'" group by run, generation', sep='')
  
  temp = sqldf(query)
  
  measures_averages_gens_1[[met]] = temp
  
  temp = measures_averages_gens_1[[met]] 
  
  temp$generation = as.numeric(temp$generation)
  
  measures_ini[[met]] = sqldf(paste("select * from temp where generation=0"))
  measures_fin[[met]] = sqldf(paste("select * from temp where generation=",gens-1))
  query = 'select generation'
  for (i in 1:length(measures_names))
  {
    # later renames vars _avg_SUMMARY, just to make it in the same style as the quantile variables
    query = paste(query,', avg(', methods[met], '_',measures_names[i],'_avg) as '
                  ,methods[met],'_',measures_names[i],'_avg', sep='') 
    query = paste(query,', max(', methods[met],'_',measures_names[i],'_avg) as '
                  ,methods[met],'_',measures_names[i],'_max', sep='') 
    query = paste(query,', stdev(', methods[met],'_',measures_names[i],'_avg) as '
                  , methods[met],'_',measures_names[i],'_stdev', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_avg) as '
                  , methods[met],'_',measures_names[i],'_median', sep='')
    
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_avg', sep='') )
  }
  query = paste(query,' from temp group by generation', sep="")
  
  temp2 = sqldf(query)
  
  p <- c(0.25, 0.75)
  p_names <- map_chr(p, ~paste0('Q',.x*100, sep=""))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  quantiles = data.frame(temp %>% 
                           group_by(generation) %>% 
                           summarize_at(vars(measures_aux), funs(!!!p_funs)) )
  
  measures_averages_gens_2[[met]] = sqldf('select * from temp2 inner join quantiles using (generation)')
  
}


for (met in 1:length(methods))
{
  if(met==1){
    measures_averages_gens = measures_averages_gens_2[[1]]
  }else{
    measures_averages_gens = merge(measures_averages_gens, measures_averages_gens_2[[met]], all=TRUE, by = "generation")
  }
}

file <-file(paste(output_directory,'/trends.txt',sep=''), open="w")

#tests trends in curves and difference between ini and fin generations


# ini VS fin
array_wilcoxon = list()
array_wilcoxon2 = list()

# curve
array_mann = list()


for (m in 1:length(methods))
{
  
  array_wilcoxon[[m]] = list()
  array_mann[[m]] = list()
  
  for (i in 1:length(measures_names)) 
  {
    
    writeLines(paste(experiments_type[m],measures_names[i],'ini avg ',as.character(
      mean(c(array(measures_ini[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]) )[[1]]) ) ,sep=" "), file )
    
    
    writeLines(paste(methods[m],measures_names[i],'fin avg ',as.character(
      mean(c(array(measures_fin[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]) )[[1]]) ) ,sep=" "), file )
    
    array_wilcoxon[[m]][[i]]  = wilcox.test(c(array(measures_ini[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]))[[1]] ,
                                            c(array(measures_fin[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]))[[1]]
    )
    
    writeLines(c(
      paste(methods[m],'iniVSfin',measures_names[i],'wilcox p: ',as.character(round(array_wilcoxon[[m]][[i]]$p.value,4)), sep=' ')
      ,paste(methods[m],'iniVSfin',measures_names[i],'wilcox est: ',as.character(round(array_wilcoxon[[m]][[i]]$statistic,4)), sep=' ')
      
    ), file)
    
    
    #tests  trends
    array_mann[[m]][[i]] =  mk.test(c(array(measures_averages_gens_2[[m]][paste(methods[m],"_",measures_names[i],'_median',sep='')]) )[[1]],
                                    continuity = TRUE)
    
    
    writeLines(c(
      paste(experiments_type[m],measures_names[i], ' Mann-Kendall median p', as.character(round(array_mann[[m]][[i]]$p.value,4)),sep=' '),
      paste(experiments_type[m],measures_names[i], ' Mann-Kendall median s', as.character(round(array_mann[[m]][[i]]$statistic,4)),sep=' ')
    ), file)
    
  }
  
}



# tests final generation among experiments_type

aux_m = length(methods)

if (aux_m>1)
{
  
  # fins
  array_wilcoxon2[[1]] = list()
  array_wilcoxon2[[2]] = list()
  
  aux_m = aux_m -1
  count_pairs = 0
  for(m in 1:aux_m)
  {
    aux = m+1
    for(m2 in aux:length(methods))
    {  
      
      count_pairs = count_pairs+1
      array_wilcoxon2[[1]][[count_pairs]] = list()
      
      for (i in 1:length(measures_names)) 
      {
        
        writeLines(paste(methods[m],measures_names[i],'fin avg ',as.character(
          mean(c(array(measures_fin[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]) )[[1]]) ) ,sep=" "), file )
        
        writeLines(paste(methods[m2],measures_names[i],'fin avg ',as.character(
          mean(c(array(measures_fin[[m2]][paste(methods[m2],"_",measures_names[i],"_avg",sep='')]) )[[1]]) ) ,sep=" "), file )
        
        array_wilcoxon2[[1]][[count_pairs]][[i]]  = wilcox.test(c(array(measures_fin[[m]][paste(methods[m],"_",measures_names[i],"_avg",sep='')]))[[1]] ,
                                                                c(array(measures_fin[[m2]][paste(methods[m2],"_",measures_names[i],"_avg",sep='')]))[[1]]
        )
        
        writeLines(c(
          paste(methods[m],'VS',methods[m],measures_names[i],'fin avg wilcox p: ',as.character(round(array_wilcoxon2[[1]][[count_pairs]][[i]]$p.value,4)), sep=' ')
          ,paste(methods[m],'VS',methods[m2],measures_names[i],'fin avg wilcox est: ',as.character(round(array_wilcoxon2[[1]][[count_pairs]][[i]]$statistic,4)), sep=' ')
          
        ), file)
        
      }
      
      
      array_wilcoxon2[[2]][[count_pairs]] = paste(initials[m],initials[m2],sep='')
      
    }
  }
  
}

close(file)

# plots measures 

for (type_summary in c('means','quants'))
{
  for (i in 1:length(measures_names)) 
  {
    tests1 = ''
    tests2 = ''
    tests3 = ''
    break_aux = 0
    
    graph <- ggplot(data=measures_averages_gens, aes(x=generation))
    
    for(m in 1:length(methods))
    {
      if(type_summary == 'means')
      {
        graph = graph + geom_ribbon(aes_string(ymin=paste(methods[m],'_',measures_names[i],'_avg','-',methods[m],'_',measures_names[i],'_stdev',sep=''), 
                                               ymax=paste(methods[m],'_',measures_names[i],'_avg','+',methods[m],'_',measures_names[i],'_stdev',sep='') ), 
                                    fill=experiments_type_colors[m] ,  color=experiments_type_colors[m],alpha=0.2)
      }else
      {
        graph = graph + geom_ribbon(aes_string(ymin=paste(methods[m],'_',measures_names[i],'_avg_Q25',sep=''), 
                                               ymax=paste(methods[m],'_',measures_names[i],'_avg_Q75',sep='') ), 
                                    fill=experiments_type_colors[m] ,  color=experiments_type_colors[m],alpha=0.2) 
      }
    }
    
    for(m in 1:length(methods))
    {
      if(type_summary == 'means')
      {
        if(show_legends == TRUE){
          graph = graph + geom_line(aes_string(y=paste(methods[m],'_',measures_names[i],'_avg',sep=''), colour=shQuote(experiments_labels[m]) ), size=2)
        }else{
          graph = graph + geom_line(aes_string(y=paste(methods[m],'_',measures_names[i],'_avg',sep='')   ),size=2, color = experiments_type_colors[m])
        }
        
      }else{
        if(show_legends == TRUE){
          graph = graph + geom_line(aes_string(y=paste(methods[m],'_',measures_names[i],'_median',sep='') , colour=shQuote(experiments_labels[m])   ),size=2 )
        }else{
          graph = graph + geom_line(aes_string(y=paste(methods[m],'_',measures_names[i],'_median',sep='')  ),size=2, color = experiments_type_colors[m] )
        }
      }
      
      if (length(array_mann)>0)
      {
        if (length(array_mann[[m]])>0)
        {
          if(!is.na(array_mann[[m]][[i]]$p.value))
          {
            if(array_mann[[m]][[i]]$p.value<=sig)
            {
              if(array_mann[[m]][[i]]$statistic>0){ direction = "/  "} else { direction = "\\  "}
              tests1 = paste(tests1, initials[m],direction,sep="") 
            }
          }
        }
      }
    }
    
    if (length(array_wilcoxon[[m]])>0)
    {
      for(m in 1:length(methods))
      {
        if(!is.na(array_wilcoxon[[m]][[i]]$p.value))
        {
          if(array_wilcoxon[[m]][[i]]$p.value<=sig)
          {
            tests2 = paste(tests2, initials[m],'C  ', sep='') 
          }
        }
      }
    }
    
    if (length(array_wilcoxon2)>0)
    {
      for(p in 1:length(array_wilcoxon2[[1]]))
      {
        if (length(array_wilcoxon2[[1]][[p]])>0)
        {
          if(!is.na(array_wilcoxon2[[1]][[p]][[i]]$p.value))
          {
            if(array_wilcoxon2[[1]][[p]][[i]]$p.value<=sig)
            {
              if(nchar(tests3)>line_size && break_aux == 0){
                tests3 = paste(tests3, '\n')
                break_aux = 1
              }
              tests3 = paste(tests3, array_wilcoxon2[[2]][[p]],'D  ',sep='')
            }
          }
        }
      }
    }
    
    
    max_y =  0
    min_y = 0
    if (measures_names[i] == 'displacement_velocity_hill' )  { 
      max_y = 2.5 
      min_y = -0.5}
    if (measures_names[i] == 'head_balance' ) {    
      max_y = 1
      min_y = 0.6}
    if (measures_names[i] == 'absolute_size' )  {    max_y = 16}
    if (measures_names[i] == 'recurrence' )  {    max_y = 0.3}
    if (measures_names[i] == 'sensors' )  {    max_y = 0.3 }
    if (measures_names[i] == 'sensors_reach' )  {    max_y = 1 }
    
    
    graph = graph  +  labs( y=measures_labels[i], x="Generation", title="Tilted Season") 
    if (max_y>0) {
      graph = graph + coord_cartesian(ylim = c(min_y, max_y)) 
    }
    
    if(show_markers == TRUE){
      graph = graph  + labs( y=measures_labels[i], x="Generation", subtitle = paste(tests1,'\n', tests2, '\n', tests3, sep='')) 
    }
    graph = graph  + theme(legend.position="bottom" ,  legend.text=element_text(size=20), axis.text=element_text(size=27),axis.title=element_text(size=25),
                           plot.subtitle=element_text(size=25 ),plot.title=element_text(size=25 )) 
    
    ggsave(paste( output_directory,'/',type_summary,'_' ,measures_names[i],'_generations.pdf',  sep=''), graph , device='pdf', height = 10, width = 10)
    
  }
  
}


for (met in 1:length(methods))
{
  measures_aux = c()
  p <- c(0.25, 0.75)
  p_names <- map_chr(p, ~paste0('Q',.x*100, sep=""))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)
  
  query ='select run, generation'
  for (i in 1:length(measures_names))
  {
    query = paste(query,', avg(',measures_names[i],') as ', methods[met], '_',measures_names[i],'_mean', sep='')
    query = paste(query,', median(',measures_names[i],') as ', methods[met], '_',measures_names[i],'_median', sep='')
    query = paste(query,', min(',measures_names[i],') as ', methods[met], '_',measures_names[i],'_min', sep='')
    query = paste(query,', max(',measures_names[i],') as ', methods[met], '_',measures_names[i],'_max', sep='')
    measures_aux = c(measures_aux, measures_names[i])
  }
  query = paste(query,' from measures_snapshots_all
                  where method="', methods[met],'" group by run, generation', sep='')
  inner_measures = sqldf(query)
  
  quantiles = data.frame(measures_snapshots_all %>%
                           filter(method==methods[met]) %>%
                           group_by(run, generation) %>%
                           summarize_at(vars(  measures_aux), funs(!!!p_funs)) )
  for (i in 1:length(measures_names)){
    for(q in c('Q25', 'Q75')){
      variable =  paste(measures_names[i], q, sep='_')
      names(quantiles)[names(quantiles) == variable] <- paste(methods[met], '_',variable, sep='')
    }
  }
  inner_measures = sqldf('select * from inner_measures inner join quantiles using (run, generation)')
  
  measures_averages_gens_1[[met]] = inner_measures
  
  inner_measures = measures_averages_gens_1[[met]]
  
  inner_measures$generation = as.numeric(inner_measures$generation)
  
  measures_aux = c()
  query = 'select generation'
  for (i in 1:length(measures_names))
  {
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_mean) as ' , methods[met],'_',measures_names[i],'_mean_median', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_median) as ', methods[met],'_',measures_names[i],'_median_median', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_min) as ', methods[met],'_',measures_names[i],'_min_median', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_max) as ', methods[met],'_',measures_names[i],'_max_median', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_Q25) as ', methods[met],'_',measures_names[i],'_Q25_median', sep='')
    query = paste(query,', median(', methods[met],'_',measures_names[i],'_Q75) as ', methods[met],'_',measures_names[i],'_Q75_median', sep='')
    
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_mean', sep='') )
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_median', sep='') )
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_min', sep='') )
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_max', sep='') )
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_Q25', sep='') )
    measures_aux = c(measures_aux, paste(methods[met],'_',measures_names[i],'_Q75', sep='') )
  }
  query = paste(query,' from inner_measures group by generation', sep="")
  outter_measures = sqldf(query)
  
  quantiles = data.frame(inner_measures %>%
                           group_by(generation) %>%
                           summarize_at(vars(  measures_aux), funs(!!!p_funs)) )
  
  measures_averages_gens_2[[met]] = sqldf('select * from outter_measures inner join quantiles using (generation)')
  
}


for (met in 1:length(methods))
{
  if(met==1){
    measures_averages_gens = measures_averages_gens_2[[1]]
  }else{
    measures_averages_gens = merge(measures_averages_gens, measures_averages_gens_2[[met]], all=TRUE, by = "generation")
  }
}




all_na = colSums(is.na(measures_averages_gens)) == nrow(measures_averages_gens)
for (i in 1:length(measures_names))
{
  
  #  line plots
  
  
  # finding values for scaling
  max_y =  0
  min_y = 10000000
  for(a in 1:length(aggregations)){
    for(m in 1:length(methods)){
      max_value = max(measures_averages_gens[paste(methods[m],'_',measures_names[i],'_', aggregations[a], '_Q75',sep='')], na.rm = TRUE)
      min_value = min(measures_averages_gens[paste(methods[m],'_',measures_names[i],'_', aggregations[a], '_Q25',sep='')], na.rm = TRUE)
      if(max_value > max_y){ max_y = max_value }
      if(min_value < min_y){ min_y = min_value }
    }
  }
  #if (measures_names[i] == 'absolute_size' )  {    max_y = 16}
  
  for(a in 1:length(aggregations)){
    
    graph <- ggplot(data=measures_averages_gens, aes(x=generation))
    
    for(m in 1:length(methods)){
      
      is_all_na = all_na[paste(methods[m],'_',measures_names[i],'_', aggregations[a], '_median', sep='')]
      
      if (is_all_na == FALSE) {
        
        graph = graph + geom_ribbon(aes_string(ymin=paste(methods[m],'_',measures_names[i],'_', aggregations[a],'_Q25',sep=''),
                                               ymax=paste(methods[m],'_',measures_names[i],'_', aggregations[a],'_Q75',sep='') ),
                                    fill=experiments_type_colors[m], alpha=0.2, size=0)
        
        graph = graph + geom_line(aes_q(y = as.name(paste(methods[m],'_',measures_names[i],'_', aggregations[a], '_median', sep='')) ,
                                        colour=paste(methods_labels[m], aggregations[a], sep='_')), size=1)
      }
    }
    
    if (max_y>0) {
      graph = graph + coord_cartesian(ylim = c(min_y, max_y))
    }
    graph = graph  +  labs(y=measures_labels[i], x="Generation", title="")
    
    graph = graph +   scale_color_manual(values=experiments_type_colors)
    graph = graph  + theme(legend.position="bottom" ,  legend.text=element_text(size=25), axis.text=element_text(size=32), axis.title=element_text(size=30),
                           plot.subtitle=element_text(size=30 ), plot.title=element_text(size=30 ))
    
    ggsave(paste( output_directory,'/',measures_names[i], '_', aggregations[a], '_lines.pdf',  sep=''), graph , device='pdf', height = 10, width = 10)
    
    
    
    # creates one box plot per measure, and one extra in case outlier removal is needeed
    outliers = c('full', 'filtered')
    for (out in outliers)
    {
      has_outliers = FALSE
      
      for(gc in gens_box_comparisons)
      {
        
        all_final_values = data.frame()
        for (met in 1:length(methods))
        {
          
          met_measures = measures_averages_gens_1[[met]]
          gen_measures = sqldf(paste("select * from met_measures where generation=", gc, sep=''))
          
          temp = data.frame( c(gen_measures[paste(methods[met],'_',measures_names[i],'_', aggregations[a], sep='')]))
          colnames(temp) <- 'values'
          
          if (out == 'filtered'){
            if (!all(is.na(temp$values))){
              
              num_rows_before = nrow(temp)
              upperl <- quantile(temp$values)[4] + 1.5*IQR(temp$values)
              lowerl <- quantile(temp$values)[2] - 1.5*IQR(temp$values)
              temp = temp %>% filter(values <= upperl & values >= lowerl )
              
              if (num_rows_before > nrow(temp)){
                has_outliers = TRUE
              }
            }
          }
          
          temp$type = methods_labels[met]
          all_final_values = rbind(all_final_values, temp)
        }
        
        g1 <-  ggplot(data=all_final_values, aes(x= type , y=values, color=type )) +
          geom_boxplot(position = position_dodge(width=0.9),lwd=2,  outlier.size = 4) +
          labs( x="Method", y=measures_labels[i], title=str_to_title(aggregations[a]))
        
        g1 = g1 +  scale_color_manual(values=  experiments_type_colors  )
        
        g1 = g1 + theme(legend.position="none" , text = element_text(size=50) ,
                        plot.title=element_text(size=50),  axis.text=element_text(size=50),
                        axis.title=element_text(size=55),
                        axis.text.x = element_text(angle = 20, hjust = 0.9),
                        plot.margin=margin(t = 0.5, r = 0.5, b = 0.5, l =  1.3, unit = "cm"))+
          stat_summary(fun.y = mean, geom="point" ,shape = 16,  size=11)
        
        # in this list, use the desired pairs names from methods_labels
        comps = list( methods_labels )
        
        max_y =  0
        #if (measures_names[i] == 'absolute_size' )  {    max_y = 16}
        if (max_y>0) {
          graph = graph + coord_cartesian(ylim = c(min_y, max_y))
        }
        
        g1 = g1 + geom_signif( test="wilcox.test", size=1, textsize=18,
                               comparisons = comps,
                               map_signif_level=c() )
        
        if (out == 'full' || (out == 'filtered' &&  has_outliers == TRUE) ){
          ggsave(paste(output_directory,"/",measures_names[i],"_",gc,"_", aggregations[a],'_', out,"_boxes.pdf",sep = ""), g1, device = "pdf", height=18, width = 10)
        }
        
      }
      
    }
    
  }
  
}