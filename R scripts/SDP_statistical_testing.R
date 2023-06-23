library("readxl")
library("effsize")

ind_stat_test <-function(col,data){
  algorithms <- c('nsga3','nsga3_NO_COMPLEXITY' )
  target ="nsga3"
  x = unlist(subset(data,algorithm == "nsga3")[col],use.names = FALSE)
  files <- c( 
    'activemq-5.0.0_exp_0_train', 'activemq-5.1.0_exp_68_train',
     'activemq-5.2.0_exp_37_train', 'activemq-5.3.0_exp_32_train',
     'groovy-1_5_7_exp_51_train', 'groovy-1_6_BETA_1_exp_53_train',
     'hbase-0.94.0_exp_25_train', 'hbase-0.95.0_exp_3_train',
     'hive-0.10.0_exp_19_train', 'hive-0.9.0_exp_47_train',
     'jruby-1.1_exp_30_train', 'jruby-1.4.0_exp_41_train',
     'jruby-1.5.0_exp_61_train', 'lucene-2.3.0_exp_57_train',
     'lucene-2.9.0_exp_11_train', 'lucene-3.0.0_exp_43_train',
     'wicket-1.3.0-beta2_exp_51_train',
     'wicket-1.3.0-incubating-beta-1_exp_0_train')
  
  results <- data.frame(algorithm = character(),file_id = character(),effect_size = double(),p_value=double(),mean = double(),median = double())
  for (project in files){
    cat("processing project",project,"\n")
    for (algo in algorithms)
    {
      cat(algo,"\n")
      x = unlist(subset(data,algorithm == target & file_id == project)[col],use.names = FALSE)
      y = unlist(subset(data,(algorithm == algo) & file_id == project)[col],use.names = FALSE)
      print(y)
      p_val = wilcox.test(x,y)
      delta = cliff.delta(x,y)
      new_entry <- list(algorithm=algo,file_id = project, effect_size = delta$estimate, p_value = p_val$p.value,mean= mean(y),median = median(y))
      results <- rbind(results,new_entry)
      print(p_val)
      print(delta)
      
    }
  }
  for (algo in algorithms)
  {
    cat(algo,"\n")
    x = unlist(subset(data,algorithm == target )[col],use.names = FALSE)
    y = unlist(subset(data,(algorithm == algo) )[col],use.names = FALSE)
    p_val = wilcox.test(x,y)
    delta = cliff.delta(x,y)
    new_entry <- list(algorithm=algo,file_id = 'all', effect_size = delta$estimate, p_value = p_val$p.value,mean= mean(y),median = median(y))
    results <- rbind(results,new_entry)
    print(p_val)
    print(delta)
    
  }
  
  return (results)
}
my_data <- read.csv("C:/Users/Motaz/Desktop/work/DP_performance_complexity/code/GP_COMPLEXITY_VS_GP_NOCOMPLEXITY_COMPLEXITY.csv")
results <- ind_stat_test("total_node_number",my_data)
write.csv(results,'C:/Users/Motaz/Desktop/work/DP_performance_complexity/code/results/RQ_GP_VS_ML_results/GP_COMPLEXITY_VS_GP_NOCOMPLEXITY_COMPLEXITY_total_node_number.csv')