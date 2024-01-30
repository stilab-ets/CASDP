package MOEA_GP;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.moeaframework.core.Solution;
import org.moeaframework.util.tree.Node;

import core.ConfusionMatrix;
import core.DataFrame;
import core.GraphViz;

public class EvaluateMultiTree {
	
	public DataFrame val_data;
	public DataFrame test_data;
	public MultiTreeRun multi_tree_run;
	protected Object[] true_test_labels ; 
	protected Object[] true_val_labels ; 
	protected double[] perfect_point ; 

	public EvaluateMultiTree(DataFrame val_data, DataFrame test_data,MultiTreeRun multi_tree_run,double [] perfect_point) {
		
		this.val_data = val_data ; 
		this.test_data = test_data ; 
		this.perfect_point = perfect_point ; 
		this.multi_tree_run = multi_tree_run ; 
		this.true_test_labels = test_data.get_column_data(this.multi_tree_run.topredict) ;
		this.true_val_labels = val_data.get_column_data(this.multi_tree_run.topredict) ;
				
		
	}
	
	public void save(String path)
	{
		int i = 0 ; 
		Boolean success = (new File(path).mkdirs()) ; 
		String model_path ; 
		
		OutputStream train_confusion_matrix ;
		OutputStream val_confusion_matrix ;
		OutputStream test_confusion_matrix ;
		OutputStream rules_propreties ; 
		
		ConfusionMatrix best_model_train_confusion_matrix = null ;
		ConfusionMatrix best_val_confusion_matrix = null;
		ConfusionMatrix best_test_confusion_matrix = null;
		Solution best_model = null  ; 
		Double min_dist = Double.MAX_VALUE ; 
	    success = (new File(path+"/best_model_performance").mkdir()) ; 
		ConfusionMatrix train_matrix ; 
		ConfusionMatrix val_matrix ; 
		ConfusionMatrix test_matrix ; 
		String [] objectives = this.multi_tree_run.objectives ; 
		int nb_series = objectives.length*(objectives.length-1) /2; 
		XYSeries [] train_series = new XYSeries[nb_series];
		XYSeries [] val_series = new XYSeries[nb_series];
		XYSeries [] test_series = new XYSeries[nb_series];
		//XYSeriesCollection data = new XYSeriesCollection() ; 
		for (int k = 0 ; k < nb_series ; k++) {
			train_series[k] = new XYSeries("train") ;
			val_series[k] = new XYSeries("val") ;
			test_series[k] = new XYSeries("test") ;
		}
		
		try
		{
			
			ObjectOutputStream parameters = new ObjectOutputStream(new FileOutputStream(path+"/parameters.txt")) ;
			parameters.writeChars(multi_tree_run.run_configs.toString());
			parameters.close();
			//System.out.println(train_mogp.solutions.size());
			Boolean[][] all_train_predictions = new Boolean[multi_tree_run.train_data.count()][multi_tree_run.solutions.size()] ;
			Boolean[][] all_val_predictions = new Boolean[this.val_data.count()][multi_tree_run.solutions.size()] ;
			Boolean[][] all_test_predictions = new Boolean[this.test_data.count()][multi_tree_run.solutions.size()] ;
			for (Solution sol : multi_tree_run.solutions)
			{
				Double model_dist = 0.0 ; 
				
				Properties Train_props = new Properties() ;
				Properties Val_props = new Properties() ; 
				Properties Test_props = new Properties() ; 
				Properties rule_props = new Properties() ; 
				Map<String, Object> rule_prop_hashmap = new HashMap<String, Object>() ; 
				
				
				Boolean[][] train_predictions_all =  TrainMultiTrees.predict_all(sol,this.multi_tree_run.train_data,this.multi_tree_run.features.keySet()) ;
				Boolean[][] val_predictions_all =  TrainMultiTrees.predict_all(sol, this.val_data, this.multi_tree_run.features.keySet()) ; 
				Boolean[][] test_predictions_all =  TrainMultiTrees.predict_all(sol, this.test_data, this.multi_tree_run.features.keySet()) ; 
				
				Boolean[] train_predictions =  TrainMultiTrees.majority_voting(train_predictions_all) ;
				Boolean[] val_predictions =  TrainMultiTrees.majority_voting(val_predictions_all) ; 
				Boolean[] test_predictions =  TrainMultiTrees.majority_voting(test_predictions_all) ; 
				for (int j = 0 ; j < multi_tree_run.train_data.count() ; j++)
				{
					all_train_predictions[j][i] = train_predictions[j] ;  
				}
				for (int j = 0 ; j < this.test_data.count() ; j++)
				{
					all_test_predictions[j][i] = test_predictions[j] ;  
				}
				for (int j = 0 ; j < this.val_data.count() ; j++)
				{
					all_val_predictions[j][i] = val_predictions[j] ;  
				}
				model_path = path+"/model" + ++i ; 
				File dir = new File(model_path);
				dir.mkdirs();
				train_confusion_matrix = new FileOutputStream(model_path+"/train_confusion_matrix.xml") ;
				val_confusion_matrix = new FileOutputStream(model_path+"/val_confusion_matrix.xml") ; 
				test_confusion_matrix = new FileOutputStream(model_path+"/test_confusion_matrix.xml") ; 
				rules_propreties = new FileOutputStream(model_path+"/rule_propreties.xml") ; 

				//ObjectOutputStream model_rule = new ObjectOutputStream(new FileOutputStream(model_path+"/rule.txt")) ;
				
				
				train_matrix = new ConfusionMatrix(multi_tree_run.true_train_labels,train_predictions) ; 
				val_matrix = new ConfusionMatrix(true_val_labels,val_predictions) ; 	
				test_matrix = new ConfusionMatrix(true_test_labels,test_predictions) ; 	
				if (sol.getNumberOfObjectives() == 1) {
					best_model = sol ; 
					best_model_train_confusion_matrix = train_matrix ; 
					best_test_confusion_matrix = test_matrix ; 
					continue ; 
				}
				for (int k = 0 ; k < this.perfect_point.length - 1; k++)
				{
					if (!objectives[k].contains("-")) {
						model_dist+= (perfect_point[k] - sol.getObjective(k)*-1.0)*(perfect_point[k] - sol.getObjective(k)*-1.0) ;
					}
					
					else {
						model_dist+= (this.perfect_point[k] - sol.getObjective(k))*(perfect_point[k] - sol.getObjective(k)) ;
					}
					/*updating plots series 
					for (int n = k + 1 ; n < this.perfect_point.length; n++) {
						int index = (k*this.perfect_point.length + n) /2; 
						double first_train_objective = sol.getObjective(k) ; 
						double second_train_objective = sol.getObjective(n) ; 
						double first_test_objective,second_test_objective  ; 
						first_test_objective = (double) Double.parseDouble((String) test_matrix.get_statistics().get(objectives[k].replace("-", ""))) ; 
						second_test_objective = (double) Double.parseDouble((String) test_matrix.get_statistics().get(objectives[n].replace("-", ""))) ; 
						if (!objectives[k].contains("-")) {
							first_train_objective = first_train_objective*-1.0 ; 
							
						}
						if (!objectives[n].contains("-")) {
							second_train_objective = second_train_objective*-1.0 ; 
						}
						train_series[index].add(first_train_objective, second_train_objective);
						test_series[index].add(first_test_objective, second_test_objective);

						
					}*/
				}
				/*if(model_dist < min_dist)
				{
					best_model = sol ; 
					best_model_train_confusion_matrix = train_matrix ; 
					best_test_confusion_matrix = test_matrix ; 
					min_dist =  model_dist ;
				}*/
				//train_series.add((double) Double.parseDouble((String) train_matrix.get_statistics().get("tpr")),(double) Double.parseDouble((String)train_matrix.get_statistics().get("tnr")));
				//test_series.add((double) Double.parseDouble((String)test_matrix.get_statistics().get("tpr")),(double) Double.parseDouble((String)test_matrix.get_statistics().get("tnr")));
				
				
				
				//train_confusion_matrix.writeChars(train_matrix.get_statistics().toString());
				//test_confusion_matrix.writeChars(test_matrix.get_statistics().toString());
				for (int ivar = 0 ; ivar < sol.getNumberOfVariables() ; ivar++) {
					File tree_dir = new File(model_path+"/tree_"+ivar);
					tree_dir.mkdirs();
					GraphViz.createDotGraph(((Node)sol.getVariable(ivar)).getNodeAt(1).todot((long) 1), model_path+"/tree_"+ivar+"/tree.pdf" );
					try (PrintStream out = new PrintStream(new FileOutputStream(model_path+"/tree_"+ivar+"/rule.txt"))) {
					    out.print(((Node)sol.getVariable(ivar)).getNodeAt(1).todot((long) 1).toString());
					}
				}
					
				
                 
				Train_props.putAll(train_matrix.get_statistics());
				Val_props.putAll(val_matrix.get_statistics()) ; 
				Test_props.putAll(test_matrix.get_statistics());

				//System.out.println(Test_props) ; 
				//System.out.println(Train_props) ; 
				
				Train_props.storeToXML(train_confusion_matrix, "train performance");
				Val_props.storeToXML(val_confusion_matrix, "val performance");
				Test_props.storeToXML(test_confusion_matrix, "test performance");
				
				rule_prop_hashmap.put("fit_time_ms", String.valueOf(this.multi_tree_run.fit_time)) ; 
				
				rule_props.putAll(rule_prop_hashmap);
				rule_props.storeToXML(rules_propreties, null);
				
				train_confusion_matrix.close();
				val_confusion_matrix.close();
				test_confusion_matrix.close(); 
				rules_propreties.close() ; 
			}
			// saving best model data 
			//GraphViz.createDotGraph(((Node)best_model.getVariable(0)).getNodeAt(1).todot(1), path+"/best_model_performance/tree");
			
			Properties Train_props = new Properties() ; 
			Properties Val_props = new Properties() ; 
			Properties Test_props = new Properties() ; 
			

			//Train_props.putAll(best_model_train_confusion_matrix.get_statistics());
			//Test_props.putAll(best_test_confusion_matrix.get_statistics());
			
			//Train_props.storeToXML(new FileOutputStream(path+"/best_model_performance/train_confusion_matrix.xml"), "train performance");
			//Test_props.storeToXML(new FileOutputStream(path+"/best_model_performance/test_confusion_matrix.xml"), "test performance");
			//writeFeatureOcc(best_model ,path+"/best_model_performance"); 
			
			save_predictions(all_train_predictions,path + "/train_predictions.csv") ; 
			save_predictions(all_val_predictions,path + "/val_predictions.csv") ; 
			save_predictions(all_test_predictions,path + "/test_predictions.csv") ; 
			for (int k = 0 ; k < objectives.length ; k++) {
				for (int n = k + 1 ; n < objectives.length ; n++) {
					int index  = (k*objectives.length + n) /2 ; 
					OutputStream chart_stream = new FileOutputStream(path+"/"+objectives[k].replace("-","")+"_" + objectives[n].replace("-","")+".png"); 
					XYSeriesCollection data = new XYSeriesCollection() ; 
					data.addSeries(train_series[index]);
					data.addSeries(val_series[index]);
					data.addSeries(test_series[index]);
					JFreeChart chart = ChartFactory.createScatterPlot(
							"Algorithm : "+this.multi_tree_run.run_configs.get("algorithm") +  " population size:" + this.multi_tree_run.run_configs.get("populationsize") + " max generation number: " + this.multi_tree_run.run_configs.get("maxgeneration") + " crossover rate: "+this.multi_tree_run.run_configs.get("crossover_rate") + " mutation rate: "+this.multi_tree_run.run_configs.get("mutation_rate") ,
							objectives[k].replace("-",""), 
							objectives[n].replace("-",""), 
			    	        data,
			    	        PlotOrientation.VERTICAL,
			    	        true,
			    	        true,
			    	        false
							 );
					ChartUtilities.writeChartAsPNG(chart_stream,
				            chart,
				            1000,
				            1000);
				}
			}
			

		}
		catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	public void  save_predictions(Boolean[][] matrix,String filepath)
	{
		//writing header
		StringBuilder header = new StringBuilder();
		for (int i = 0 ; i < matrix[0].length ; i++)
		{
			if (i < matrix[0].length - 1) 
				header.append("model"+(i+1)+",") ; 
			else  
				header.append("model"+(i+1)+"\n") ;
		}
		writeAtTheEnd(header, filepath);
		//writing the data
		for (int irow = 0 ; irow < matrix.length ; irow++)
		{
			StringBuilder row = new StringBuilder();	
			for (int icol = 0 ; icol < matrix[irow].length ; icol++)
			{
				if (icol < matrix[irow].length - 1) 
					row.append(matrix[irow][icol]+",") ; 
				else  
					row.append(matrix[irow][icol]+"\n") ;
			}
			writeAtTheEnd(row, filepath);
		}
	}
	
    public  static  void writeAtTheEnd (StringBuilder s,String filePath) {
		try(
				FileWriter fw = new FileWriter( filePath, true);
			    BufferedWriter bw = new BufferedWriter(fw);
			    PrintWriter out = new PrintWriter(bw))
			{
			    out.print(s.toString());
			    out.close();
			} catch (IOException e) {
				  e.printStackTrace();
			}
    }
}
