package MOEA_GP;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.Map.Entry;

import org.moeaframework.core.Solution;
import org.moeaframework.core.variable.Program;
import org.moeaframework.problem.AbstractProblem;
import org.moeaframework.util.tree.* ; 
import core.ConfusionMatrix;
import core.CsvDataFrame;
import core.DataFrame;

public class Train_MOGP extends AbstractProblem {
	
	private DataFrame train_data ; 
    Object[] true_labels  ; 
    private Rules rules;
    private Set<String> features ; 
    private String[] Objectives ; 
    private ArrayList<Node> must_contain; 
    
	public Train_MOGP(DataFrame data,Object[] true_labels, HashMap<String,Integer> feature_columns,String[] Objectives)
	{
		
		super(1, Objectives.length, 0);
		
		this.Objectives = Objectives ; 
        this.features = feature_columns.keySet() ; 
        this.train_data = data;
		this.rules = new Rules();
		this.rules.add(new And());
		this.rules.add(new Or());
		//this.rules.add(new OR3());
		//this.rules.add(new AND3());
		//this.rules.add(new MajorityVote3());
		//this.rules.add(new OR3());
		//this.rules.add(new AND3());
		//this.rules.add(new Xor());
		//this.rules.add(new IfElse(Boolean.class));
		//this.rules.add(new LogisticReg(new Class<?> [] {Object.class, Object.class, Object.class} )) ; 
		//this.rules.add(new LogisticReg(new Class<?> [] {Object.class, Object.class, Object.class, Object.class, Object.class, } )) ; 
		//this.rules.add(new LogisticReg(new Class<?> [] {Object.class, Object.class, Object.class, Object.class, Object.class,Object.class, Object.class, } )) ; 
		//this.rules.add(new Add());
		//this.rules.add(new Subtract());
		//this.rules.add(new Xor());
		
		//this.rules.add(new Not());
		/*this.rules.add(new Add());
		this.rules.add(new Divide());
		this.rules.add(new Multiply());
		this.rules.add(new Max());
		this.rules.add(new Min());
		this.rules.add(new Log10());
		this.rules.add(new GreaterThanOrEqual());
		this.rules.add(new LessThanOrEqual());
	    this.rules.add(new GreaterThan());
		this.rules.add(new SquareRoot());
		this.rules.add(new Square());
	    this.rules.add(new LessThan());
	    this.rules.add(new EphermeralConstant(0.0, 100.0));*/
	    //this.rules.add(new EphermeralConstant(0.0, 1.0));
		this.must_contain = new ArrayList<Node>() ; 
		//this.must_contain.add(new Equal_threshold("FFT_prediction", (Double)0.0, (Double)0.0, (Double)1.0, 1)) ; 
		//this.must_contain.add(new Equal_threshold("FFT_prediction", (Double)(0.0), (Double)(0.0), (Double)0.0, 1)) ; 
		
		
	    //this.rules.add(new Equal_threshold("FFT_prediction", (Double)0.0, (Double)0.0, (Double)1.0, 1));
	    //this.rules.add(new Equal_threshold("FFT_prediction", (Double)0.0, (Double)0.0, (Double)0.0, 1));
		
		this.rules.add(new Greater_than_threshold("wmc", -1.0, -1.0, 44.75999999999988, 0));
		this.rules.add(new Lesser_than_threshold("dit", -1.0, -1.0, 4.0, 0));
		this.rules.add(new Greater_than_threshold("noc", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("cbo", -1.0, -1.0, 3.0, 0));
		this.rules.add(new Greater_than_threshold("rfc", -1.0, -1.0, 106.75999999999999, 0));
		this.rules.add(new Greater_than_threshold("lcom", -1.0, -1.0, 718.92, 0));
		this.rules.add(new Greater_than_threshold("ca", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("ce", -1.0, -1.0, 3.0, 0));
		this.rules.add(new Greater_than_threshold("npm", -1.0, -1.0, 41.48000000000002, 0));
		this.rules.add(new Lesser_than_threshold("lcom3", -1.0, -1.0, 0.99863888892, 0));
		this.rules.add(new Greater_than_threshold("loc", -1.0, -1.0, 80.59999999999997, 0));
		this.rules.add(new Greater_than_threshold("dam", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("moa", -1.0, -1.0, 1.0, 0));
		this.rules.add(new Lesser_than_threshold("mfa", -1.0, -1.0, 0.979166667, 0));
		this.rules.add(new Lesser_than_threshold("cam", -1.0, -1.0, 0.5600000002000004, 0));
		this.rules.add(new Greater_than_threshold("ic", -1.0, -1.0, 1.0, 0));
		this.rules.add(new Greater_than_threshold("cbm", -1.0, -1.0, 6.0, 0));
		this.rules.add(new Greater_than_threshold("amc", -1.0, -1.0, 7.3873548387999985, 0));
		this.rules.add(new Greater_than_threshold("max_cc", -1.0, -1.0, 2.0, 0));
		this.rules.add(new Greater_than_threshold("avg_cc", -1.0, -1.0, 1.0, 0));
		
		this.rules.add(new Greater_than_threshold("wmc", -1.0, -1.0, 9.0, 0));
		this.rules.add(new Greater_than_threshold("dit", -1.0, -1.0, 1.0, 0));
		this.rules.add(new Greater_than_threshold("noc", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("cbo", -1.0, -1.0, 3.0, 0));
		this.rules.add(new Greater_than_threshold("rfc", -1.0, -1.0, 16.0, 0));
		this.rules.add(new Lesser_than_threshold("lcom", -1.0, -1.0, 3.0, 0));
		this.rules.add(new Greater_than_threshold("ca", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("ce", -1.0, -1.0, 3.0, 0));
		this.rules.add(new Lesser_than_threshold("npm", -1.0, -1.0, 5.0, 0));
		this.rules.add(new Lesser_than_threshold("lcom3", -1.0, -1.0, 0.99863888892, 0));
		this.rules.add(new Greater_than_threshold("loc", -1.0, -1.0, 80.59999999999997, 0));
		this.rules.add(new Greater_than_threshold("dam", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("moa", -1.0, -1.0, 0.0, 0));
		this.rules.add(new Greater_than_threshold("mfa", -1.0, -1.0, 0.2, 0));
		this.rules.add(new Lesser_than_threshold("cam", -1.0, -1.0, 0.5171717174000001, 0));
		this.rules.add(new Greater_than_threshold("ic", -1.0, -1.0, 1.0, 0));
		this.rules.add(new Greater_than_threshold("cbm", -1.0, -1.0, 2.0, 0));
		this.rules.add(new Greater_than_threshold("amc", -1.0, -1.0, 7.3873548387999985, 0));
		this.rules.add(new Greater_than_threshold("max_cc", -1.0, -1.0, 1.0, 0));
		this.rules.add(new Greater_than_threshold("avg_cc", -1.0, -1.0, 1.0, 0));

		
		
		
		
		
	    rules.setReturnType(Boolean.class);
		rules.setMaxInitializationDepth(10);
		//rules.setFunctionCrossoverProbability(0.8);
		rules.setMaxVariationDepth(10);
		
		this.true_labels = true_labels ; 
		for (Entry<String, Integer> entry : feature_columns.entrySet())
		{
			HashMap<String, Object> feature_limits = this.train_data.get_column_limits(entry.getKey()) ;
			
			//System.out.println(entry.getKey()) ; 
			//System.out.println(entry.getKey(*));
			this.rules.add(new Greater_than_threshold(entry.getKey(),(Double)feature_limits.get("min"),(Double) feature_limits.get("max"),-1.0,entry.getValue()));
			this.rules.add(new Lesser_than_threshold(entry.getKey(),(Double)feature_limits.get("min"),(Double) feature_limits.get("max"),-1.0,entry.getValue()));
			//this.rules.add(new Get(Double.class, entry.getKey()));
			//this.rules.add(new Interval(entry.getKey(),(Double)feature_limits.get("min"),(Double) feature_limits.get("max"),entry.getValue(),null));
			//this.rules.add(new Get(Double.class,entry.getKey()));
			
		}
		
	}
	@Override
	public void evaluate(Solution solution)
	{
		Boolean [] outputs =Gp_try.compute_predictions(solution,this.train_data.getData(),features)  ; 
		ConfusionMatrix matrix = new ConfusionMatrix(this.true_labels,(Object[])outputs) ;
		//solution.setConstraint(0, 0.0);
		/*solution.setConstraint(1, 0.0);
	
		int num_existant_nodes = this.countNumMustExistNode(solution) ; 
		if (num_existant_nodes >= 1) {
			solution.setConstraint(1, 0.0);
		}
		else {
			solution.setConstraint(1,1.0);
		}*/
		//solution.setConstraint(1, 0.0);
		for (int i = 0 ; i < this.numberOfObjectives ; i++)
		{
			if (this.Objectives[i] == "G")
			{
				solution.setObjective(i,-1*matrix.G_measure());
			}
			if (this.Objectives[i] == "accuracy")
			{
				solution.setObjective(i,-1*matrix.accuracy());
			}
			if (this.Objectives[i] == "f1")
			{
				solution.setObjective(i,-1*matrix.F1_measure());
			}
			if (this.Objectives[i] == "tnr")
			{
				solution.setObjective(i,-1*matrix.specificity());
			}
			if (this.Objectives[i] == "tpr")
			{
				solution.setObjective(i,-1*matrix.sensitivity());
			}
			if (this.Objectives[i] == "maxheight")
			{
				solution.setObjective(i,((Node)solution.getVariable(0)).getMaximumHeight());
			}
			if (this.Objectives[i] == "nodenumber")
			{
				//solution.setObjective(i,((Node)solution.getVariable(0)).getNumberOfNodes());
				int functions_number = ((Node)solution.getVariable(0)).getNumberOfFunctions() ;
				int terminal_number = ((Node)solution.getVariable(0)).getNumberOfTerminals() ;
				solution.setObjective(i,(functions_number + terminal_number)*1.0/32); 
				int n_nodes = ((Node)solution.getVariable(0)).getNumberOfNodes() ; 
				/*if (n_nodes <= 5) {
					solution.setConstraint(0, 0);
				}
				else {
					solution.setConstraint(0, n_nodes - 9);
				}*/
				if (n_nodes  >= 7 ) {
					solution.setConstraint(0, 0.0);
				}
				else {
						solution.setConstraint(0, 7 - n_nodes);
				}
			}
			if (this.Objectives[i] == "-fpr")
			{
				solution.setObjective(i,1*matrix.fpr());
			}
			if (this.Objectives[i] == "f1")
			{
				solution.setObjective(i,-1*matrix.F1_measure());
			}
			if (this.Objectives[i] == "G")
			{
				solution.setObjective(i,-1*matrix.G_measure());
			}
			if (this.Objectives[i] == "-fnr")
			{
				solution.setObjective(i,1*matrix.fnr());
			}
			if (this.Objectives[i] == "tpr-fpr")
			{
				solution.setObjective(i,-1*(matrix.specificity() + (1-matrix.fpr())));
			}
			if (this.Objectives[i] == "tnr-fnr")
			{
				solution.setObjective(i,-1*(matrix.sensitivity() + (1-matrix.fnr())));
			}
			if (this.Objectives[i] == "precision")
			{
				solution.setObjective(i,-1*(matrix.precision()));
			}
			if (this.Objectives[i] == "mean_tpr_tnr")
			{
				solution.setObjective(i,-1*0.5*(matrix.sensitivity() + matrix.specificity()));
			}
			if (this.Objectives[i] == "MCC")
			{
				solution.setObjective(i,-1*(matrix.MCC()));
			}
			if (this.Objectives[i] == "-balance")
			{
				solution.setObjective(i,1*(matrix.balance()));
			}
			if (this.Objectives[i] == "MCC_times_f1")
			{
				solution.setObjective(i,-1*(matrix.MCC()*matrix.F1_measure()));
			}
			if (this.Objectives[i] == "-MisCost")
			{
				double c = 1.0 ; 
				double alpha = 5.0 ; 
				double fp = matrix.getFp() ; 
				double fn = matrix.getFn() ;
				double cost = (fn + alpha*fp)*1.0/this.train_data.count(); 
				solution.setObjective(i,cost);
			}
			
			
		}
		/*Double tpr = matrix.specificity()  ; 
		Double tnr = matrix.sensitivity() ; 
	    System.out.println("tpr:"+tpr+" tnr:"+tnr) ; 
		System.out.println("acc:"+matrix.accuracy()) ; 
		System.out.println("f1:"+matrix.F1_measure()) ; 
		if (tpr > 0.9 && tnr > 0.9)
			System.out.println("WoW!!") ;  
		solution.setObjective(0,-1*tpr);
		solution.setObjective(1,-1*tnr);*/
	}
	public int countNumMustExistNode(Solution solution) {
		ArrayList<Node> terminal_list = ((Node)solution.getVariable(0)).getTerminals() ; 
		int num_node_exist = 0 ;
		Greater_than_threshold check_exist_greater = null ; 
		Lesser_than_threshold check_exist_lesser  = null ;
		Equal_threshold check_exist_equal  = null ;
		for (Node must_exist: this.must_contain) {
			if (must_exist instanceof Greater_than_threshold) {
				check_exist_greater = (Greater_than_threshold) must_exist;
			}
			if (must_exist instanceof Lesser_than_threshold) {
				check_exist_lesser = (Lesser_than_threshold) must_exist;
			}
			
			if (must_exist instanceof Equal_threshold) {
				check_exist_equal = (Equal_threshold) must_exist;
			}
			
			for (Node terminal: terminal_list) {
				if (check_exist_greater != null) {
					if (terminal instanceof Greater_than_threshold) {
						if (check_exist_greater.is_equal((Greater_than_threshold)terminal)) {
							num_node_exist += 1 ; 
							break; 
						}
					}
				}
				if (check_exist_lesser != null) {
					if (terminal instanceof Lesser_than_threshold) {
						if (check_exist_lesser.is_equal((Lesser_than_threshold)terminal)) {
							num_node_exist += 1 ; 
							break; 
						}
					}
				}
				if (check_exist_equal != null) {
					if (terminal instanceof Equal_threshold) {
						if (check_exist_equal.is_equal((Equal_threshold)terminal)) {
							num_node_exist += 1 ; 
							break; 
						}
					}
				}
			}
		}
		return num_node_exist ; 
	}
	@Override
	public Solution newSolution() { 
		Solution solution = new Solution(1, this.Objectives.length,this.numberOfConstraints); 
		solution.setVariable(0, new Program(rules));
		return solution;
	}
}
