import weka.classifiers.Classifier;
import weka.classifiers.UpdateableClassifier;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.OptionHandler;
import weka.core.RevisionUtils;
import weka.core.WeightedInstancesHandler;


public class SAODE extends Classifier implements OptionHandler, WeightedInstancesHandler, UpdateableClassifier {

    private double[] m_ClassCounts;
    private double[][] m_SumForCounts;
    private int m_NumClasses;
    private int m_NumAttributes;
    private int m_NumInstances;
    private int m_ClassIndex;
    private Instances m_Instances;
    private int m_TotalAttValues;
    private int[] m_StartAttIndex;
    private int[] m_NumAttValues;
    private double[] m_Frequencies;
    private double m_SumInstances;
    private int m_Limit = 1;   // Use m=1 for all experiments. This value is used to choose parent attributes. It also uses m=1 as the default value of the Weka built-in AODE classifier and for the comparison purpose, we also use the same value
    private double[][][][] m_TimeCondiCounts;
    private double[][] m_TimeCounts;
    private double[][] m_TimeFrequencies;
    private int m_NumTimeVals;


    public SAODE() {
    }


    public void buildClassifier(Instances instances) throws Exception {  //Creating required arrays to store counts which will be then used to calculate probabilities
        this.m_Instances = new Instances(instances);
        this.m_Instances.deleteWithMissingClass();
        this.m_SumInstances = 0.0;
        this.m_ClassIndex = instances.classIndex();
        this.m_NumInstances = this.m_Instances.numInstances();
        this.m_NumAttributes = this.m_Instances.numAttributes();
        this.m_NumClasses = this.m_Instances.numClasses();
        this.m_StartAttIndex = new int[this.m_NumAttributes];
        this.m_NumAttValues = new int[this.m_NumAttributes];
        this.m_TotalAttValues = 0;

        this.m_NumTimeVals = instances.attribute(0).numValues();
        this.m_NumAttValues[0] = this.m_NumTimeVals;

        for (int i = 1; i < this.m_NumAttributes; ++i) {
            if (i != this.m_ClassIndex) {
                this.m_StartAttIndex[i] = this.m_TotalAttValues;
                this.m_NumAttValues[i] = this.m_Instances.attribute(i).numValues();
                this.m_TotalAttValues += this.m_NumAttValues[i] + 1;
            } else {
                this.m_NumAttValues[i] = this.m_NumClasses;
            }
        }

        this.m_TimeCondiCounts = new double[this.m_NumClasses][this.m_NumTimeVals][this.m_TotalAttValues][this.m_TotalAttValues];
        this.m_TimeCounts = new double[this.m_NumClasses][this.m_NumTimeVals];

        this.m_ClassCounts = new double[this.m_NumClasses];
        this.m_SumForCounts = new double[this.m_NumClasses][this.m_NumAttributes];
        this.m_Frequencies = new double[this.m_TotalAttValues];
        this.m_TimeFrequencies = new double[this.m_NumTimeVals][this.m_TotalAttValues];

        for (int i = 0; i < this.m_NumInstances; ++i) {
            this.addToCounts(this.m_Instances.instance(i));
        }

        this.m_Instances = new Instances(this.m_Instances, 0);
    }

    public void updateClassifier(Instance instance) throws Exception { //Update label counts when it finds a new instance
        this.addToCounts(instance);
    }

    private void addToCounts(Instance instance) { //Adding label counts for the required arrays
        if (!instance.classIsMissing()) {
            int classNo = (int) instance.classValue();
            int timeNo = (int) instance.value(0);
            double weight_1 = instance.weight();
            this.m_ClassCounts[classNo] += weight_1;   // weight is equal to 1 with the considered scenarios
            this.m_TimeCounts[classNo][timeNo] += weight_1;
            this.m_SumInstances += weight_1;
            int[] attributeValues = new int[this.m_NumAttributes];

            attributeValues[0] = -1;
            for (int i = 1; i < this.m_NumAttributes; ++i) {
                if (i == this.m_ClassIndex) {
                    attributeValues[i] = -1;
                } else if (instance.isMissing(i)) {
                    attributeValues[i] = this.m_StartAttIndex[i] + this.m_NumAttValues[i];
                } else {
                    attributeValues[i] = this.m_StartAttIndex[i] + (int) instance.value(i);
                }
            }

            for (int i = 0; i < this.m_NumAttributes; ++i) {
                if (attributeValues[i] != -1) {
                    this.m_Frequencies[attributeValues[i]] += weight_1;
                    this.m_TimeFrequencies[timeNo][attributeValues[i]] += weight_1;
                    if (!instance.isMissing(i)) {
                        this.m_SumForCounts[classNo][i] += weight_1;
                    }

                    for (int j = 0; j < this.m_NumAttributes; ++j) {
                        if (attributeValues[j] != -1) {
                            this.m_TimeCondiCounts[classNo][timeNo][attributeValues[i]][attributeValues[j]] += weight_1;
                        }
                    }
                }
            }
        }
    }

    public double[] distributionForInstance(Instance instance) throws Exception {  //Find the probability distribution for all class labels  for a given instance
        double[] classProbs = new double[this.m_NumClasses];
        int[] attributeValueIndexes = new int[this.m_NumAttributes];

        int timeNo = (int) instance.value(0);

        attributeValueIndexes[0] = -1;
        for (int i = 1; i < this.m_NumAttributes; ++i) {
            if (!instance.isMissing(i) && i != this.m_ClassIndex) {
                attributeValueIndexes[i] = this.m_StartAttIndex[i] + (int) instance.value(i);
            } else {
                attributeValueIndexes[i] = -1;
            }
        }

        for (int i = 0; i < this.m_NumClasses; ++i) {
            classProbs[i] = 0.0;
            double currentProb = 0.0;
            int numberOfParents = 0;

            for (int j = 0; j < this.m_NumAttributes; ++j) {
                if (attributeValueIndexes[j] != -1) {
                    int requiredAttributeIndex = attributeValueIndexes[j];
                    if (this.m_Frequencies[attributeValueIndexes[j]] >= (double) this.m_Limit) {

                        attributeValueIndexes[j] = -1;
                        ++numberOfParents;

                        //Get the laplace estimation for the probability
                        currentProb = (this.m_TimeCondiCounts[i][timeNo][requiredAttributeIndex][requiredAttributeIndex] + 1.0) /
                                (this.m_SumInstances - this.m_TimeFrequencies[timeNo][this.m_StartAttIndex[j] + this.m_NumAttValues[j]] + (double) (this.m_NumClasses * this.m_NumAttValues[j] * this.m_NumTimeVals));

                        for (int k = 0; k < this.m_NumAttributes; ++k) {
                            if (attributeValueIndexes[k] != -1) {
                                double value = this.m_TimeCondiCounts[i][timeNo][requiredAttributeIndex][this.m_StartAttIndex[k] + this.m_NumAttValues[k]];

                                //Get the laplace estimation for the probability
                                currentProb *= (this.m_TimeCondiCounts[i][timeNo][requiredAttributeIndex][attributeValueIndexes[k]] + 1.0) /
                                        (this.m_TimeCondiCounts[i][timeNo][requiredAttributeIndex][requiredAttributeIndex] - value + (double) this.m_NumAttValues[k]);
                            }
                        }
                        classProbs[i] += currentProb;
                        attributeValueIndexes[j] = requiredAttributeIndex;
                    }
                }
            }

            double classProb = this.m_ClassCounts[i] + 1.0 / (this.m_SumInstances + this.m_NumClasses);
            double timeClassProb = this.m_TimeCounts[i][timeNo] + 1.0 / (this.m_SumInstances + (this.m_NumClasses * this.m_NumTimeVals));
            classProbs[i] = (classProbs[i] * classProb * timeClassProb) / (double) numberOfParents;  //Calculate the final probability distribution array
        }
        return classProbs;
    }

    public String toString() { //Provide a customized description about the classifier
        return "SAODE Classifier";
    }

    @Override
    public String getRevision() {
        return null;
    }
}
