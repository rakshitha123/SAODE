import weka.classifiers.Classifier;
import weka.classifiers.UpdateableClassifier;
import weka.classifiers.bayes.NaiveBayesUpdateable;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;


public class Multiple_Naive_Bayes extends Classifier implements UpdateableClassifier {

    Classifier m_childClassifiers[] = null;
    Instances m_childInstances[] = null;

    @Override
    public void buildClassifier(Instances data) throws Exception {
        Attribute timeAttr = data.attribute(0);  // find the number of values in the time attribute

        m_childClassifiers = new Classifier[getSplitCount(timeAttr)];   // allocate child classifier and child instances structures
        m_childInstances= new Instances[getSplitCount(timeAttr)];

        Attribute classAttribute = data.classAttribute();

        int childClassIndex = -1;
        FastVector attributes = new FastVector();
        for (int i = 0; i < data.numAttributes(); ++i) {
            Attribute attr = data.attribute(i);
            if (attr != timeAttr) {
                attributes.addElement(attr);
                if (attr == classAttribute) {
                    childClassIndex = attributes.size() - 1;
                }
            }
        }

        for (int i = 0; i < m_childInstances.length; i++) {
            m_childInstances[i]=new Instances(data.relationName(), attributes, 0);
            m_childInstances[i].setClassIndex(childClassIndex);
        }

        for(int i=0;i<data.numInstances();i++){
            int timeAtt = getSplit(data.instance(i).value(0));
            Instance tempInstance = new Instance(data.instance(i).weight(), data.instance(i).toDoubleArray());
            tempInstance.deleteAttributeAt(0);
            m_childInstances[timeAtt].add(tempInstance);
        }

        for (int i = 0; i < m_childClassifiers.length; ++i) {
            m_childClassifiers[i] = new NaiveBayesUpdateable();  // Creating multiple Naive Bayes classifiers one per each season
            m_childClassifiers[i].buildClassifier(m_childInstances[i]);
        }
    }

    @Override
    public double[] distributionForInstance(Instance instance) throws Exception {   //Find the probability distribution for all class labels  for a given instance
        int timeAtt = getSplit(instance.value(0));
        Instance childInstance = new Instance(instance.weight(), instance.toDoubleArray());
        childInstance.deleteAttributeAt(0);
        childInstance.setDataset(m_childInstances[timeAtt]);

        return m_childClassifiers[timeAtt].distributionForInstance(childInstance);
    }

    @Override
    public void updateClassifier(Instance instance) throws Exception {  //Update classifier attributes when it finds a new instance
        int timeAtt = getSplit(instance.value(0));
        Instance childInstance = new Instance(instance.weight(), instance.toDoubleArray());
        childInstance.deleteAttributeAt(0);
        childInstance.setDataset(m_childInstances[timeAtt]);

        ((UpdateableClassifier) m_childClassifiers[timeAtt]).updateClassifier(instance);
    }

    /**
     *
     * @param timeValue value of the time attribute for an instance
     * @return the zero-based index for the split
     */
    protected int getSplit(double timeValue) {
        return (int) timeValue;
    }

    /**
     *
     * @param timeAttr attribute encoding the time in the data
     * @return the number of classifiers to split the data into
     */
    protected int getSplitCount(Attribute timeAttr) {
        return timeAttr.numValues();
    }

    @Override
    public String getRevision() {
        return null;
    }
}
