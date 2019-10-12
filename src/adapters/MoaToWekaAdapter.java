package adapters;

import com.yahoo.labs.samoa.instances.InstancesHeader;
import com.yahoo.labs.samoa.instances.WekaToSamoaInstanceConverter;

import weka.classifiers.UpdateableClassifier;
import weka.core.DenseInstance;
import weka.core.Instance;
import weka.core.Instances;

import java.util.Arrays;
import java.util.Enumeration;


public class MoaToWekaAdapter implements UpdateableClassifier {

    private static final long serialVersionUID = -2749212217130560336L;

    protected WekaToSamoaInstanceConverter m_converter;
    protected moa.classifiers.Classifier m_moaClassifier;
    protected int m_numClassValues;

    public MoaToWekaAdapter(moa.classifiers.Classifier moaClassifier) {
        this(moaClassifier, null);
    }

    public MoaToWekaAdapter(moa.classifiers.Classifier moaClassifier, String cliString) {
        this.m_converter = null;
        this.m_moaClassifier = moaClassifier;

        m_moaClassifier.prepareForUse();
        if (cliString != null) {
            m_moaClassifier.getOptions().setViaCLIString(cliString);
        }
    }

    @Override
    public void buildClassifier(Instances data) throws Exception {
        m_moaClassifier.resetLearning();
        this.m_converter = new WekaToSamoaInstanceConverter();
        m_numClassValues = data.classAttribute().numValues();

        for (Enumeration<Instance> e = data.enumerateInstances(); e.hasMoreElements();) {
            updateClassifier(e.nextElement());
        }
    }

    @Override
    public double[] distributionForInstance(Instance instance) throws Exception {
        com.yahoo.labs.samoa.instances.Instance moaInstance = wekaInstanceToMoa(instance);

        double[] votes = m_moaClassifier.getVotesForInstance(moaInstance);

        // votes are sometimes only for a subset of the values - pad out to full set of values
        if (votes.length < m_numClassValues) {
            votes = Arrays.copyOf(votes, m_numClassValues);
        }

        // normalise votes
        double sum = 0;
        for (double d : votes) {
            sum += d;
        }

        if (sum == 0 || Double.isNaN(sum) || Double.isInfinite(sum)) {
            // special handling for useless value from classifier (probably due to lack of training)
            for (int i = 0; i < votes.length; i++) {
                votes[i] = 1.0 / m_numClassValues;
            }
        } else {
            for (int i = 0; i < votes.length; i++) {
                votes[i] /= sum;
            }
        }

        return votes;
    }

    @Override
    public void updateClassifier(Instance instance) throws Exception {
        com.yahoo.labs.samoa.instances.Instance moaInstance = wekaInstanceToMoa(instance);

        if (m_moaClassifier.getModelContext() == null) {
            m_moaClassifier.setModelContext(new InstancesHeader(moaInstance.dataset()));
        }
        m_moaClassifier.trainOnInstance(moaInstance);
    }

    protected com.yahoo.labs.samoa.instances.Instance wekaInstanceToMoa(Instance instance) {
        // there is a bug in the samoaInstance() function for handling sparse instances, leading the 
        // class attribute not having the correct index set
        // to avoid bug we convert the instance to dense, and then convert to moa instance
        DenseInstance denseInstance = new DenseInstance(instance);
        denseInstance.setDataset(instance.dataset());
        com.yahoo.labs.samoa.instances.Instance moaInstance = m_converter.samoaInstance(denseInstance);

        return moaInstance;
    }
    
    @Override
    public String toString() {
        return m_moaClassifier.toString();
    }
}
