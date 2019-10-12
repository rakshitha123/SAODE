import java.io.*;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;

import static java.nio.file.StandardOpenOption.*;

import adapters.MoaToWekaAdapter;
import weka.classifiers.Classifier;
import weka.classifiers.UpdateableClassifier;
import weka.classifiers.bayes.NaiveBayesUpdateable;
import weka.classifiers.bayes.AODE;
import weka.classifiers.trees.HoeffdingTree;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Utils;
import weka.core.converters.ArffLoader;

import moa.classifiers.meta.AccuracyUpdatedEnsemble;
import moa.classifiers.meta.LeveragingBag;
import moa.classifiers.meta.OzaBag;
import moa.classifiers.meta.OzaBoost;
import moa.classifiers.trees.HoeffdingAdaptiveTree;
import moa.classifiers.trees.HoeffdingOptionTree;


public class SAODE_Exp {

    static String sourceDirectory = "C:/Projects/SAODE_Exp/inputs/";

    final static int BATCH_SIZE = 500;
    final static int GC_BATCH_SIZE = 50000;
    final static int BUFFER_SIZE = 10000;


    protected static void evaluateClassifier(Classifier classifier, String classifierName, PrintWriter writer, String sourceFile) throws IOException, Exception {
        ArffLoader.ArffReader reader = new ArffLoader.ArffReader(new BufferedReader(new FileReader(sourceDirectory+sourceFile), BUFFER_SIZE), 10000);
        Instances structure = reader.getStructure();
        structure.setClassIndex(structure.numAttributes() - 1);

        UpdateableClassifier updater = (UpdateableClassifier) classifier;

        classifier.buildClassifier(structure);

        Instance inst;
        int nInstances = 0;
        int nErrors = 0;

        Attribute classAttribute = structure.classAttribute(); // get class value names
        String[] classValueNames = new String[classAttribute.numValues()];
        for (int i = 0; i < classAttribute.numValues(); i++) {
            classValueNames[i] = classAttribute.value(i);
        }

        writer.print("Actual");  // output header line
        for (String classValueName : classValueNames) {
            writer.format(",Pr%1$s", classValueName);
        }
        writer.format(",%1$s", "Predicted");
        writer.println();

        long start = System.currentTimeMillis();

        while ((inst = reader.readInstance(structure)) != null) {
            double[] distribution = classifier.distributionForInstance(inst);
            int predictedClass = Utils.maxIndex(distribution);
            int actualClass = (int) inst.classValue();
            if (predictedClass != actualClass) {
                nErrors++;
            }

            writer.print(classValueNames[actualClass]);
            for (int i = 0; i < distribution.length; i++) {
                writer.format(",%1$s", distribution[i]);
            }
            writer.format(",%1$s", classValueNames[predictedClass]);
            writer.println();

            updater.updateClassifier(inst);

            nInstances++;

            if (nInstances % BATCH_SIZE == 0) {
                long now = System.currentTimeMillis();
                long elapsed = now - start;
                start = now;

                System.out.println(nInstances+" "+nErrors+" "+((double) ( (nInstances - nErrors) * 100 / nInstances) )+ "%");
            }
            if (nInstances % GC_BATCH_SIZE == 0) {
                System.gc();
            }
        }
        System.out.format("classifier: %s, instance count: %d, error count: %d\n", classifierName, nInstances, nErrors);
        System.out.println(nInstances);
        System.out.println("Correct " + (double) (nInstances - nErrors));
        System.out.println("Accuracy percentage is " + ((double) (nInstances - nErrors) / nInstances) * 100);
    }



    protected static void startClassification(Classifier classifier, String classifierName, String sourceFileName) throws IOException, Exception {
        File dataFile = new File(sourceDirectory + sourceFileName);

        String sourceBaseFullFileName = dataFile.getName();  // make name of results file

        String sourceBaseFileName = sourceBaseFullFileName.substring(0, sourceBaseFullFileName.lastIndexOf('.'));  // drop file extension
        File resultsBaseFile = new File(sourceBaseFileName + '-' + classifierName);

        File resultsTempFile = new File(resultsBaseFile.getPath() + ".tmp");
        File resultsFile = new File(resultsBaseFile.getPath() + ".csv");

        try (FileChannel fc = FileChannel.open(resultsTempFile.toPath(), CREATE, WRITE, READ)) {
            FileLock fl = fc.tryLock();
            if (fl != null) {
                if (fc.size() > 0) {
                    fc.truncate(0).force(true);
                    System.out.format("discarding partial results: %s\n", resultsTempFile.getAbsolutePath());
                }

                Writer rawWriter = Channels.newWriter(fc, "UTF-8");
                try (PrintWriter writer = new PrintWriter(rawWriter)) {
                    evaluateClassifier(classifier, classifierName, writer, sourceFileName);
                }
            } else {
                return;
            }
        }
        resultsTempFile.renameTo(resultsFile);
    }



    public static void main(String args[]) throws Exception {
        /**
         * RCV1-v2 Data Set Related Experiments
         */

        //Classifiers without seasonal features
        startClassification(new MoaToWekaAdapter(new AccuracyUpdatedEnsemble()), "AccuracyUpdatedEnsemble", "tc2000_804414_none_b.arff");
        startClassification(new HoeffdingTree(), "HoeffdingTree", "tc2000_804414_none_b.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingOptionTree()), "HoeffdingOptionTree", "tc2000_804414_none_b.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingAdaptiveTree()), "HoeffdingAdaptiveTree", "tc2000_804414_none_b.arff");
        startClassification(new MoaToWekaAdapter(new LeveragingBag()), "LeveragingBag", "tc2000_804414_none_b.arff");
        startClassification(new MoaToWekaAdapter(new OzaBag()), "OzaBag", "tc2000_804414_none_b.arff");
        startClassification(new MoaToWekaAdapter(new OzaBoost()), "OzaBoost", "tc2000_804414_none_b.arff");
        startClassification(new NaiveBayesUpdateable(), "NaiveBayes", "tc2000_804414_none_b.arff");
        startClassification(new AODE(), "AODE", "tc2000_804414_none_b.arff");


        //Classifiers with seasonal features
        startClassification(new MoaToWekaAdapter(new AccuracyUpdatedEnsemble()), "AccuracyUpdatedEnsemble", "tc2000_804414_dow_b.arff");
        startClassification(new HoeffdingTree(), "HoeffdingTree", "tc2000_804414_dow_b.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingOptionTree()), "HoeffdingOptionTree", "tc2000_804414_dow_b.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingAdaptiveTree()), "HoeffdingAdaptiveTree", "tc2000_804414_dow_b.arff");
        startClassification(new MoaToWekaAdapter(new LeveragingBag()), "LeveragingBag", "tc2000_804414_dow_b.arff");
        startClassification(new MoaToWekaAdapter(new OzaBag()), "OzaBag", "tc2000_804414_dow_b.arff");
        startClassification(new MoaToWekaAdapter(new OzaBoost()), "OzaBoost", "tc2000_804414_dow_b.arff");
        startClassification(new NaiveBayesUpdateable(), "NaiveBayes", "tc2000_804414_dow_b.arff");
        startClassification(new AODE(), "AODE", "tc2000_804414_dow_b.arff");


        //Multiple classifiers one per each season
        startClassification(new Multiple_Hoeffding_Tree(), "MultipleHoeffdingTrees", "tc2000_804414_dow_b.arff");
        startClassification(new Multiple_Naive_Bayes(), "MultipleNaiveBayes", "tc2000_804414_dow_b.arff");
        startClassification(new Multiple_AODE(), "MultipleAODE", "tc2000_804414_dow_b.arff");


        //New classifier: SAODE
        startClassification(new SAODE(), "SAODE", "tc2000_804414_dow_b.arff");



        /**
         * NYTimes Data Set Related Experiments
         */

        //Classifiers without seasonal features
        startClassification(new MoaToWekaAdapter(new AccuracyUpdatedEnsemble()), "AccuracyUpdatedEnsemble", "no_time.arff");
        startClassification(new HoeffdingTree(), "HoeffdingTree", "no_time.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingOptionTree()), "HoeffdingOptionTree", "no_time.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingAdaptiveTree()), "HoeffdingAdaptiveTree", "no_time.arff");
        startClassification(new MoaToWekaAdapter(new LeveragingBag()), "LeveragingBag", "no_time.arff");
        startClassification(new MoaToWekaAdapter(new OzaBag()), "OzaBag", "no_time.arff");
        startClassification(new MoaToWekaAdapter(new OzaBoost()), "OzaBoost", "no_time.arff");
        startClassification(new NaiveBayesUpdateable(), "NaiveBayes", "no_time.arff");
        startClassification(new AODE(), "AODE", "no_time.arff");


        //Classifiers with seasonal features
        startClassification(new MoaToWekaAdapter(new AccuracyUpdatedEnsemble()), "AccuracyUpdatedEnsemble", "time.arff");
        startClassification(new HoeffdingTree(), "HoeffdingTree", "time.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingOptionTree()), "HoeffdingOptionTree", "time.arff");
        startClassification(new MoaToWekaAdapter(new HoeffdingAdaptiveTree()), "HoeffdingAdaptiveTree", "time.arff");
        startClassification(new MoaToWekaAdapter(new LeveragingBag()), "LeveragingBag", "time.arff");
        startClassification(new MoaToWekaAdapter(new OzaBag()), "OzaBag", "time.arff");
        startClassification(new MoaToWekaAdapter(new OzaBoost()), "OzaBoost", "time.arff");
        startClassification(new NaiveBayesUpdateable(), "NaiveBayes", "time.arff");
        startClassification(new AODE(), "AODE", "time.arff");


        //Multiple classifiers one per each season
        startClassification(new Multiple_Hoeffding_Tree(), "MultipleHoeffdingTrees", "time.arff");
        startClassification(new Multiple_Naive_Bayes(), "MultipleNaiveBayes", "time.arff");
        startClassification(new Multiple_AODE(), "MultipleAODE", "time.arff");


        //New classifier: SAODE
        startClassification(new SAODE(), "SAODE", "time.arff");
    }
}
