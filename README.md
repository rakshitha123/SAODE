# SAODE_Exp
SAODE Related Experiments

SAODE_Exp.java file: 
It contains all experiments we did to evaluate our SAODE classifier against 9 state-of-the-art stream and concept drift classification models: Hoeffding Tree, Hoeffding Option Tree, OzaBag, OzaBoost, Hoeffding Adaptive Tree, Accuracy Updated Ensemble, Leveraging Bagging , Naive Bayes (NB) and Averaged One-Dependence Estimators (AODE) each with and without the consideration of the seasonal feature. We also compare SAODE with the variations of NB, Hoeffding Tree and AODE containing multiple classifiers: one for each considered season. We implemented SAODE and multiple classifiers using Weka platform. We used the Weka and MOA buit-in classifiers to test other classifiers.

Multiple_Naive_Bayes.java: 
It builds multiple Naive Bayes classifiers one per each season in the classification process.

Multiple_Hoeffding_Tree.java: 
It builds multiple Hoeffding Tree classifiers one per each season in the classification process.

Multiple_AODE.java: 
It builds multiple AODE classifiers one per each season in the classification process.

SAODE.java
It contains the code for our new classifier: SAODE

Datasets - We used 2 datasets with our experiments: RCV1-v2 dataset and New York Times dataset.

The folder named "inputs" contains the four ".arff" filed used by Weka for classification. 
tc2000_804414_none_b.arff: Arff file created from RCV1-v2 dataset without using a seasonal feature.
tc2000_804414_dow_b.arff: Arff file created from RCV1-v2 dataset considering day of the week as the seasonal feature.
no_time.arff: Arff file created from New York Times dataset without using a seasonal feature.
time.arff: Arff file created from New York Times dataset considering day of the week as the seasonal feature.
