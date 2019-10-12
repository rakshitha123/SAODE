# SAODE_Exp
SAODE Related Experiments

SAODE_Exp.java file 
It contains all experiments we did to evaluate our SAODE classifier against 9 state-of-the-art stream and concept drift classification models: Hoeffding Tree, Hoeffding Option Tree, OzaBag, OzaBoost, Hoeffding Adaptive Tree, Accuracy Updated Ensemble, Leveraging Bagging , Naive Bayes (NB) and Averaged One-Dependence Estimators (AODE) each with and without the consideration of the seasonal feature. We also compare SAODE with the variations of NB, Hoeffding Tree and AODE containing multiple classifiers: one for each considered season. 

Multiple_Naive_Bayes.java
It builds multiple Naive Bayes classifiers one per each season in the classification process.

Multiple_Hoeffding_Tree.java
It builds multiple Hoeffding Tree classifiers one per each season in the classification process.

Multiple_AODE.java
It builds multiple AODE classifiers one per each season in the classification process.
