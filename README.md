# SAODE
SAODE Related Experiments

The "src" directory contains the ".java" files related to SAODE and multiple classifiers implementation. Inside this directory, there is a package named as "adapter" which contains the adaper to convert MOA classifiers to be compatible with Weka platform.

The "Scripts" directory contains the R scripts we used for data preprocessing and evaluation with the 2 datasets we used.

src/SAODE_Exp.java file: 
It contains all experiments we did to evaluate our SAODE classifier against 9 state-of-the-art stream and concept drift classification models: Hoeffding Tree, Hoeffding Option Tree, OzaBag, OzaBoost, Hoeffding Adaptive Tree, Accuracy Updated Ensemble, Leveraging Bagging , Naive Bayes (NB) and Averaged One-Dependence Estimators (AODE) each with and without the consideration of the seasonal feature. We also compare SAODE with the variations of NB, Hoeffding Tree and AODE containing multiple classifiers: one for each considered season. We implemented SAODE and multiple classifiers using Weka platform. We used the Weka and MOA buit-in classifiers to test other classifiers (adapter was used with this).

src/Multiple_Naive_Bayes.java: 
It builds multiple Naive Bayes classifiers one per each season in the classification process.

src/Multiple_Hoeffding_Tree.java: 
It builds multiple Hoeffding Tree classifiers one per each season in the classification process.

src/Multiple_AODE.java: 
It builds multiple AODE classifiers one per each season in the classification process.

src/SAODE.java
It contains the code for our new classifier: SAODE


Datasets: We used 2 datasets with our experiments: RCV1-v2 dataset and New York Times dataset.

Data files were preprocessed using the preprocessing R scripts in "Scripts" folder and converted into ".arff" formats to compatible with Weka platform. All classifiers take one of these ".arff" file as an input containing the instances that need to be classified. These ".arff" files can be accessed through the following link.
https://drive.google.com/file/d/1BXAPiYwB1Pom3O6UvdWzeSJwixZz0Vya/view?usp=sharing

