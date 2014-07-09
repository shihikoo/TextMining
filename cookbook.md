# Cookbook for Text miming Script
========

This program is designed to learn the pattern of Neropathic Pain related paper abstracts and then develop a classifier with decent accurancy. 

textmining.r contains most of the functions that can be used later in similar text-mining program. Pain.r is the script that uses those functions for neropathic pain.
The pipeline of the programe: 
<img src="/img/pipeline for text mining.png" alt="pipeline" width='80%' height='80%'>

## Manual preprocessing
The data are a xlsx file received from Gillian (name: Jing_painrefs_070514.txt, 33819 records with 4 of them empty) and nicki (Jing_NP_references.txt 33575). Gillian's data are considered the full data here, while nicki's data contains valuable information about the article types that aids the classification significantly.

Before running the scipt, it is required that the xlsx file has to be opened in Excel or similar program, keep only the needed columns (ID, Abstract, s1.included, s1.excluded, s2.included, s2.excluded, and convert/save the file as a txt file ( which should automatically change the file to a tab seprated file).

There was one paper (ID 19120) with no title but was identified as a NP-related paper. Hence, the title has been manully searched and added:
* 19120 δ-Opioid mechanisms for ADL5747 and ADL5859 effects in mice: analgesia, locomotion, and receptor internalization.

There was four papers that were found in nicki's dataset while only one of them is paper. At the same time, there are just one record in Gill's record but this one is actually a poster. This record has also been corrected manuly in Gill's record. 
* 1849	Journal Article	C. Zhao;J. M. Tall;R. A. Meyer;S. N. Raja	2003	Anti - allodynic effects of systemic and intrathecal morphine in two models of neuropathic pain in rats	The efficacy of opioids for neuropathic pain remains controversial. We investigated the effects of morphine on mechanical allodynia in two models of neuropathic pain in rats-the spared nerve injury (SNI, tight ligation of tibial and common peroneal nerves) and the spinal nerve ligation (SNL, L5/L6 tight ligation) models. Paw withdrawal threshold (PWT) to mechanical stimuli (VF hair) was measured using the up-down method in hairy and glabrous skin territories of the sural nerve(SNI) or in the mid-plantar paw(SNL). The PWT decreased significantly after SNI in hairy and glabrous skins (p<0.001). Thirty days after the SNI, the threshold in hairy skin (0.33 (0.21, 1.91)g; Median (25%,75% quartiles)) was significantly lower than in glabrous skin (1.85 (0.59, 3.5) g). In blinded experiments, both s.c. morphine (1, 3 and 10 mg/kg at 40 min intervals) and intrathecal (i.t.) morphine (0.1, 0.3, 0.6, 1.0 or 10 mug) dose-dependently attenuated mechanical allodynia induced by SNI measured in the hairy skin. The ED50 for the i.t. morphine was 0.66 (95%CI: 0.50-0.89) mug. The anti-allodynic effect of i.t. morphine (1.0 mug) was reversed by naloxone (5 mg/kg, i.p.). Furthermore, i.t. morphine (1 mug) had similar potencies in attenuating SNI-induced mechanical allodynia in glabrous and hairy skins. In SNL rats, PWT decreased significantly from 26 g at baseline to 2.7 (1.47, 3.51) g thirty days after SNL. Intrathecal morphine (30 mug) almost completely reversed the mechanical allodynia induced by SNL. Conclusions: 1) SNI-induced mechanical allodynia was more robust in hairy than in glabrous skin. 2) Systemic and i.t. morphine reverse the SNI-induced mechanical allodynia in a dose-dependent fashion. 3) i.t. morphine is also effective in reversing SNL-induced mechanical allodynia. Our results suggest that morphine, especially i.t. morphine, is likely to be effective in the treatment of neuropathic pain.	TRUE	FALSE	TRUE

## Preprocessing
1. Initialize a proper enviorment for the code to run should be done first with the function initiation() in pain.r script. This function should be editted as the requirement and condition changes.

2. Function read data() in pain.r is used to read the data from the txt files that were prepared with the previousely mentioned steps. 
Meeting/proceedings/conference records are not considered valid paper because its lack of enough number results. However, taking them out should larged increase the accurency of the classification. This is also the reason we are trying to merge nicki's and gill's data here to elimite all those records.

Here is a summary of the avaliable types of article:
                   article           article; meeting article; proceedings paper              book; meeting 
                     12682                          1                        434                          4 
       conference abstract           conference paper          conference review                 correction 
                      1489                        129                         17                         10 
      correction, addition                  editorial                    erratum                     letter 
                         1                         34                         11                         37 
                   meeting           meeting abstract              meeting paper                       note 
                      3209                        387                          3                         45 
         proceedings paper                    reprint                     review               short survey 
                         3                          4                       1378                         53 
       thesis/dissertation                       Book               Book Section
                         1                        140                         88

Only Book, Book section, article, review are kept for training the machine. Those procedures are done with function readData(), the output is the proper "orignal" data set. The script then call this function and save the output into "alldata".

Function preparedata() takes the alldata as input. It is made to remove the records with different dicisions made by s1 and s2, and then clean up the data from numbers, pounctuations, extra spaces, and stop words, by calling function normalizeabstract.

The script then creates corpus. One can also create a word cloud at this step.

Then, script creates document term matrix and removes the sparse terms.

If the Naive Bayes is chosen as the machine, the docuemtns term matrix has to be convert from the occurrence to Yes/No, as the Naive Bayes only takes the later one.

## Trainning  
In order to train, validate and test the learning algorithm, we create the index for trainning data, validation data and test data with the ratio (60%, 20%, 20%). K-fold cross validation cannot be done at this moment becaues of the limitation of the current computing power and memery storage, but will be implemented so it can be used when better perfomance computer is avaliable.  

Choice of models are naive Bayes, k nearest neighbor, support vector machine. 

We tested and tuned all of them and showed the analysis as following. Sensitivity(TP/(TP+FN), tpr) measures the proportion of actual positives which are correctly identified. Specificity(TN/(FP+TN), tnr) measures the proportion of negatives which are correctly identified. The aim of the classification is increase the specificity as much as possible while maintain a decent sensitivity. A typical sensitivity if around 80%, so currently we aim at a >80% sensitivity and a as good as possible specifity. Balanced accurancy is the mean of sensitivity and specificity, so a high blanced accurancy is a good proxy for good classifier. All trainning results are shown in solid lines and validation result shown in dashed lines. Sensitivity, specificity and balanced accurancy are in green, red and blue.

1. Naive Bayes model:
Naive Bayes model does't have tunning paramenters except the number of features. Here, we plotted feature curve and learnning curve.  

This figure is the sparslevel curve of document term matrix , which represent how number of features relates to the prediction result. It shows that the result is optimistic when the sparse level is at 0.9.
![Naive Bayes feature curve](/img/feature_curve_NB.png)

This figure is the learning curve when the sparse level at 0.9. It shows how the precition result changes with the number of training dataset. The balanced accurancy reaches 75% from ~2700, and it does not increase much more after that. 
![Naive Bayes learning curve with sparselevel 0.9](/img/learning_curve_NB_sl099.png)

Thus, for small trainning data set(>2700) Naive  is a good model giving a acceptable result with sensitivity at 75%, specificity around 43% and balanced accuracy around 53%.

2. kNN model
kNN model has one paramenter (k) to tune, on top of number of features and the size of the training data. 

One suggestion in choosing k is k = sqrt(n) as the first tempt but in our case k will have to be ~100, which may take a rather long time. We can try it once we obtain bette hardware. Typical choices of k are odd number range from 1 to 10, here we plotted for 1, 3, 5, 7, 9 in the following k curve as following.
![kNN k-curve with sparse level = 0.92](/img/k_curve_kNN_sl092.png)

For all k choices, the sensitivity is too low, althought the specificity is good. This is a surprising result since kNN has been reported to be a decent classifier. Also, the trainning speed is fast but the time for making prediction is rather long especially for large k. We use k = 1 for the following test since it gives best balanced accuracy in the k curve.

This plot is the feature curve with k = 3. 
![kNN feature curve with k = 3](/img/feature_curve_kNN_k3.png)

![kNN learning curve with sparselevel = 0.92 and k = 3](/img/learning_curve_kNN_sl092_k3.png)

*kNN takes a fairly long time to make predictions. 

3. SVM model
Here we choose the c-classification SVM model with RBL kenel. Parameters for RBL kenel are cost, gamma and sparse level. An ideal way of turning will be loop over all choices of parameters and find the parameters combination that gives the best validation result.

![SVM feature curve with c = 3 and gamma = 0.002](/img/feature_curve_SVM_c2_gamma0002.png)

![SVM cost curve with sparse level = 0.92 and gamma = 0.002](/img/cost_curve_SVM_sl092_gamma0002.png)

![SVM gamma curve with sparse level = 0.92 and cost = 2](/img/gamma_curve_SVM_sl092_c2.png)

![SVM learning curve with sparse level = 0.92, c = 3 and gamma = 0.002](/img/learning_curve_SVM_sl092_c2_gamma0002.png)