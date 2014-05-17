Text miming - Pain
========

Text-mining script for neuroscience paper related to "pain". 

The data are a xlsx file received from Gillian.

Before running the scipt, it is required that the xlsx file has to be opened in Excel or similar program, keep only the needed columns (ID, Abstract, s1.included, s1.excluded, s2.included, s2.excluded, and convert/save the file as a txt file ( which should automatically change the file to a tab seprated file).

# 19120 δ-Opioid mechanisms for ADL5747 and ADL5859 effects in mice: analgesia, locomotion, and receptor internalization.

1849	Journal Article	C. Zhao;J. M. Tall;R. A. Meyer;S. N. Raja	2003	Anti - allodynic effects of systemic and intrathecal morphine in two models of neuropathic pain in rats	The efficacy of opioids for neuropathic pain remains controversial. We investigated the effects of morphine on mechanical allodynia in two models of neuropathic pain in rats-the spared nerve injury (SNI, tight ligation of tibial and common peroneal nerves) and the spinal nerve ligation (SNL, L5/L6 tight ligation) models. Paw withdrawal threshold (PWT) to mechanical stimuli (VF hair) was measured using the up-down method in hairy and glabrous skin territories of the sural nerve(SNI) or in the mid-plantar paw(SNL). The PWT decreased significantly after SNI in hairy and glabrous skins (p<0.001). Thirty days after the SNI, the threshold in hairy skin (0.33 (0.21, 1.91)g; Median (25%,75% quartiles)) was significantly lower than in glabrous skin (1.85 (0.59, 3.5) g). In blinded experiments, both s.c. morphine (1, 3 and 10 mg/kg at 40 min intervals) and intrathecal (i.t.) morphine (0.1, 0.3, 0.6, 1.0 or 10 mug) dose-dependently attenuated mechanical allodynia induced by SNI measured in the hairy skin. The ED50 for the i.t. morphine was 0.66 (95%CI: 0.50-0.89) mug. The anti-allodynic effect of i.t. morphine (1.0 mug) was reversed by naloxone (5 mg/kg, i.p.). Furthermore, i.t. morphine (1 mug) had similar potencies in attenuating SNI-induced mechanical allodynia in glabrous and hairy skins. In SNL rats, PWT decreased significantly from 26 g at baseline to 2.7 (1.47, 3.51) g thirty days after SNL. Intrathecal morphine (30 mug) almost completely reversed the mechanical allodynia induced by SNL. Conclusions: 1) SNI-induced mechanical allodynia was more robust in hairy than in glabrous skin. 2) Systemic and i.t. morphine reverse the SNI-induced mechanical allodynia in a dose-dependent fashion. 3) i.t. morphine is also effective in reversing SNL-induced mechanical allodynia. Our results suggest that morphine, especially i.t. morphine, is likely to be effective in the treatment of neuropathic pain.	TRUE	FALSE	TRUE

                   article           article; meeting article; proceedings paper              book; meeting 
                     13212                          1                        450                          4 
       conference abstract           conference paper          conference review                 correction 
                      1551                        131                         17                         10 
      correction, addition                  editorial                    erratum                     letter 
                         1                         36                         11                         37 
                   meeting           meeting abstract              meeting paper                       note 
                      3326                        417                          3                         47 
         proceedings paper                    reprint                     review               short survey 
                         3                          4                       1405                         53 
       thesis/dissertation 
                         1 

                         
            flag_test
ab_test_pred TRUE FALSE
       TRUE   815   684
       FALSE  184  3813
                                          
               Accuracy : 0.8421          
                 95% CI : (0.8322, 0.8516)
    No Information Rate : 0.8182          
    P-Value [Acc > NIR] : 1.717e-06       
                                          
                  Kappa : 0.5556          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.8158          
            Specificity : 0.8479          
         Pos Pred Value : 0.5437          
         Neg Pred Value : 0.9540          
             Prevalence : 0.1818          
         Detection Rate : 0.1483          
   Detection Prevalence : 0.2727          
      Balanced Accuracy : 0.8319          
                                          
       'Positive' Class : TRUE     
                             manual identification
       prediction.naiveBayes TRUE FALSE
                TRUE        701   647
                FALSE        241  3906
                                         
               Accuracy : 0.8384         
                 95% CI : (0.8284, 0.848)
    No Information Rate : 0.8286         
    P-Value [Acc > NIR] : 0.02707        
                                         
                  Kappa : 0.5142         
 Mcnemar's Test P-Value : < 2e-16        
                                         
            Sensitivity : 0.7442         
            Specificity : 0.8579         
         Pos Pred Value : 0.5200         
         Neg Pred Value : 0.9419         
             Prevalence : 0.1714         
         Detection Rate : 0.1276         
   Detection Prevalence : 0.2453         
      Balanced Accuracy : 0.8010         
                                         
       'Positive' Class : TRUE    


       prediction.knn TRUE FALSE
     TRUE   456   331
     FALSE  486  4222
                                          
               Accuracy : 0.8513          
                 95% CI : (0.8416, 0.8606)
    No Information Rate : 0.8286          
    P-Value [Acc > NIR] : 2.877e-06       
                                          
                  Kappa : 0.4401          
 Mcnemar's Test P-Value : 7.133e-08       
                                          
            Sensitivity : 0.48408         
            Specificity : 0.92730         
         Pos Pred Value : 0.57942         
         Neg Pred Value : 0.89677         
             Prevalence : 0.17143         
         Detection Rate : 0.08298         
   Detection Prevalence : 0.14322         
      Balanced Accuracy : 0.70569         
                                          
       'Positive' Class : TRUE

       knn with k = 3
       prediction TRUE FALSE
     TRUE   397   188
     FALSE  545  4365
                                          
               Accuracy : 0.8666          
                 95% CI : (0.8573, 0.8755)
    No Information Rate : 0.8286          
    P-Value [Acc > NIR] : 6.664e-15       
                                          
                  Kappa : 0.4474          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.42144         
            Specificity : 0.95871         
         Pos Pred Value : 0.67863         
         Neg Pred Value : 0.88900         
             Prevalence : 0.17143         
         Detection Rate : 0.07225         
   Detection Prevalence : 0.10646         
      Balanced Accuracy : 0.69008         
                                          
       'Positive' Class : TRUE 

       svm
       prediction TRUE FALSE
     TRUE   558   152
     FALSE  400  4384
                                          
               Accuracy : 0.8995          
                 95% CI : (0.8913, 0.9074)
    No Information Rate : 0.8256          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.6114          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.5825          
            Specificity : 0.9665          
         Pos Pred Value : 0.7859          
         Neg Pred Value : 0.9164          
             Prevalence : 0.1744          
         Detection Rate : 0.1016          
   Detection Prevalence : 0.1292          
      Balanced Accuracy : 0.7745          
                                          
       'Positive' Class : TRUE 