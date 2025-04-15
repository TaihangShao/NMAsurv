### Tips

This APP provided three methods to test the PH assumption. They are ***Schoenfeld residual plot***, ***Log-Log plot*** and ***Grambsch-Therneau test***. Here is the summarized introduction for these three methods:

***Schoenfeld residual plot***

The Schoenfeld residual test primarily examines the proportional hazards assumption by analyzing the changes in Schoenfeld residuals over time. The basic idea is that if the proportional hazards assumption holds, the Schoenfeld residuals should be randomly distributed over time without showing any systematic trend. In this APP, user can get the P value through the Schoenfeld residual plot. *Typically, if P value is smaller than 0.05, PH assumption is considered not held*.

***Grambsch-Therneau test***

The Grambsch-Therneau test is a more formal statistical test based on Schoenfeld residuals. It evaluates the proportional hazards assumption by examining the correlation between Schoenfeld residuals and the ranks of time. In this APP, user can get the P value through the Grambsch-Therneau test. *Typically, if P value is smaller than 0.05, PH assumption is considered not held*.

***Log-Log plot***

If the proportional hazards (PH) assumption holds, the log-cumulative hazard curves for different groups should be parallel. *Through visual inspection, if two lines are not parallel, PH assumption is considered not held*.

Usually, PH assumption uncertain in \> 1 trials in the network, users should consider NMA that do not rely on PH assumption; No evidence of a violation of the PH assumption in any of the trials in the network, users can consider PH models.
