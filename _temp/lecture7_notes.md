



## Introduction: Motivating Example

Consider the following cross-cultural question: 

> Does bilingualism improve cognitive abilities in children? 

There is evidence that bilingual children perform better oat cognitive tasks, but is learning more than one language a onfounding factors? 

In this lecture, we will discuss the fundamental problem of causal inference, its assumptions, and how we can address this issue in both experimental and observational settings.



## Part 1: The Fundamental Problem of Causal Inference as a Missing Data Problem 

To understand the fundamental problem of causal inference, let's first define two potential outcomes for each individual in our study:

- $Y_i^{a = 1}$: The cognitive ability of child $i$ if they are bilingual.  This is the counterfactual outcome when A = 1.
- $Y_i^{a = 0}$:: The cognitive ability of child $i$ if they are monolingual. This is the counterfactual outcome when A = 0.

The causal effect of bilingualism on cognitive ability for individual $i$ is then defined as the difference between these potential outcomes:

$$
\text{Causal Effect}_i = Y^{a=1} - Y^{a=0} 
$$

Note that sometimes we will write this contrast as:


$$
\text{Causal Effect}_i = Y_i(a = 1) - Y_i(a = 0)
$$


or simply: 


$$
\text{Causal Effect}_i = Y(1) - Y(0)
$$

or so simply: 

$$
\text{Causal Effect}_i = Y^{1} - Y^{0} 
$$



We say there is a causal effect if: 

$$
Y^{a=1} - Y^{a=0}  \neq 0
$$




The Philosopher David Hume defines causation in the following way: 

> “We may define a cause to be an object followed by another, and where all the objects, similar to the first, are followed by objects similar to the second [definition 1]. Or, in other words, where, if the first object had not been, the second never would have existed [definition 2].” 
- Enquiries Concerning Human Understanding, and Concerning the Principles of Morals


This is a contrast between two states of the world. One in which a child recieves bi-lingual exposure and one in which a child does not.


However, consider that we can only observe one of the potential outcomes for each child.  The contrast we require for identifying a causal effect is typically not observed.


Robert Frost writes, 

> Two roads diverged in a yellow wood,
And sorry I could not travel both
And be one traveler, long I stood
And looked down one as far as I could
To where it bent in the undergrowth;

>Then took the other, as just as fair,
And having perhaps the better claim,
Because it was grassy and wanted wear;
Though as for that the passing there
Had worn them really about the same,

>And both that morning equally lay
In leaves no step had trodden black.
Oh, I kept the first for another day!
Yet knowing how way leads on to way,
I doubted if I should ever come back.

> I shall be telling this with a sigh
Somewhere ages and ages hence:
Two roads diverged in a wood, and I—
I took the one less traveled by,
And that has made all the difference.



“And sorry I could not travel both. And be one traveller$\dots$”  The fact that causal contrasts are not observed on individuals is called "The fundamental problem of causal inference."


We will discuss how this missing data problem arises in different research designs and explore strategies for addressing it.  

For now, note that the problem isn't merely one of statistical analysis on the data. **The problem is that the relevant data to identify individual causal effects are missing.**



## Part 2: Fundamental Assumptions of Causal Inference 

Although we typically cannot observe individual causal effects, under certain assumptions we can obtain average causal effects.


### Causal Consistency.

When the counterfactual consistency theorem holds, and individuals observed outcome under an exposure is equal to their counterfactual outcome under that exposure: 



\[
Y_i = Y_i(1) \mathtext{if} A_i = 1
\]
and 

\[
Y_i = Y_i(0) \mathtext{if}  A_i = 0
\]




The fundamental problem of causal inference is that an individual cannot receive two different exposures at the same time. For a binary exposure, an observed outcome under an exposure can be can be expressed: 

\[Y^{observed} = AY^{a=1} + (1-A)Y^{a=0} \]

Table \ref{tab:consistency} expresses the relationship between observable outcomes and counterfactual outcomes as a contingency table. 

\begin{table}
\begin{center}
\begin{tabular}{ |l|l|l| } 
 \hline
 Group & $Y^{a = 1}$ & $Y^{a=0}$ \\ 
 \hline
 Exposure $A = 1$ & Observable Y & Counterfactual \\ 
 No Exposure  $A = 0$ & Counterfactual & Observable Y  \\ 
 \hline
\end{tabular}
\end{center}
\label{tab:consistency}
\end{table}

Although for individuals, causal effects are not identified, an average causal effect $\E(\delta)$: may be identified as the expectation of the mean differences between the two exposure groups.

\[

E(\delta) = E(Y^{a=1} - Y^{a=0})
            = E(Y^{a=1}) - E(Y^{a=0}) = ATE
\]





### 2. **Conditionally exchangeablity**

Given the observed covariates, the treatment assignment is independent of the potential outcomes. Mathematically, this can be expressed as.




$$Y^a\indep A \text{ for all }a$$


or with strata of confounding covarates:


$$Y^a\indep A \text{ for all }a|L$$


When conditional exchangability holds:


$$
\begin{aligned}
ATE = E[Y^{a=1}|L = l] - E[Y^{a=0}|L = l] \text{for any value l}
\end{aligned}
$$


> "We say that a set L of measured non-descendants of L is a sufficient set for confounding adjustment when conditioning on L blocks all backdoor paths–that is, the treated and the untreated are exchangeable within levels of L" (Hernan & Robins, What IF p. 86)



### Positivity

There is a non-zero probability of receiving each treatment level for all strata of the observed covariates. This means that every individual has some chance of being in either treatment or control group.


## Part 3: Causal Inference from Randomized Experiments

Randomized experiments can help us overcome the the fundamental problem of causal inference. By randomly assigning individuals to treatment (bilingual) or control (monolingual) groups, we create comparable groups that allow us to estimate causal effects. 

Random assignment ensures that all observed and unobserved confounders are balanced between the treatment and control groups on average, satisfying the conditionally exchangeable assumption.

The key benefits of randomized experiments are:

1. Unbiased estimation of average treatment effects.
2. Control of both observed and unobserved confounding.
3. Facilitated interpretation of causal effects.



Notice that the results of an experiment recover Average Treatment Effects, not individual causal effects.  Although a treament might be beneficial for the majority of people, it might not be benefitial for you.  

This consideration may be extended to subgroups.  Although a treatment might be beneficial, on average, for the majority of a population, it might not be benefitial for a sub-population.  However, unlike individual causal effects, we can obtain causal effect estimates within sub-populations.  We will will return to this topic next week.  



## Part 4: Challenges in Identifying Causal Effects from Observational Data

In observational studies, we cannot randomly assign individuals to treatment and control groups. As a result, there may be confounding factors that affect both the treatment assignment and the outcome, making it difficult to identify causal effects.

In this context, some strategies to address confounding include:

1. **Matching**: Pair individuals with similar observed covariates in treatment and control groups. 

2. **Propensity score methods** Use the propensity score (the probability of treatment given covariates) to balance treatment and control groups on observed covariates.

3. **Regression adjustment** Include potential confounders as control variables in a regression model. 

