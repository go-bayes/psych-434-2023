---
title: "Causal Inference: reconsidering measurement and selection biases"
date: "2023-MAY-16"
bibliography: references.bib
---

```{r}
#| include: false
#| echo: false
#read libraries

library("tinytex")
library(extrafont)
loadfonts(device = "all")

# read libraries
source("/Users/joseph/GIT/templates/functions/libs2.R")

# read functions
#source("/Users/joseph/GIT/templates/functions/funs.R")
```

## Overview

Recall that psychology begins with a question about cognition and behaviour. What do we want to know? Before all else, we must ask, and motivate this question.

Suppose we have defined a question. How can we address it using observational data?

  This is the topic of todays seminar.

## How do we do causal estimation?

There are two steps to causal estimation:

1.  Ask a causal question
2.  Answer that question [@hernán2022]

## Step 1: State a causal question

Stating a causal question requires describing: a. outcome(s), b. exposure, c. measured confounders, d. (suspected) unmeasured confounders, e. scale of causal contrasts, f. target population for whom the inferences apply.

We consider each of these processes in turn.

### a. Identify the outcome(s) of interest

We use *Y* to denote an outcome of interest. This is the "effect" of interest.

#### Consider:

-   The outcome might be binary (severely distressed/not severely distressed), continuous (the average of the sum of the indicators), or a rate variable (the sum of the indicators).
-   It is crucial to specify the units in which the outcome is measured.
-   Transforming the outcome into standard deviation units can be beneficial.
-   The outcome must occur after the exposure or treatment.
-   We must designate a time period within which the outcome occurs, e.g, "the one-year effect of a treatment on well-being as measured by Kessler-6."
-   We may be interested in multiple outcomes. This is the rational behind outcome-wide science \[cite tyler\]
-   We must remember that the outcome might be measured with error, that such errors may be affected by the treatment or correlated with the measurement error of the treatment. (A topic of future seminars, which we will set to the side for now)

Here, imagine we are interested in understanding only one outcome in our study: well-being as measured by the Kessler-6 depression/anxiety scale.

### b. Define the exposure or intervention

We use *A* to denote the the exposure or treatment. This is the variable that we hypothesise might affect the outcome. This variable denotes the cause of interest. We will restrict our focus to consider in which there is only one treatment (i.e. we will not consider complex multi-treatment regimes.)

Here, imagine we are interested in understanding the causal effect of 'Church attendance'.

#### Consider:

-   To affect *Y*, *A* must occur before *Y*.
-   What does *A* indicate? We may characterise 'Church Attendance' as a binary exposure (attend/not attend) or as a continuous exposure (attend weekly/attend monthly). The exposure, in this instance, remains nebulous until we delineate the unit change in *A* we are interested in investigating.
-   For theoretical and practical purpose we might want to truncate the variable into categories: (none/some; none/less than weekly, weekly or more)... Why? Because our causal question requires stating the contrast between the states of the world in which we are interested. Additionally, experts often make decisions on the basis of discrete thresholds (has risk of depression/does not).
-   We must remember that an exposure or treatment might be measured with error, that such errors might be correlated with the measurement error of the outcome or affect it. (Again, a topic of future seminars, which we will set to the side for now).

### c. Identify pre-exposure covariates for confounding control

We use **L** to denote the set of measured baseline confounders of the the exposure-outcome association. For a three-wave panel design, we employ VanderWeeles modified disjunctive cause criterion, we advises: control for any covariate that is a cause of the exposure, the outcome, or both, excluding any instrumental variable and incorporating any proxy for an unmeasured variable that is a common cause of both the exposure and the outcome.

#### Consider:

-   To affect *Y*, *L* must occur before *Y*.\
-   To affect *A*, *L* must occur before *A*.
-   Although we may gain precision by including an *L* that affects *Y* but is unrelated to *A*, including variables that occur after *A* will be hazardous if it is possible that *A* affects *L* (or its measurement).
-   A useful set of default counfounders in NZAVS studies is given in your workbooks.
-   Note we have left out the concept of "selection bias." There are indeed sources of bias that may occure after *A*. This is another topic for the week ahead.

### d. Highlight unmeasured pre-treatment covariates

Let **U** denoted unmeasured pre-treatment covariates that may potentially bias the statistical association between *A* and *Y* independently of the measured covariates.

#### Consider:

-   To affect *Y* and *A*, *U* must occur before *A*.
-   It is useful to draw a causal diagramme to illustrate all potential sources of bias.
-   Causal diagrammes are qualitative tools that require specialist expertise. We cannot typically obtain a causal graph from the data.
-   A causal diagramme should include only as much information as is required to assess confounding. See @fig-dag-outcomewide for an example.
-   Because we cannot ensure the absence of unmeasured confounders in observational settings, it is vital to conduct sensitivity analyses for the results. For sensitivity analyeses, we use E-values, a topic for a latter seminar.

```{tikz}
#| label: fig-dag-outcomewide
#| fig-cap: "Causal graph: three-wave panel design. We use the prefixes "t0, t1, and t2" to denote temporal ordering. We include in the set of baseline confounders the pre-exposure measurement of *A* and *Y*. This allows for more substantial confounding control. For unmeasured confounder to affect both the exposure and the outcome, it would need to do so independently of the pre-exposure confounders. Additionally, including the baseline exposure gives us an effect estimate for the incidence exposure, rather than the prevelance of the exposure. This helps us to assess the expected change in the outcome were we to initate a change in the exposure.
#| out-width: 100%
#| echo: false

\usetikzlibrary{positioning}
\usetikzlibrary{shapes.geometric}
\usetikzlibrary{arrows}
\usetikzlibrary{decorations}
\tikzstyle{Arrow} = [->, thin, preaction = {decorate}]
\tikzset{>=latex}

\begin{tikzpicture}[{every node/.append style}=draw]
\node [rectangle, draw=white] (U) at (0, 0) {U};
\node [rectangle, draw=black, align=left] (L) at (2, 0) {t0/L \\t0/A \\t0/Y};
\node [rectangle, draw=white] (A) at (4, 0) {t1/A};
\node [ellipse, draw=white] (Y) at (6, 0) {t2/Y};
\draw [-latex, draw=black] (U) to (L);
\draw [-latex, draw=black] (L) to (A);
\draw [-latex, draw=red, dotted] (A) to (Y);
\draw [-latex, bend left=50, draw =black] (L) to (Y);
\draw [-latex, bend right=50, draw =black, dotted] (U) to (Y);
\draw [-latex, bend left=50, draw =black, dotted] (U) to (A);


\end{tikzpicture}
```

### e. Choose the scale for a causal contrast

Average causal effects can be inferred by contrasting the expected outcome when a population is exposed to an exposure level, $E[Y(A = a)]$, with the expected outcome under a different exposure level, $E[Y(A=a^{\prime})]$.

For a binary treatment with levels $A=0$ and $A=1$, the Average Treatment Effect (ATE), on the difference scale, is expressed:

$$ATE_{\text{risk difference}} = E[Y(1)|L] - E[Y(0)|L]$$

On the risk ratio scale, the ATE is expressed:

$$ATE_{\text{risk ratio}} = \frac{E[Y(1)|L]}{E[Y(0)|L]}$$

Other effect scales, such as the incidence rate ratio, incidence rate difference, or hazard ratio, might also be of interest. We can also define the Average Treatment Effect on the Treated (ATT) :

$$ATT_{\text{risk difference}} = E[Y(1) - Y(0)|A=1,L]$$

$$ATT_{\text{risk ratio}} = \frac{E[Y(1)|A=1,L]}{E[Y(0)|A=1, L]}$$

Another common estimand is the Population Average Treatment Effect (PATE), which denotes the effect the treatment would have on the entire population if applied universally to that population. This quantity can be expressed:

$$PATE_{\text{risk difference}} = f(E[Y(1) - Y(0)|L], W)$$

$$PATE_{\text{risk ratio}} = f\left(\frac{E[Y(1)|L]}{E[Y(0)|L]}, W\right)$$

where $f$ is a function that incorporates weights $W$ into the estimation of the expected outcomes. These weights may correspond to the inverse probability of being sampled or in the case of NZAVS data, the survey weights are given from census estimates for the wider population. Note: I will show you how to use weights in future seminars.

We might also be interested in identifying effects specific to certain strata, such as risk differences or risk ratios, as they are modified by baseline indicators. Denote a stratum of interest by $S$. We may then compute:

$$ATE_{S,\text{risk difference}} = E[Y(1) - Y(0)|S, L]$$

$$ATE_{S,\text{risk ratio}} = \frac{E[Y(1)|S, L]}{E[Y(0)|S, L]}$$

#### Consider:

-   \*\* In this course, we are interested in stratum specific comparisons \*\*
-   In the causal inference literature, the concept we use to make sense of stratum specific comparisons is called "effect modification."
-   By inferring effects within stratums, we may evaluate whether the effects of different exposures or treatments on some well-defined outcome (measured in some well-defined time-period after the exposure) differ depending on group measurement.
-   The logic of effect modification differs from that of intereaction.

#### Aside: extensions

For continuous exposures, we must stipulate the level of contrast for the exposure (e.g. weekly versus monthly church attendance):

$$ATE_{A,^{\prime}} = E[Y(A) - Y(^{\prime})| L]$$

This essentially denotes an average treatment effect comparing the outcome under treatment level $A$ to the outcome under treatment level $^{\prime}$.

Likewise:

  $$ATE_{A/^{\prime}} = \frac{E[Y(A)| L]}{E[Y(^{\prime})| L]}$$

  This defines the contrast of $A$ and $^{\prime}$ on a ratio scale.

#### f. Describe the population(s) for whom the intended study is meant to generalise.

The potential outcomes literature in causal inference distinguishes between the concepts of generalisability and transportability.

-   **Generalisability** refers to the ability to apply the causal effects estimated from a sample to the population it was drawn from. In simpler terms, it deals with the extrapolation of causal knowledge from a sample to the broader population. This concept is also called "external validity".

$$\text{Generalizability} = PATE \approx ATE_{\text{sample}}$$

-   **Transportability** refers to the ability to extrapolate causal effects learned from a source population to a target population when certain conditions are met. It deals with the transfer of causal knowledge across different settings or populations.

$$\text{Transportability} = ATE_{\text{target}} \approx f(ATE_{\text{source}}, T)$$

where $f$ is a function and $T$ is a function that maps the results from our source population to another population. To achieve transportability, we need information about the source and target populations and an understanding of how the relationships between treatment, outcome, and covariates differ between the populations. Assessing transportability requires scientific knowledge.

### Summary Step 1: Consider how much we need to do when asking a causal question!

We discover that asking a causal question is a multifaceted task. It demands careful definition of the outcome, including its timing, the exposure, and covariates. It also requires selecting the appropriate scale for causal contrast, controlling for confounding, and potentially adjusting for sample weights or stratification. Finally, when asking a causal question, we must consider for whom the results apply. Only after following these steps can we then ask: "How may we answer this causal question?"

## STEP 2: ANSWER A CAUSAL QUESTION

#### Obtain longitudinal data

Note that causal inference from observational data turns on the appropriate temporal ordering of the key variables involved in the study.

Recall we have defined.

-   **A**: Our exposure or treatment variable, denoted as **A**. Here we consider the example of 'Church attendance'.

-   **Y**: The outcome variable we are interested in, represented by **Y**, is psychological distress. We operationalise this variable through the 'Kessler-6' distress scale.

-   **L**: The confounding variables, collectively referred to as **L**, represent factors that can independently influence both **A** and **Y**. For example, socio-economic status could be a confounder that impacts both the likelihood of church attendance and the levels of psychological distress.

Given the importance of temporal ordering, we must now define time:

-   **t** $\in$ T: Let $t$ denote within a multiwave panel study with **T** measurement intervals.

Where $t/\text{{exposure}}$ denotes the measurement interval for the exposure. Longitudinal data collection provides us the ability to establish a causal model such that: $t\text{{baseline}}/\mathbf{L} < t\text{{exposure}}/\text{{A}} < t\text{{exposure}}/\mathbf{Y}$.

To minimise the posibility of time-varying confounding and obtain the clearest effect estimates, we should acquire the most recent values of $\mathbf{L}$ preceding $A$ and the latest values of $A$ before $Y$ such that: $t\text{{0}}/\mathbf{L} < t\text{{1}}/\text{{A}} < t\text{{2}}/\mathbf{Y}$.

### Include the measured outcome with baseline covariates

Controlling for previous outcome is crucial because the outcome at baseline is often the most potent confounder of both the exposure post-exposure-outcome association.

### Include the measured exposure with baseline covariates

Controlling for prior exposure enables the interpretation of the effect estimate as a change in the exposure in a manner akin to a randomised trial. We propose that the effect estimate with prior control for the exposure estimates the "incidence exposure" rather than the "prevalence exposure" \cite{Hernan Danaei, Tavakkoli and Hernán, 2012, Hernán, 2015}. It is crucial to estimate the incidence exposure because if the effects of an exposure are harmful in the short term such that these effects are not subsequently measured, a failure to adjust for prior exposure will yield the illusion that the exposure is beneficial. Furthermore, this approach aids in controlling for unmeasured confounding. For such a confounder to dismiss the observed exposure-outcome association, it would need to do so independently of the prior level of the exposure and outcome.

### State the eligibility criteria for participation

This step is invaluable for assessing whether we are answering the causal question that we have asked.

#### Consider:

-   Generalisability: we cannot evaluate inferences to a target group from the source popuation if we do not describe the source population
-   Eligibility criteria will help us to ensure whether we have correctly evaluated potential measurement bias/error in our instruments.

For example, the New Zealand Attitudes and Values Study is a National Probability study of New Zealanders. The details provided in the supplementary materials describe how individuals were randomly selected from the countrys electoral roll. From these invitations there was typically less than 15% response rate. How might this process of recruitment affect generalisability and transportability of our results?

  -   (Aside: discuss per protocol effects/ intention to treat effects)

### Handling of missing data

-   As we will consider in the upcoming weeks, loss to follow up and non-response opens sources for bias. We must develop a strategy for handling missing data.

### State a statistical model

The models we have considered in this course are G-computation, Inverse Probability of Treatement Weighting, and Doubly-Robust estimation.

#### **G-computation algorithm**

**Step 1** Estimate the outcome model. Fit a model for the outcome $Y$, conditional on both the exposure $A$ and the covariates $A$. This model can be linear regression, logistic regression, or another statistical model. The goal is to capture the relationship between the outcome, exposure, and confounders

$$ E(Y|A,L) = f_Y(A,L; \theta_Y) $$

  This equation represents the expected value of the outcome $Y$ given the exposure $A$ and covariates $A$ as modeled by the function $f_Y$ with parameters $\theta_Y$. This formulation allows for the prediction of the average outcome $Y$ given certain values of $A$ and $L$. The specific form of $f_Y$. For continuous outcomes, we define $f_Y$ as a linear regression model, for rare binary outcomes as a logistic regression model, and for common binary outcomes or rates outcomes as log-linear regression model (i.e.poisson model.)

**Step 2** Simulate potential outcomes. For each individual i in the population, predict their potential outcome (\hat{Y}\_i(a)) under the intervention A=a, and (\hat{Y}\_i(^{\prime})) under the intervention $A=^{\prime}$, using the estimated outcome model. This is done by replacing the observed value of the exposure A in the outcome model with the counterfactual exposure values $a$ and $a^{\prime}$, while keeping the values of the covariates W unchanged:

\[\hat{Y}\_i(a) = E\[Y\|A=a,A_i; \theta\_Y\]\]

\[\hat{Y}\_i(^{\prime}) = E\[Y\|A=^{\prime},A_i; \theta\_Y\]\]

This step involves simulating the outcomes under each intervention level for every individual, as if they were exposed to treatment level a or ^{\prime}. The simulated potential outcomes represent the expected values of the outcome variable Y under the specific intervention levels a and ^{\prime}, accounting for the confounding effects of the covariates W.

**Step 3** Estimate the average causal effect. Compute the expected value of the potential outcomes under each intervention level:

\[E\[Y(a)\] = \frac{1}{N}\sum\_{i=1}\^N \hat{Y}\_i(a)\]

\[E\[Y(^{\prime})\] = \frac{1}{N}\sum\_{i=1}\^N \hat{Y}\_i(^{\prime})\]

and calculate the difference:

\[\delta = E\[Y(a)\] - E\[Y(^{\prime})\]\]

To obtain the standard errors and confidence intervals several methods can we use simulation-based inference using the clarify package [@greifer2023]

#### **IPTW algorithm**

**Step 1** Estimate the outcome model. Fit a model for the outcome $Y$, conditional on both the exposure $A$ and the covariates $A$. This model can be linear regression, logistic regression, or another statistical model. The goal is to capture the relationship between the outcome, exposure, and confounders

$$ E(Y|A,L) = f_Y(A,L; \theta_Y) $$

  This equation represents the expected value of the outcome $Y$ given the exposure $A$ and covariates $A$ as modeled by the function $f_Y$ with parameters $\theta_Y$. This formulation allows for the prediction of the average outcome $Y$ given certain values of $A$ and $L$. The specific form of $f_Y$. For continuous outcomes, we define $f_Y$ as a linear regression model, for rare binary outcomes as a logistic regression model, and for common binary outcomes or rates outcomes as log-linear regression model (i.e.poisson model.)

**Step 2** Simulate potential outcomes. For each individual i in the population, predict their potential outcome (\hat{Y}\_i(a)) under the intervention A=a, and (\hat{Y}\_i(^{\prime})) under the intervention $A=^{\prime}$, using the estimated outcome model. This is done by replacing the observed value of the exposure A in the outcome model with the counterfactual exposure values a and ^{\prime}, while keeping the values of the covariates W unchanged:

\[\hat{Y}\_i(a) = E\[Y\|A=a,A_i; \theta\_Y\]\]

\[\hat{Y}\_i(^{\prime}) = E\[Y\|A=^{\prime},A_i; \theta\_Y\]\]

This step involves simulating the outcomes under each intervention level for every individual, as if they were exposed to treatment level a or ^{\prime}. The simulated potential outcomes represent the expected values of the outcome variable Y under the specific intervention levels a and ^{\prime}, accounting for the confounding effects of the covariates W.

**Step 3** Estimate the average causal effect. Compute the expected value of the potential outcomes under each intervention level:

\[E\[Y(a)\] = \frac{1}{N}\sum\_{i=1}\^N \hat{Y}\_i(a)\]

\[E\[Y(^{\prime})\] = \frac{1}{N}\sum\_{i=1}\^N \hat{Y}\_i(^{\prime})\]

and calculate the difference:

\[\delta = E\[Y(a)\] - E\[Y(^{\prime})\]\]

To obtain the standard errors and confidence intervals several methods can we use simulation-based inference using the clarify package[@greifer2023]

#### **Inverse Probability of Treatment Weighting (IPTW) algorithm**

**Step 1** Estimate the treatment model. Fit a model for the exposure $A$, conditional on the covariates $L$ and subgroup indicator $G$. For a binary exposure we use logistic regression model and a multinomial logistic model if the exposure has multiple categories. The goal is to capture the relationship between the exposure and the covariates, and to estimate the probability of receiving the treatment given the covariates and the subgroup.

$$ P(A|L) = f_A(L; \theta_A) $$

  This equation represents the probability of the exposure $A$ given the covariates $L$ as modeled by the function $f_A$ with parameters $\theta_A$. This formulation allows for the prediction of the probability of receiving the treatment given certain values of $L$.

**Step 2** Compute inverse probability weights. For each individual $i$ in the population, compute the inverse probability of treatment weight (IPTW) as the inverse of the estimated probability of receiving the treatment that the individual actually received:

  $$w_i = \frac{1}{\hat{P}(A_i|L_i)}$$

  where $\hat{P}(A_i|L_i)$ is the estimated probability of receiving the treatment that individual $i$ actually received, given their covariates.

**Step 3** Weight the outcome model. Fit a model for the outcome $Y$, conditional on the exposure $A$, weighted by the IPTWs. This model can be a linear regression model for continuous outcomes, a logistic regression model for binary outcomes, or another appropriate model for other types of outcomes.

$$ E(Y|A) = f_Y(A; \theta_Y, W) $$

  where $W$ are the weights computed in Step 2.

**Step 4** Estimate the average causal effect. Compute the weighted average of the outcomes under each level of the exposure:

  $$E[Y(a)] = \frac{1}{\sum w_i} \sum_{i=1}^N w_i \hat{Y}_i(a)$$

  where $\hat{Y}_i(a)$ is the predicted outcome for individual $i$ under exposure level $a$.

To compare two exposure levels $a$ and $^{\prime}$, calculate the difference:

$$\delta = E[Y(a)] - E[Y(^{\prime})]$$

  This difference represents the average causal effect of changing the exposure from level $^{\prime}$ to level $a$.

To obtain the standard errors and confidence intervals, we use simulation-based inference using the clarify package[@greifer2023]

### For Subroup Analysis

#### **Inverse Probability of Treatment Weighting (IPTW) for Subgroup Analysis Algorithm**

**Step 1** Estimate the treatment model. Fit a model for the exposure $A$, conditional on the covariates $L$ and subgroup indicator $G$. For a binary exposure we use logistic regression model and a multinomial logistic model if the exposure has multiple categories. The goal is to capture the relationship between the exposure and the covariates, and to estimate the probability of receiving the treatment given the covariates and the subgroup.

$$ P(A|L, G) = f_A(L, G; \theta_A) $$

This equation represents the probability of the exposure $A$ given the covariates $L$ and subgroup $G$ as modeled by the function $f_A$ with parameters $\theta_A$. This formulation allows for the prediction of the probability of receiving the treatment given certain values of $L$ and $G$.

**Step 2** Compute inverse probability weights. For each individual $i$ in the population, compute the inverse probability of treatment weight (IPTW) as the inverse of the estimated probability of receiving the treatment that the individual actually received:

$$w_i = \frac{1}{\hat{P}(A_i|L_i, G_i)}$$

where $\hat{P}(A_i|L_i, G_i)$ is the estimated probability of receiving the treatment that individual $i$ actually received, given their covariates and subgroup.

**Step 3** Weight the outcome model. Fit a model for the outcome $Y$, conditional on the exposure $A$ and the subgroup $G$, weighted by the IPTWs. This model can be a linear regression model for continuous outcomes, a logistic regression model for binary outcomes, or another appropriate model for other types of outcomes.

$$ E(Y|A, G) = f_Y(A, G; \theta_Y, W) $$

where $W$ are the weights computed in Step 2.

**Step 4** Estimate the average causal effect for each subgroup. For each subgroup $g$, compute the weighted average of the outcomes under each level of the exposure:

$$E[Y(a)|G=g] = \frac{1}{\sum_{G_i=g} w_i} \sum_{G_i=g} w_i \hat{Y}_i(a)$$

where $\hat{Y}_i(a)$ is the predicted outcome for individual $i$ under exposure level $a$ and subgroup $g$.

To compare two exposure levels $a$ and $^{\prime}$, calculate the difference:

  $$\delta_g = E[Y(a)|G=g] - E[Y(^{\prime})|G=g]$$

This difference represents the average causal effect of changing the exposure from level $^{\prime}$ to level $a$ within subgroup $g$.

To obtain the standard errors and confidence intervals for the subgroup-specific causal effects, we use simulation-based inference using the clarify package[@greifer2023].

### What we have not done

We have not addressed: - measurement error. Our graphs do not incorporate it. - selection bias - Outcome-wide studies - Sampling weights.
