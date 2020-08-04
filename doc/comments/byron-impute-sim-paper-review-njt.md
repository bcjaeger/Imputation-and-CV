Review of Byron's paper

Note: I have marked up the paper on my iPad, but I've written corresponding longer form notes here.

<< BYRON's COMMENTS >>

# Introduction

I feel that the main idea of the paper starts in the third paragraph, I wonder if it might be possible to instead start there? The first two paragraphs provide great introduction to cross validation, but it at first sets me up to think the paper is about cross validation, rather than missing data? I guess in some ways it is about cross validation as well, so perhaps that is warranted. 

<< this is a good point. I have trouble going straight to the punch line b/c I feel it is important to define CV and missing data imputation before talking about doing imputation before CV. I would like to proceed with the original paragraph order for now but we can talk more about it if you'd like. >>

It seems to me that the large benefit of this new approach is that you essentially get the same results, but for a fraction of the computational time. Do you think the paragraph before the aims are outlined could outline this potential benefit, to fortify the motivation for the paper?

<<Yes - here is what I have now: A more computationally efficient approach would be to implement `unsupervised imputation' (\ie imputing MD without accessing outcome information) \emph{before} conducting CV. Ordering operations this way could result in substantially faster training and tuning costs for pipeline modeling algorithms.>>

I am not clear on what the overall goal/aim/objectives are of the paper. Can I suggest something like the following:

> The overall aim of this paper is to understand whether **unsupervised imputation** before, or after CV, impacts model pipeline error. The two conditions are defined as follows: **unsupervised imputation** to the training data **before CV** as "I → CV"; **unsupervised imputation** being applied during each replicate of CV as "CV ↻ I".
> To achieve this aim, we conduct XX number of experiments using simulated data, and XX experiments using real data.

<< Wonderful. I incorporated this in the second to last paragraph of the intro. >>

# Section 2 - missing data

I'm not sure if your definition of crowding is MCAR, I would consider this MAR, since this is something observed that could be related to missing data - for example here it could be more missing data for covid tests when there is more community transmission (since there is high community transmission there are more people getting tests). Perhaps instead suggest random clerical data entry error?

<< Good catch - I updated the MCAR example >>

I've suggested defining Type I error, but that probably isn't needed, just a thought, since we might get machine learning folks reading this paper and my understanding is they don't know what that means, and type I error is only described once in the paper, so perhaps we say "false positive" or "falsely rejecting a true null hypothesis".

<< Not hard to include this at all, it's been added in parentheses >>

I've also suggested to cite Max Kuhn's book - https://bookdown.org/max/FES/handling-missing-data.html. I thought this book might also come in handy when explaining the idea of the "analysis" and "assessment" set from Max Kuhn...but I can't seem to find his explanation anywhere? Hmmmm. 

<< A great book - it is now cited to back up the statement that most applied ML analyses use single imputation methods >>

# 3: Order of operations

I don't think "analysis and assessment set" have been defined yet.

<< Ah we wouldn't want that - here is what I have now: We refer to the $k-1$ folds and $k$ remaining fold used to internally train and test a modeling algorithm as **analysis** and **assessment** sets, respectively, to avoid abuse of notation. >>

# 4: Simulated experiments

- Is the term, "oracle" defined, and the name explained? 

<< It was half-heartedly so. Here is what I have now: The 'oracle' performance is the performance of an 'oracle' model whose specification coincides with the specification that generated the data that the model is fitted to. >>

- Can we include a table head of what the data looks like?

<< I like this idea. Can we make this part of the ReadMe on github? >>

- Can we define what junk variables are, and why they are important to include?

<< Yes, good idea. Here is what I have now: In practice, $\textbf{X}$ often has some 'junk' variables that are not related to the outcome. To make our simulations similar to applied settings, we generated normally distributed variables that had no relation to the simulated outcome .>>

- I'm not quite sure how incomplete replicates happen? Although I really appreciate that you describe honestly that not all analysis was completed.

<< It is a rare hiccup related to the ampute() function in mice. If the ampute() function can't determine a set of weights for MAR amputation, it fails and the simulation cannot proceed. The error only comes up with small sample sizes and when MAR amputation is needed. I appreciate that you appreciate this! =] >>

- Can we get the data on the computational time taken to complete each replicate? It would be great to have a figure that says like, "Look how much faster this method is!" And perhaps we could even do a plot of some external model performance measure against time taken to complete, so we can help demonstrate the advantage of this approach? 

<< Absolutely. I added an in-line summary of time used to create imputed data and also added a figure to the ames analysis (see Figure 5). >>

# 5: Real data experiments

- I had a thought about using equivalence testing, I wonder if it might be useful to state that the methods are same, rather than saying that they are not significantly different?

<< Yes, that would be a more interesting hypothesis to test. I am not sure how to set up the equivalence test based on the 5,000 simulation replicates. Is it just as simple as making an empirical confidence interval and showing that both upper and lower bounds fall within a pre-defined band of equivalence? Let's discuss. >>

# Discussion

- There is an enormous time saving with this approach! I wonder if we can describe how it is better by perhaps, for example, discussing the number of computer time / computer years saved.

<< I agree with this, it needs to be highlighted that this can save lots of time. I added a sentence to the discussion: 'Throughout our analysis, \icv\space required less computation time than \cvi\space by a factor of roughly $v$, the number of folds employed by CV.' >>

# General note on figures / table captions:

- Di Cook taught me that when unpacking a figure it should have three parts: "Overview", "describe the graphic", "what you learn".
- Although a lot of the figures here are themselves infographics, so they might defy this process somewhat. For me, I'm not quite sure what to take away from figure 1 - is it that there is a process where incomplete training data is _either_ cross validated, or pre-processed, according to which condition it is a part of? If that's the case, when perhaps we could signify that one pathway is ICV, and the other is CVI?

<< I really like the three stage approach to figure interpretation. For Figure 1, the 'overview' should be 'this is the standard workflow to develop and validate a pipeline modeling algorithm'. The description is hopefully clear from the caption and annotations in the graph. As far as 'what you learn', I think the reader may already be familiar with the workflow but they will understand that this workflow is what we will be studying in the current analysis. Would you like to discuss this more when we talk? We may be able to tweak figure 1 so that it is more aligned with what I wrote here. >> 

- Table 1
    - what number is in the parentheses? 
    << good catch, i added text to define this in table 1 and 2 captions >>
    - What is the "NA" value doing in "overall"
    << it is 'NA' for the value of N since it is overall. >>
    - I can't help but think there might be a way to show this table as a plot, but I'm not sure if it is too much perhaps. I wonder if we can highlight/shade in the parts of the table we want people to pay particular attention to?
    << I hate to trash talk my own table but Table 1 is really just there for completeness. It shows that we didn't get any wonky R2 values and hopefully gives reviewers the sense that we implemented our simulation study well. The first sentence of the results sort of builds up to the crescendo of presenting Figure 1 at the end. I admit the beginning is boring, but I am not sure how to fix it. It seems moving Figure 4 to the beginning of the paragraph would be hard to do b/c it is a lot of information to show without giving context that Tables 1 and 2 (hopefully) provide. >> 
    
    
- Figure 4
    - I really love this figure, is there a way that we can make this the "hero image" of the paper? I also wonder what it might look like if we include some faint grid lines?

<< I love the figure too and have included grid lines. Some journals allow you to specify a 'central' illustration, and we will definitely go with Figure 4 if that is something SIM wants. 

# General

- There are some citations missing for packages like `mice`, and `randomForest` in the paper, I've marked these.
- Other minor text changes in text marked.