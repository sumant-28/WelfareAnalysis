# WelfareAnalysis

One of the benefits of the McFadden RUM model is that it is theoretically possible and analytically straightforward to infer welfare effects from changes in the market. This can be useful to get an answer to a question on which directly applicable data is not available. A welfare effect in economic terms is a change in consumer surplus and purports to measure in dollar amounts how much a consumer is effected by a change in market conditions. 

There are not many publically available choice data sets on which to estimate a RUM model where income is measured. Something that can be done with an income variable is simulate an increase in income to simulate how much better off a consumer is. A desirable model will have a welfare effect close to the monetary change in income. For this estimation this was not the case; a $1000 increase in income created a positive welfare effect of between ~$2 and ~$11. There can be many explanations for this:

* the model specification, 
* the accuracy of the income/price coefficients 
* the economic assumption that the coefficient on the cost variable is a good estimate of the marginal utility of income

The estimated willingness to pay is defined using marginal effects in the scale the variable was recorded in. It is the coefficient on any variable in the utility specification divided by the coefficient on the price variable. It is equivalent to the change in consumer surplus from a one unit improvement in any variable that the representative consumer values positively in their utility function.

## Bibliography

McFadden, D. 1974. *“Conditional Logit Analysis of Qualitative Choice Behaviour”*. Frontiers of Econometrics.

Small, K. and H. Rosen 1981. "Applied Welfare Economics with Discrete Choice Models". *Econometrica*. 105 -- 130. 
