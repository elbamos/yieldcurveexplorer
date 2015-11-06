# Yield Curve Explorer
Amos Elb  
November 6, 2015  


This was a project to create a visualization that would help explain the relationship between Fed target interest rate setting, the yield curve, and asset prices. 

The basic functionality is to animate how the yield curve has behaved as short-term interest rate targets were adjusted.  In particular, it illustrates nicely the effect of target rates on pulling the front of the yield curve down, while having a less discernible effect on longer-term rates.

I expanded the visualization to show the effect on GDP growth, and then to give the user a choice whether to take inflation into account.

I then expanded it further to also show effects on the S&P500 and CS housing price index. 

The app will update its data the first time each day that its run, checking if new data is available from FRED or Yahoo Finance. 

The visualization is not bug-free.  In particular, if the user asks to see the S&P or HPI, the lines will not hide if the user later asks to hide them. I did not explore whether it is possible to work around this limitation in the way ggvis handles animation.

The running app is [here](https://amose.shinyapps.io/yieldcurve).
