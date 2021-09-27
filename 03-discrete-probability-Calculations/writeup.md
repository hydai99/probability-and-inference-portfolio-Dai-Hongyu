How often does the better team win the World Series?
================
Hongyu Dai
2021/9/25

The world series is a best-of-7 match-up between the champions of the
American and National Leagues of Major League Baseball.

In this blog post, I am going to answer a series of probability
calculations questions related to the World Series.

# Assumption

1.  The Braves and the Yankees are teams competing in the World Series.

2.  In any given game, the probability that the Braves win is
    *P*<sub>*B*</sub> and the probability that the Yankees win is
    *P*<sub>*Y*</sub> = 1 − *P*<sub>*B*</sub>

3.  If no special explanation, the game rules are 7 games 4 wins

4.  To avoid ties, the number of games can only be an odd number

5.  The winning team must win the last game

6.  The results of each game are independent of each other

# Questions and Answers

``` r
# Import packages
library(tidyverse)
library(ggplot2)
library(dplyr)
```

## What is the probability that the Braves win the World Series given that *P*<sub>*B*</sub> = 0.55?

Because the game rules are 7 games 4 wins, if Braves win the World
Series, they should win the last game and also need to gain 3 wins in
previous games. This means that Braves needs to win 4 games and lose at
most 3 games:

``` r
PB=0.55
pnbinom(3, 4, PB) 
```

    ## [1] 0.6082878

So, the probability that the Braves win the World Series given that
*P*<sub>*B*</sub> = 0.55 is 0.6082878.

## What is the probability that the Braves win the World Series given that *P*<sub>*B*</sub> = *x*? This will be a figure (see below) with *P*<sub>*B*</sub> on the x-axis and *P*(Braves win World Series) on the y-axis.

This question is similar to the ﬁrst question in that we just need to
vary the probability of PB and we can obtain the result, as shown in the
diagram:

``` r
x <- seq(0.4,1,by = 0.01)
y <- pnbinom(3,4,x)
p_win2<- data.frame(x,y)
p_win2
```

    ##       x         y
    ## 1  0.40 0.2897920
    ## 2  0.41 0.3093807
    ## 3  0.42 0.3294116
    ## 4  0.43 0.3498411
    ## 5  0.44 0.3706237
    ## 6  0.45 0.3917122
    ## 7  0.46 0.4130579
    ## 8  0.47 0.4346107
    ## 9  0.48 0.4563199
    ## 10 0.49 0.4781337
    ## 11 0.50 0.5000000
    ## 12 0.51 0.5218663
    ## 13 0.52 0.5436801
    ## 14 0.53 0.5653893
    ## 15 0.54 0.5869421
    ## 16 0.55 0.6082878
    ## 17 0.56 0.6293763
    ## 18 0.57 0.6501589
    ## 19 0.58 0.6705884
    ## 20 0.59 0.6906193
    ## 21 0.60 0.7102080
    ## 22 0.61 0.7293131
    ## 23 0.62 0.7478954
    ## 24 0.63 0.7659184
    ## 25 0.64 0.7833483
    ## 26 0.65 0.8001543
    ## 27 0.66 0.8163083
    ## 28 0.67 0.8317859
    ## 29 0.68 0.8465656
    ## 30 0.69 0.8606298
    ## 31 0.70 0.8739640
    ## 32 0.71 0.8865576
    ## 33 0.72 0.8984038
    ## 34 0.73 0.9094991
    ## 35 0.74 0.9198442
    ## 36 0.75 0.9294434
    ## 37 0.76 0.9383045
    ## 38 0.77 0.9464394
    ## 39 0.78 0.9538632
    ## 40 0.79 0.9605947
    ## 41 0.80 0.9666560
    ## 42 0.81 0.9720724
    ## 43 0.82 0.9768724
    ## 44 0.83 0.9810869
    ## 45 0.84 0.9847497
    ## 46 0.85 0.9878968
    ## 47 0.86 0.9905661
    ## 48 0.87 0.9927972
    ## 49 0.88 0.9946307
    ## 50 0.89 0.9961084
    ## 51 0.90 0.9972720
    ## 52 0.91 0.9981634
    ## 53 0.92 0.9988237
    ## 54 0.93 0.9992928
    ## 55 0.94 0.9996085
    ## 56 0.95 0.9998064
    ## 57 0.96 0.9999187
    ## 58 0.97 0.9999736
    ## 59 0.98 0.9999947
    ## 60 0.99 0.9999997
    ## 61 1.00 1.0000000

``` r
ggplot(p_win2,aes(x,y))+  
  geom_point()+
  labs(title = "Probability of Winning the World Series",
       y = "Pr(Win World Series)",
       x = "Probability of the Braves winning a head-to-head matchup") +
  theme_bw()+
  theme(panel.grid.major=element_blank())
```

![](writeup_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Suppose one could change the World Series to be best-of-9 or some other best-of-n series. What is the shortest series length so that *P*(Braves win World Series\|*P*<sub>*B*</sub> = 0.55) ≥ 0.8.

For best-of-X series, if Braves want to win the World Series, they
should win more than $\\frac{n}{2}$ games, which means they can not lose
more than $\\frac{n}{2}-1$ games.

After calculating the probability of winning, we combined the number of
games and the corresponding probability of winning for the Braves into a
data frame. Then we can get the result:

``` r
n <- seq(1,100,2)
p <- pnbinom(ceiling(n/2)-1,ceiling(n/2),0.55)
df3 <- data.frame(game_number=n, prob=p)
min ((df3 %>% filter(prob>=0.8))$game_number)
```

    ## [1] 71

The shortest series length so that *P*(Braves win World
Series\|*P*<sub>*B*</sub> = .55) ≥ 0.8 is 71.

## 4. What is the shortest series length so that *P*(Braves win World Series\|*P*<sub>*B*</sub> = *x*) ≥ 0.8?

Similar to the third question, we only need to change *P*<sub>*B*</sub>,
then we can get the shortest series length corresponding to different
*P*<sub>*B*</sub>, as shown in the diagram

Note: To avoid ties, the number of matches should be odd.

``` r
b <- vector()
x <- seq(0.5,1,by = 0.01)

for (j in x){
  n <- seq(1,3000,2)
  p <- pnbinom(ceiling(n/2)-1,ceiling(n/2),j)
  df4 <- data.frame(game_number=n, prob_win=p)
  end=min ((df4 %>% filter(prob_win>=0.8))$game_number)
  b[which(near(x,j))]<- ifelse( end!=Inf,end,NA )
}
df5 <- data.frame(shortest_series=b,PB=x)
df5
```

    ##    shortest_series   PB
    ## 1               NA 0.50
    ## 2             1771 0.51
    ## 3              443 0.52
    ## 4              197 0.53
    ## 5              111 0.54
    ## 6               71 0.55
    ## 7               49 0.56
    ## 8               37 0.57
    ## 9               27 0.58
    ## 10              21 0.59
    ## 11              17 0.60
    ## 12              15 0.61
    ## 13              13 0.62
    ## 14              11 0.63
    ## 15               9 0.64
    ## 16               7 0.65
    ## 17               7 0.66
    ## 18               7 0.67
    ## 19               5 0.68
    ## 20               5 0.69
    ## 21               5 0.70
    ## 22               5 0.71
    ## 23               3 0.72
    ## 24               3 0.73
    ## 25               3 0.74
    ## 26               3 0.75
    ## 27               3 0.76
    ## 28               3 0.77
    ## 29               3 0.78
    ## 30               3 0.79
    ## 31               1 0.80
    ## 32               1 0.81
    ## 33               1 0.82
    ## 34               1 0.83
    ## 35               1 0.84
    ## 36               1 0.85
    ## 37               1 0.86
    ## 38               1 0.87
    ## 39               1 0.88
    ## 40               1 0.89
    ## 41               1 0.90
    ## 42               1 0.91
    ## 43               1 0.92
    ## 44               1 0.93
    ## 45               1 0.94
    ## 46               1 0.95
    ## 47               1 0.96
    ## 48               1 0.97
    ## 49               1 0.98
    ## 50               1 0.99
    ## 51               1 1.00

``` r
ggplot(df5)+
  geom_point(aes(x=PB,y=shortest_series))+
  labs(title = "Shortest series so that P(Win WS given p) >= 0.8",
       y = "Series length",
       x = "Probability of the Braves winning a head-to-head matchup"
       ) +
  theme_bw()+
  theme(panel.grid.major=element_blank())
```

![](writeup_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 5. Calculate *P*(*P*<sub>*B*</sub> = 0.55\|Braves win World Series in 7 games) under the assumption that either *P*<sub>*B*</sub> = 0.55 or *P*<sub>*B*</sub> = 0.45. Explain your solution.

According to `Bayes Rule`, we know:

$$P(A\|B) = \\frac{P(B\|A)P(A)}{P(B)}$$
For this question, we can assume that P( A ) = P( P<sub>B</sub>=0.55 ),
P( B ) = P( Braves win World Series in 7 games ).

Take them into the `Bayes Rule`, then we can get:

$$P(P\_B=0.55\|Braves\\ win\\ World\\ Series\\ in\\ 7 games)\\\\
= \\frac{P(Braves\\ win\\ World\\ Series\\ in\\ 7\\ games\|P\_B=0.55)\*P(P\_B=0.55)}{P(Braves\\ win\\ World\\ Series\\ in\\ 7\\ games)}$$

So the question turns to how to calculate P( P<sub>B</sub>=0.55 ), P(
Braves win World Series in 7 games \| P<sub>B</sub>=0.55 ) and P( Braves
win World Series in 7 games ):

-   **P( P<sub>B</sub>=0.55 )**

    Due to the question, we are under the assumption that either
    *P*<sub>*B*</sub> = 0.55 or *P*<sub>*B*</sub> = 0.45, so the
    probability of PB=0.55 is equal to the probability of PB=0.45, which
    is 1/2. So:

$$P(P\_B=0.55)=P(P\_B=0.45)=\\frac{1}{2}=0.5$$

-   **P( Braves win World Series in 7 games \| P<sub>B</sub>=0.55 )**

    This is actually what we get in the question 1.

-   **P( Braves win World Series in 7 games )**

    We just need to sum up P(P<sub>B</sub>)\* P(Braves win World Series
    in 7 games\|P<sub>B</sub>)

``` r
#P(Braves win World Series in 7 games|P_B=0.55)
dnbinom(3, 4, 0.55)
```

    ## [1] 0.1667701

``` r
#P(Braves win World Series in 7 games)
0.5*dnbinom(3, 4, 0.55)+0.5*dnbinom(3, 4, 0.45)
```

    ## [1] 0.1516092

``` r
#Answer of question 5
dnbinom(3, 4, 0.55)*0.5/(0.5*dnbinom(3, 4, 0.55)+0.5*dnbinom(3, 4, 0.45))
```

    ## [1] 0.55

Finally, we can the get the answer, which is 0.55.
