# Project: Lyrics predictor

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Chencheng Jiang (cj2451)
+ Projec title: Lyrics predictor
+ Project summary: This project is a lyrics prediction, based on corresponding music features. The methodology is: 
	+ Music features extracting: I extracted both statistics features and segment series features from original data set. The statistics features include mean, varican, max and min of segments_loudness_max, segments_loudness_max_time, segments_pitches, bars_start and beats_start. The segment series features include 5 serieses of segments_loudness_max, segments_loudness_max_time, segment largest pitch and segments_timbre. For each series, I chose the location at 10%, 30% 50% 70% and 90% of the total length. Also, I added ACF and PACF value of segment duration, strongest pitch and segments_loudness_max. 
	+ Topic modeling: I used LDA to assign the probabilities of songs belonging to each topic. Based on cross validation, topic number is 15. Furthermore, I determined the probabilities of certain word belonging to certain topic. 
	+ Modeling: Based on cross validation, I set train set and test set. I used music features from train set to fit the 15 topics, using XGboost. 
	+ Prediction: I used fitted model and test set to predict the probablities of songs belonging to each topic, and multipled them by the probabilities of words. Furthermore, I used the frequency of the words as prior probabilities. Hence, I adjusted the probabilities of words and ranked them.
	+ Conclusion: If we only use words frequency, the average rank is about 650. Now, using new features and XGboost, I successfully decreased the rank to about 600. 
	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
