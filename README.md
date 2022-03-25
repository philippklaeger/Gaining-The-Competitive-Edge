# Gaining The Competitive Edge
### Analyzing Candidate-Party Congruence and Issue Salience of Campaign Winners in the 2021 German Federal Elections

![Gaining The Competitive Edge](https://user-images.githubusercontent.com/80161087/160180333-29a5f6dd-282b-4e38-ad5d-a8b8848e936a.png)

## Abstract
This research highlights and dissects the importance of social media for electoral campaigns in the US and western European countries (Hoegg & Lewis, 2011; Silva & Proksch, 2021; Williams, 2017). The results of this research present empirical evidence for a correlation between a higher candidate-party congruence in candidate tweets and the campaign winners (SPD & FDP) of the 2021 German Federal Elections. The candidates of campaign winners conveyed the most congruent messaging on social media, thus creating a more consistent public image to potential voters. This is in line with current research, as more consistent advertising messages are correlated with increases in online WOM and voter preference (Fossen et al., 2022).

We collect two large sets of data. First, we collect all electoral candidate tweets of the six major political parties in Germany during the entire campaign period. Second, we collect the party manifestos of the six major political parties for the 2017 and 2021 Federal Elections. We deploy advanced text analysis methods for this research. The wordscore procedure allows us to map texts on a left-to-right wing scale based on the occurrence of words in the texts and expert evaluations on political ideology (Laver et al., 2003). We adjust this method with a transformation that converts the raw scores into more robust estimates (Martin & Vanberg, 2007). Using this methodology, we can map all collected party manifestos and candidate tweets on a left-to-right wing scale for our analysis. To account for potentially latent heterogeneity of topics in the tweets, we also examine issue salience regarding central election topics (Corona, Environment) individually in addition to the overall tweet positioning.

Our results present novel implications for academic research and emphasize the need to further study and build robust measures of candidate-party congruence. Our findings have implications beyond the 2021 German Federal Elections. They apply to other German federal and state elections because they have the same political system and at least partially the same voters. Outside of German politics, the importance of candidate-party congruence applies to other countries with a multi-party system and electoral systems with proportional representation such as Belgium, Denmark, Sweden, and the Netherlands.


---

## Dependencies
- R 
- R packages:
	- Data Collection:		rtweet
	- Data Manipulation:		dplyr, data.table, stringr, lubridate, tidytext
	- Text Analysis:		quanteda, manifestoR
	- Visualization:		ggplot2, ggpubr, wordcloud, wordcloud2
- ManifestoR API Key:			https://manifesto-project.wzb.eu/
- 2019 Chapel Hill Expert Survey:	https://www.chesdata.eu/2019-chapel-hill-expert-survey

## Notes
- Due to the terms and conditions of Twitter, the data sets cannot be uploaded fully on GitHub. However the political marketing data set can be fully reproduced by using the download.R script in the code section.
- Reproducibility: encoding readers must be considerate of German special characters (e.g., ä) to attain an undistorted display of the data.
- Introduction into German politics (parties & electoral system) can be found in the appendix of the thesis file.

---

## Contributors
Philipp Kläger

## License
<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />

This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
