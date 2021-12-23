# Gaining The Competitive Edge
### Analyzing the Importance and Potential Success Drivers of Political Marketing Strategies Regarding the 2021 Federal Elections in Germany

Master Thesis Marketing Research

Master Program – M.Sc. Marketing Analytics

Marketing Department Tilburg School of Economics and Management

Tilburg University (TiU)

![background](https://user-images.githubusercontent.com/80161087/147277170-8121893c-f3ac-42e8-9657-7f824ad88f57.png)


## Abstract
Based on the work of Giavazzi et al. (2020), which identified a shift in text similarity between German political parties following terror attacks, this research conducted a topic-specific analysis to identify similar shifts in central election topics. The topic-specific stance analysis detected several shifts in language by the parties. The topic of Corona was discussed with more right-wing language by all parties compared to their overall stance position. In contrast, the topic Environment was discussed with more left-wing language by all parties compared to their overall stance position. For this purpose, this research collected two large data sets from Twitter and the 2017- and 2021-party programs of the six major political parties. As a unique contribution to political marketing research, this paper conducted a comprehensive text analysis of the party activities on Twitter regarding the 2021 Federal Elections in Germany. Additionally, this research derived practical, data-driven recommendations for potential success drivers for political marketing regarding elections. These recommendations address the public candidate image, emotive strategies, and practical techniques to manipulate voter behavior.



---

## Introduction
On September 26th, 2021, voters in Germany elected a new federal government for the upcoming legislature period. As the first incumbent chancellor in German history, Angela Merkel (CDU/CSU) did not run for office again after 16 years as head of government. The campaigning for her successor was a tight and suspenseful race with substantial fluctuation in voter supports: three parties in the Conservatives (CDU/CSU), Greens (Grüne), and Social Democrats (SPD) all held supremacy in the polls at some point. On election day, the SPD only prevailed by 1.6% – a difference of only approximately 700,000 votes out of 61 million eligible voters (Politico 2021).

This paper argues that political marketing should be considered a capable tool to generate a competitive edge over opposing parties in tight campaign races like this one for several reasons. Firstly, political marketing can generate a competitive edge because most politicians utilize social media to amplify messages and a platform for public display (Silva & Proksch 2021). Particularly Twitter as a text-centric platform has gained popularity among German politicians: roughly 70% of the current parliament members utilize the platform to connect with the voting population and colleagues (Pollytix 2021). As the digital form of election posters and press releases, it has been argued that Twitter may soon replace its analog predecessors due to its cost-efficiency and more effective targeting (Tawadrous 2021, Jaursch 2021, Jaursch 2020). Secondly, political marketing can generate a competitive edge because many voters are susceptible to persuasion. Two weeks before the elections, 23% of polled German voters were uncertain of their voting decision (Spiegel 2021). A similar pattern of 10 to 20% of undecided voters can also be empirically established in other European countries like Italy and France (Alam & Riva 2019, La Repubblica 2006, Mannheimer 2003). Lastly, political marketing can generate a competitive edge because it can persuade people in their voting decision. After becoming president in 2016, Donald Trump famously stated: “I think I would not be here if I did not have social media” (Wired 2016). His digital media director Brad Parscale specified that Twitter was crucial for gaining votes, and Facebook was fundamentally important for fundraising (Fujiwara et al., 2020). Indeed, recent research substantiates evidence for the effect of Twitter and Facebook on the voter turnout in favor of candidates dependent on the party peculiarity (Fujiwara et al. 2020, Liberini et al. 2018). Hence, this paper argues that social media's effect on the elections results from the perceptions of party positioning and the election-specific circumstances.

In addition to the content stances, the measurements of sentiments, emotions, and text similarity can be used to validly determine party positioning (Silva & Proksch 2021, Burst et al. 2021, Jolly et al. 2019, Laver et al. 2002). Based on the work of Giavazzi et al. (2020), which identified a shift in text similarity between German political parties following terror attacks, this research conducted a topic-specific analysis to identify similar shifts in central election topics. For this purpose, this research collected two large data sets from Twitter and the 2017- and 2021-party programs of the six major political parties. The first data set represents the posts of verified 2021 candidate accounts from the six major parties within the campaign period (Dec. 2020 – Sep. 2021). The second data set represents the posts from user-generated discussions based on election-relevant hashtag queries within the last four weeks before the elections. As initial positioning of parties, the Chapel Hill Expert Survey (2019) was deployed for the left-to-right positioning of the six major political parties and assigned to the texts of the respective 2017 party programs (Jolly et al. 2019).

![General Stances](https://user-images.githubusercontent.com/80161087/147256429-82abd319-abdc-4fe3-ad77-1606f4113ccc.jpg)

Figure 1: Relative Positioning of Major Political Parties based on their general wordscores (own rep.).

In line with current research, the two incumbent government parties (CDU/CSU & SPD) have utilized more positive sentiment than the opposition parties (Crabtree et al. 2020). Furthermore, the sentimental and emotive strategy of the fringe parties displays the expected results: whereas the Left exhibits the highest scores for negative sentiment and the emotion sadness, the Alternative (AfD) showcases the lowest score for positive sentiment and the highest scores for the emotions anger, disgust, and fear in their political marketing activities. The text-similarity of the six major political parties has continuously and considerably increased between 2017 and 2021 party programs and campaign period tweets - thus resulting in the most similar relative positioning for all parties for the campaign period tweets. The topic-specific stance analysis detected several shifts in language by the parties. The topic of Corona was discussed with more right-wing language by all parties compared to their overall stance position. This shift was arguably led by the Liberal Democrats (FDP) and the Alternative (AfD) based on tweet volume. In contrast, the topic Environment was discussed with more left-wing language by all parties compared to their overall stance position. This shift was arguably led by the Greens based on tweet volume.
As a unique contribution to political science, this research conducted a comprehensive text analysis of the party activities on Twitter regarding the 2021 Federal Elections in Germany. Additionally, this research derived practical, data-driven recommendations for potential success drivers for political marketing regarding elections. These include the public candidate image, emotive strategies, and practical techniques to manipulate voter behavior.

![Topic Stances](https://user-images.githubusercontent.com/80161087/147256468-b10e7736-90b9-4b69-93b6-fe5a61b09b75.jpg)

Figure 2: Topic-specific tweet stances for the topics Corona, Environment and Digitization in comparison to the respective overall stances for all tweets (own rep.).

---

## Dependencies
- R 
- R packages:
	- Data Collection:		rtweet
	- Data Manipulation:		dplyr, data.table, stringr, lubridate, tidytext
	- Text Analysis:		syuzhet, quanteda, tm, manifestoR
	- Visualization:		modelsummary, ggplot2, ggpubr, ggdendro, wordcloud, wordcloud2
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
