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
