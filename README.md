# Reproducible research: version control and R
### Questions 1, 2 and 3
https://github.com/anon4395/logistic_growth
### Question 4:
(i) The 'random walks' code produces an output which plots a randomised path of a point on a two dimensional plane. The colour corresponds to the time at which the point was where, so we can tell the point started at the beginning of the dark blue line end, and ended at the the point at the end of the lighter blue line end. Each time the *random_walk()* function is executed, a different random path is generated. The number within the bracket of this function specifies how long this walk should be, in this case, 500 time units.
![Graph of initial random walks output](https://github.com/anon4395/reproducible-research_homework/blob/main/random_walks_plot.png)

(ii) A random seed is a start point which initialises a psuedorandom number generator. When a 'random' number generator algorithm is run in R, its output is not truly random, but rather, it is psuedorandom, being determined by which 'random seed' was used to intialise it. If the same seed is used, the same 'random' output will be produced each time the function is executed, making the output reproducible. Depending on the algorithm in use, the starting input (the seed) will be used in different ways mathematically, but the same seed in the same algorithm will always produce the same output. This means if we specify the seed before producing the *random_walks* simulation, the same output will be produced each time the code is run.

(iv) Reproducible simulation commit history:
![commit history](https://github.com/anon4395/reproducible-research_homework/blob/main/random_walk_commit_history.png)

### Question 5:
(i) The table has 13 columns, and 33 rows.

(ii) A log transformation can be used to fit a linear model to the allometric equation:

$$\ V = \beta L^\alpha \$$

$$\ ln(V) = ln(\beta) + \alpha ln(L) \$$
We can transform the data (see [question_5_code.R](/anon4395/reproducible-research_homework/blob/main/question_5_code.R) for full code) and fit the linear model as such:
```
log_virus_data <- virus_data %>%
  mutate(log_V = log(virion_volume_nm_nm_nm)) %>%
  mutate(log_L = log(genome_length_kb))

model1 <- lm(log_V ~ log_L, log_virus_data)
```

(iii) In the linear model, the gradient is alpha and the intercept is ln(beta). Values from *summary()* table give:
$$\ \alpha = 1.5152 \$$
$$\ \beta = e^{7.0748} = 1181.8 \$$

When rounded, these are the same values as in table 2 of the paper (alpha = 1.52, beta = 1182). 

(iv) Code to reproduce the figure (see also [question_5_code.R](/anon4395/reproducible-research_homework/blob/main/question_5_code.R))
```
log_virus_data %>%
  ggplot(aes(x=log_L, 
             y=log_V))+
  geom_point()+
  geom_smooth(method="lm", linewidth=0.8)+
  labs(x="log[Genome length (kb)]", 
       y ="log[Virion volume(nm3)]")+
  theme(axis.title = element_text(face="bold"))+
  theme_bw()
```
(v) Estimated volume of 300 kb dsDNA virus:
```
#Equation of linear model
log_V <- 7.0748 + 1.5152*log_L 

#L value input
log_L <- log(300)

#Calculate V
V <- exp(log_V)
V
```
Estimated volume of 300kb dsDNA virus is 6697006.6nm^3 (or 6.70 x 10^6 to 3sf).

### Bonus Question:
Reproducibility refers to using the same analytical methods and code to obtain the same results as the first analysis. Replicability on the other hand refers to repeating a whole study using the same research methods on a newly collected dataset to obtain the same overall results. Reproducibility is then mostly a measure of the robustness of analytical methods (and the code used to do this), whereas replicability more broadly considers the study design.

Git enhances reproducibility by enabling version control, so that changes in analysis code are tracked and visible. Github enables sharing of this code, including its version history, so that other researchers can critique the code and analytical methods used, and also use them to reproduce the data. These files are dated so anyone can tell how recently they have been updated. 

Additionally, Github enables collaboration, as repositories can be forked, which directly enables another researcher to easily reproduce your data. Also, if multiple researchers are working on the same project, this forking feature enables different people to work on different versions of an analysis code, and later merge these into a main repository. This makes collaboration easier, but also protects the main files from accruing errors. 

The structure of github repositories also provides background information about the study and the analysis, including an informative README file, raw data, and coding files. This open access makes replication of studies much easier. Furthermore, anyone viewing a github repository can easily contact the owner to ask any questions about the data or analysis, which may speed up the process of reproduction or replication.

However, there are also some limitations to git and github. Although they are very user-friendly, it can take a little while to get used to the workflows, which might be an initial limitation. Another limitation may arise if the dataset in question is sensitive and there are privacy concerns, as storing this in a public github repository would not be suitable. There are also limits to the amount of files and data that can be stored in a single repository, which may be problematic if a very large project is bein undertaken. 

## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

**Bonus** (**10 points**) Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
