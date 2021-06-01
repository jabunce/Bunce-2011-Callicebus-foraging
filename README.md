# Bunce-2011-Callicebus-foraging
Data and analysis scripts for the manuscript:

Bunce, JA, LA Isbell, MN Grote, and GH Jacobs (2011) Color vision variation and foraging behavior in wild Neotropical titi monkeys (Callicebus brunneus): Possible mediating roles for spatial memory and reproductive status. International Journal of Primatology 32:1058-1075. Published [here](https://link.springer.com/article/10.1007/s10764-011-9522-y). 

The self-archived preprint is [here](https://jabunce.files.wordpress.com/2014/11/bunce_ijp_manuscript_10mar11.pdf).

This paper was published before I became aware of the open-data movement. Here, I revisit the data and analysis after more than 10 years in order to make them publically available. Some of the R functions I originally used for the analysis have been updated, but the qualitative results reported in the paper hold up rather well.

Steps to reproduce the analysis in this paper:

1) Create a project folder on your machine. Name it whatever you want.

2) Inside this project folder, put the file ``RunAll.R``

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the four ``.csv`` files

5) Inside the ``Code`` folder, put the other ``.R`` files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your project folder. You can run all of its parts in order in R. In that case, Figures 1 and 2 in the paper will appear in the ``Plots`` folder. Or you can open the individual scripts called by ``RunAll.R`` and run them separately to see a particular piece of the analysis. ``RunAll.R`` contains descriptions of the scripts it calls.

The excel file ``ConfidenceIntervals_Figure1_21may09a.xlsx`` contains results in the second and third worksheets that are reported in the Foraging Overview section (pg 1067-1068) of the paper. 
