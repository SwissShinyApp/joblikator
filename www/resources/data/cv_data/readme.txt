
OK it's a bit tedious but you do it once, then only replicate it. If I find some time. I will do an automatic solution for this.

1. Rename these folders according to the job types as defined in meta_data/job_type.csv. They must be named exactly equal to the id in job_type.csv.

2. Second for all files inside these folders, replace the word type_1 with the id from job_type.csv

3. If you have versions in other languages (DE, PT), then you can copy the files and change the ending from "en" to "de" or "pt". Again, the appreciations should correspond to the id meta_data/language.csv.

4. Open the files and adjust your data.
- Attention: for example in the education file, you can see that some rows have doubled university entries in the sense that only the column details differ. This means that the CV will have only one title with the university and 3 lines of details. The same logic applies for the work file.

- The files projects and volunteer have just simple entries.

- the file skills will have the left most column as title and the second column will be the listed skills under this title.

5. The cv_txt_type_1_en.txt file is a plane text of your CV. Print the first CV, then copy paste the raw text to this file. This is needed for GPT.


6. The CV_type_1_en.Rmd file is a Markdown file in the R Vitae package style. If you don't understand it, it's best you don't touch it too much. Only change the following in case needed:

- if you want to change your CV style or sections, please follow the green comments inside the documents

- documentation here:
https://cran.r-project.org/web/packages/vitae/vignettes/vitae.html