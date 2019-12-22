
Requirements: Both R-Studio and R-GUI should be installed on the system.

1. R Studio
   Link to download R Studio: https://www.rstudio.com/

2. R-GUI
   Link to download R: https://www.r-project.org/

Library Requirement: Libraries needed to run some features and some visualization task. The following packages must either be present or be installedon R-studio.
                     crayon
                     htmltools
                     ggplot2
                     lattice
                     Performance analytics
                     scatterplot3d
                     gridExtra

Instructions for installing the packages in R studio
1. Run R studio
2. Click on the Packages tab in the bottom-right section and then click on install. The following dialog box will appear.
3. In the Install Packages dialog, write the package name you want to install under the Packages field and then click install. 
   This will install the package you searched for or give you a list of matching package based on your package text.

Note: Before running the R code make sure both the csv file and the .R code is in the working directory.
      To check the working directory type getwd() on the bottom left terminal of R-studio.

To run the code 
1. Open the code in R studio
2. Goto Code-> Run Region-> Run All

Instructions after execution of code:
While the program is running the texts and prompts for user inputs are either highlighted in blue or green text.

1. "do you want to view the datas of all 104 houses? input y for yes and n for no :"
   This is the first instruction that appears on the screen. If the user enter y it will display the entire csv file. If the user enter n, it will go to next step.  

2. "Enter the number of rows to be taken (should not be greater than 104) :" 
   It asks user to input the number of houses to be considered while performing the analysis and visualization. The output is displayed on terminal as well as separate window.

3. "Do you want to view the features of the housing data"
   "Enter y for yes n for no :"
   It displays the various features of each column such as average, min, max, median, mod, standard deviation, correlation, and covariance. If the user enter y it will display the result. If the user enter n, it will go to next step. 

4. "Input any value to view the price comparison among the years"
   "Enter"
   It visually plots the prices of all houses and diplays it in form of line graph and diverging bar. The user can enter any input and press enter to view this.  

5. select the year for which you want to perform visualization 
   1. 1998 
   2. 2007 
   3. 2011 
   4. 2014
   Enter your choice : 
   The user has the choice to choose any year to perform visualization. For example to perform visualization for year 2007, the user input 2.

6. select the parameter for x-axis for which you want to perform visualization 
   1. bedrooms 
   2. squarefeet 
   3. number of rooms 
   4. number of full baths 
   5. garage space 
   6. Walkscore
   Enter your choice : 
   The user has the choice to choose any parameter to plot the graph. For example to plot a graph for bedrooms VS price, the user input 1.

7. Do you want to continue visualising using another parameter 
   Enter any input value for yes and n for no : 
   The user has the choice to choose another parameter to plot the graph for that given year. If the user enter and value except n it go to step 5. If the user enter n, the program will go to final step.

8. A 3D graph is displayed to conclude the final hypotheses. 

 