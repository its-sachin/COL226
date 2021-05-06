# COL226: ASSIGNMENT3 (Type Checking and Evaluation)


  ### __How to run:__
1. Run the makefile by command ```make```
2. SML files with the name "bool.lex.sml", "bool.yacc.sig", "bool.yacc.sml", "bool.yacc.desc" and executable
"a2" will be generated.
3. Now run the file "loader.sml" in the sml environment using the command: ```use loader.sml```
4. Now these are the primary functions used to run the code:
  * ``` compile arg1 ``` : where arg1 is the path of the file to be executed. This function parse and execute the file and returns an value list, with each element as evaluated output of each statement.
  * ``` parse arg1 ``` : here also arg1 is the path of the file to be executed. This function only parse the file and generates the abstract syntax tree and performs no evaluation or type checking. Use this in case you want to check the ast specifically.
5. You can also execute the file by calling the executable a2 by running command ``` ./a2 arg1 ``` in terminal (not in sml environment) where arg1 is path of the file to be executed.
6. This simply calls the ``` compile arg1 ``` and prints whatever value it returns. 
7. Executed values of each statement will be printed in different lines and valid error in case of faulty file will be thrown.
8. I would recommend you to use SML/NJ environment and use step 4 to run the command as it clearly shows the return type i.e. ast and values list whereas through executable you can just check the evaluated output that too is converted to string.  
9. Run command  ```make clean``` to delete all sml files and executable generated.
