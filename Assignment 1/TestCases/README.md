Testcases
===

Correct Input Case
---

The testcases "himym.csv" covers most intricacies of a normal CSV file including Quotes, Newlines, Delimiters present 
inside Fields and empty Fields. I have also added sample output when the delimiter is converted from comma to semicolon 
("himym.ssv"), amperstand ("himym.ssv") and vertical bar ("himym.vsv"). This testcase gives an example of a correct 
input file.

Incorrect Input Case
---

The testcase "himym_uneven.csv" is exactly same as "himym.csv" but one comma has been removed in Record 5. This results 
in one less field. Thus you should raise the UnevenFieldException with string being "Expected: 5 fields, Present: 4 fields on Line 5" 