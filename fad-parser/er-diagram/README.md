To generate the ER diagram (such as it is), run

fdp -Tsvg -o er-diagram.svg er-diagram.dot 

Note that several important things are missing from this diagram.

Specifically, among other things:

1) 1-to-many, many-to-many, etc. 
2) primary key highlighting
3) "derived entity" indications or whatever you call the double boxes.
