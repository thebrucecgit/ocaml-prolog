studies(charlie, csc135).
studies(charlie, csc134).
studies(olivia, csc135).
studies(jack, csc131).  
studies(jack, csc135).  
studies(arthur, csc134).

teaches(kirke, csc135).  
teaches(collins, csc131). 
teaches(collins, csc171). 
teaches(juniper, csc134).
 

professor(X, Y) :- teaches(X, C), studies(Y, C).

