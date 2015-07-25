This directory contains two types of files:
.lat - for primary lattice cell information
.xyz - for coordinates of ligands

The format of .lat file

First line - put the number of differetn atoms 
in primary cell after "=" sign.

Example:
Number of atoms in cell= 80

Second line - description line - leave it intact.

Example:
Translation vectors

3,4,5-th lines - cartesian coordinates (3f7.4) of translation 
vectors of unit cell (a,b,c) in Angstrems.

Example: for bcc lattice
-6.1550 6.1550 6.1550
 6.1550-6.1550 6.1550
 6.1550 6.1550-6.1550

6- up to a total number of atoms - name of atom (character*3),
cartesian coordinates in Angstrems (3F10.6), charge of atom in 
fractions of elementary charge e (f6.2).

Example:

 YA  1.538750   .000000  3.077500  3.00
 ...
AlO   .000000   .000000   .000000  3.00



The format of .xyz file

First line - title of the case, up to 10 characters

Example:
NdYagYL 

Second line - the number of ligands (i2)  

Example: 
 8

Third and fourth lines - names of impurity ion and 
the ligand (both character*3)

Example:
Nd3
 O2

5th line - the value of exchange model parameter G (f4.1)

Example:
 8.0

6th - up to the number of ligands - cartesian coordinates
of ligands: arbitrary title of ligand(character*3),
 x,y,z(in Angstrems, 3f10.6),
charge(f6.2), distance to impurity ion(f10.6). 

Example:
Og  -0.424779 -1.915436  1.31612  -2.00  2.362520

