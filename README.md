# hapstep
Stepwise Analysis of Diversity for Haploid Data with Distances Between Haplotypes

This is based on original source code of Hapstep program written by Remy PETIT (Petit at pierroton.inra.fr),  April 2000.
Licenced under GNU General Public License. This class and methods will be included in the R package haplotypes in future updates.


## Usage

### S4 method for signature 'Dna'
hapstep(x,populations,indels="sic",populations, skip.se=TRUE, nperm=999,printprog=TRUE)

## Arguments

x: an object of class Dna from package haplotypes.

populations: a vector giving the populations, with one element per individual.

indels:  the indel coding method to be used. This must be one of "sic", "5th" or "missing". Any unambiguous substring can be given. See distance method for details.

skip.se: boolean; whether the standard errors are calculated or not.

nperm: the number of permutations. Set this to 0 to skip the permutation procedure.

printprog: boolean; whether messages and the progress bar are displayed or not.


## Value

An object of class hapstep, show-methods can be used to display the object. 

## Author(s)

Caner Aktas, caktas.aca at gmail.com

## Examples

Examples can be found at the end of the .r file classhapstep.r   





##
##

### Readme File of the Original Hapstep Program (Remy PETIT,  April 2000.):
This TURBO PASCAL program is based on the more simple program called HaPermut,
except that analyses can be made in a stepwise manner, by combining related haplotypes 
progressively, as explained in the paper by Odile Pons and RÈmy J. Petit 
(Genetics 1996, 144:1237-1245).
Last update: 12.03.2001

Here are the same explanations than in HaPermut:
HaPermut computes measures of diversity and differenciation from haploid 
population genetic data, when a measure of the distance between haplotypes is available,
and test whether the differentiation and diversity measures differ from the equivalent measures
that do not take into account the distances between haplotypes (ie, that consider all haplotypes 
equally divergent). 

The source file should be an ASCII file 
(its name should have 8 characters maximum: 12345678.txt)
 and should include the following information:

First line : 
Number of cytotypes    Number of populations  Number of characters distinguishing 
the variants (for instance number of polymorphic fragments, or of polymorphic 
nucleotide sites).

The program asks for the number of permutations to be made.
see the example (inperm.txt and outperm.out).

The program is dimensionned for a maximum number of 50 cytotypes, 100 populations,
and 40 characters. If you have more than this, it means the PASCAL program
permut2.pas should be modified accordingly and re-compiled. 

Then follows the number of individuals having a given cytotype (column)
in a given population (row). 

Finally, and without interruption, provide the table of character states for
all haplotypes, where each line corresponds to one haplotype, and each column to a 
character.

No column should be empty (no missing haplotype) and each population (row) 
should be composed of AT LEAST 3 individuals!

The output file provides permutated values of Nst in a single row, and the value of the last 5% 
and last 1%. The mean of the permutated values is also given and should be close to the Gst value (by construction). 
To test if the observed Nst value is larger than the Gst, we count how many permutated values 
are larger than the observed Nst. If you have 5% of the permutated values greater than the 
observed value of Nst, then your test is not significant, otherwise it is and you know
the P-value. This is akin to testing if Gst = Nst.






