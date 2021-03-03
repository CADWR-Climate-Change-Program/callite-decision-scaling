# CalLite [D]ecision-[S]caling

This repository performs a decision scaling analysis of the central valley water system simulated with CalLite 3.0. 

## Structure
	|— runbook
		R-markdown files that are meant to 
		be run in sequence using RStudio project notebook. 
	|— src :
		R source code functions for decision scaling with CalLite
		|— python :
			Python scripts for conversion of DSS records to text files (and vice-versa) 
		|— Run :
			Default wresl code and lookup tables for CalLite
		|— Run-Alternatives :
			Alternative wresl code files to represent different CalLite configurations (e.g., 32 or 64 bit ANN)
		|— Run-[*name*] :
			Alternative wresl code files to represent a significant CalLite code change (e.g., to represent adaptive water year typing)

## Contact
[Wyatt.Arnold@water.ca.gov](mailtto:Wyatt.Arnold@water.ca.gov)

## Contributors
- Andrew Schwarz
- Patrick Ray
- Sungwook Wi
- Wyatt Arnold
- Jordi Vasquez
- Matt Correa
