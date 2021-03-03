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

## Disclaimer
> All information provided by the Department of Water Resources is made available to provide immediate access for the convenience of interested persons. While the Department believes the information to be reliable, human or mechanical error remains a possibility. Therefore, the Department does not guarantee the accuracy, completeness, timeliness, or correct sequencing of the information. Neither the Department of Water Resources nor any of the sources of the information shall be responsible for any errors or omissions, or for the use or results obtained from the use of this information.
