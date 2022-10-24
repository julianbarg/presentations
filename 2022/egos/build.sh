#!/usr/bin/env bash

/bin/pandoc egos_2022.md \
--reference-doc ivey-powerpoint-template.pptx  \
--citeproc --bibliography ~/bibliography.bib \
--csl ~/apa-6th-edition.csl \
-f markdown+emoji \
-o egos_2022.pptx \
	&& xdg-open egos_2022.pptx