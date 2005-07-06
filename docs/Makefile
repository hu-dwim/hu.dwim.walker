
docs: pdf html

html:
	mkdir -p html/
	echo "" | openmcl -b -e "(asdf:oos 'asdf:load-op :qbook)" \
	        -e "(asdf:oos 'asdf:load-op :arnesi)" \
	        -e "(asdf:oos 'qbook:publish-op :arnesi :generator (make-instance 'qbook:html-generator :output-directory \"./html/\" :title \"ARNESI\"))"
	cp *.css html/

pdf:
	mkdir -p pdf/
	echo "" | openmcl -b -e "(asdf:oos 'asdf:load-op :qbook)" \
	        -e "(asdf:oos 'asdf:load-op :arnesi)" \
	        -e "(asdf:oos 'qbook:publish-op :arnesi :generator (make-instance 'qbook:latex-generator :output-file \"./pdf/arnesi.tex\" :title \"ARNESI\"))"
	(cd pdf && pdflatex arnesi.tex)
	(cd pdf && pdflatex arnesi.tex)
	rm pdf/arnesi.aux pdf/arnesi.log pdf/arnesi.toc pdf/arnesi.tex
