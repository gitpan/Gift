Gift.pm: Gift.yp
	yapp -s -m Gift Gift.yp
	perl -c Gift.pm

clean:
	rm Gift.pm Gift.pdf

pdf:
	a2pdf Gift.yp -o Gift.pdf

doc:Gift.pm 
	pod2pdf.pl Gift.pm -o Gift.pdf
