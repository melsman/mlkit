../../../bin/mlbmake: Makefile sources.cm *.sml
	echo 'CM.make();' | sml

clean:
	rm -rf CM PM MLB *~

mlbmake: Makefile sources.mlb *.sml
	../../../bin/mlbmake sources.mlb
	mv a.out mlbmake