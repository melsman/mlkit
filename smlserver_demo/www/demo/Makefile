
FILES= \
	time_of_day.sml 		\
	counter.sml 			\
	temp.html 			\
	temp.sml 			\
	recipe.html 			\
	recipe.sml 			\
	guess.sml 			\
	formvar.sml 			\
	formvar_chk.sml			\
	server.sml 			\
	currency_cache.html		\
	currency_cache.sml		\
	mail_form.sml			\
	mail.sml			\
	rating/index.sml		\
	rating/rating.sql		\
	rating/add0.sml			\
	rating/add.sml			\
	rating/wine.sml			\
	auth_form.sml			\
	auth_logout.sml			\
	auth.sml			\
	link/index.sml			\
	link/add_form.sml		\
	link/add.sml			\
	link/delete.sml			\
	hello.msp 			\
	mul.msp 			\
	calendar.msp 			\
	test.msp 			\
	images/index.html		\
	index.sml

FILES_TXT=$(FILES:%=%.txt)

%.txt: %
	cp $< $<.txt


all: $(FILES_TXT)

clean:
	rm -f $(FILES_TXT) *~ link/*~ images/*~ rating/*~