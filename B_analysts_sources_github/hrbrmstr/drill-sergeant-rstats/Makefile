all : gitbook
.PHONY: gitbook pdf epub word sync

gitbook :
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook", quiet=TRUE)' && open docs/index.html

pdf:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book", quiet=TRUE)' 

epub:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::epub_book", quiet=TRUE)' 

word:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::word_document2", quiet=TRUE)' 

git:
	git add -A 
	git commit -m "Book update"
	git push

sync:
	rsync -azP --delete docs/ bob@rud.is:/var/sites/rud.is/books/drill-sergeant-rstats/

