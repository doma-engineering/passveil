doc: share/man/man1/passveil.1

share/man/man1/passveil.1! share/man/man1/passveil.md
	pandoc -s --to man -o ${.TARGET} ${.ALLSRC}
