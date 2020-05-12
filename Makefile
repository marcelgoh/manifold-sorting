BUILD = ocamlbuild

FLAGS = -r ${USEFLAGS} ${PKGFLAGS}
USEFLAGS = -use-ocamlfind
PKGFLAGS = -pkg extlib -pkg str -pkg unix

EXT = native
EXEC = main

${EXEC}: src/main.${EXT}
	cp _build/$< $@
	rm $@.${EXT}

.PHONY: clean
clean:
	${BUILD} -clean
	rm ${EXEC}

%:
	${BUILD} ${FLAGS} $@
