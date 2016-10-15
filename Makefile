.PHONY: default clean
default: simul.opt

SOURCES = vector.ml normal.ml sphere.ml ball.ml shell.ml main.ml

simul.opt: $(SOURCES)
	ocamlopt -o simul.opt $(SOURCES)

clean:
	rm -f *.cm* *.[oa] *.opt *~
