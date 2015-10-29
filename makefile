all:
	ocamlopt -w -8 -o simulateur.out netlist_ast.ml netlist_parser.ml netlist_lexer.ml netlist.ml graph.ml scheduler.ml exec.ml main.ml

clean: 
	rm -rf simulateur *.cmi *.cmx *~ *.o
