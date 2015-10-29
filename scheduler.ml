open Netlist_ast
open Graph
exception Combinational_cycle

let read_exp (_, e) = match e with
 	| Earg (Avar a)
	| Enot (Avar a)
	| Erom (_, _, Avar a)
	| Eslice (_, _, Avar a)
	| Eselect (_, Avar a)
	| Ereg a 
			-> [] (*pour la gestion des registres, on inverse le sens des liens*)
	| Ebinop (_, Avar a, Avar b)
	| Econcat( Avar a , Avar b)
			-> [a; b]
	| Emux (Avar a, Avar b, Avar c) 
			-> [a; b; c]
	| Eram (_, _, Avar a, Avar b, Avar c, Avar d)
			-> [a; b; c; d]
	| _ -> []

let registre (dep, _) (_, e) = match e with
	| Ereg a when a = dep -> true
	| _ -> false


let schedule p =
	(*CRÉER tous lesw noeuds, ie les équations*)
	let g = Graph.mk_graph() in
	List.iter (Graph.add_node g) p.p_eqs;
	(*mettre les liens entre eux*)
	
	let cree_lien dep arr = 
		if (List.mem (fst dep.n_label) (read_exp arr.n_label))
			then Graph.add_edge g dep.n_label arr.n_label
		else if registre dep.n_label arr.n_label
			then Graph.add_edge g arr.n_label dep.n_label (*on inverse le lien*)
	in

	List.iter (fun n -> (List.iter (cree_lien n)  g.g_nodes)) g.g_nodes;
 
	if Graph.has_cycle g then raise Combinational_cycle
	else {p with p_eqs = Graph.topological g}

