exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g = 
	try
		let rec dfs noeud = match noeud.n_mark with
			| InProgress -> raise Cycle
			| Visited -> ()
			| NotVisited -> noeud.n_mark <- InProgress ; List.iter dfs  noeud.n_link_to ; 
											noeud.n_mark <- Visited ; ()
		in
		List.iter dfs g.g_nodes;
		false

	with Cycle -> true
	
	

let topological g =
	clear_marks g; 
	let ret = ref [] in
	let rec dfs noeud = match noeud.n_mark with
		| Visited -> ()
		| NotVisited -> noeud.n_mark <- Visited; List.iter dfs noeud.n_link_to; 
										ret := (noeud.n_label)::(!ret);
	in
	List.iter dfs g.g_nodes;
	!ret
