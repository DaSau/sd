(*ON ADMET UNE SEULE RAM, A DIRE DANS RAPPORT*)

open Netlist_ast
(* on met les registres dans une Hashtbl pour pouvoir lire leurs valeurs*)		

let hshVar = Hashtbl.create 17;;
let hshReg = Hashtbl.create 17;;

let genHashReg prg =
	List.iter (fun (_, Ereg nom) -> Hashtbl.add hshReg nom (VBit false)) prg.p_eqs
;; 


let genHashVar prg = 
	Env.iter (fun key _  -> Hashtbl.add hshVar key (VBit false)) prg.p_vars
;;


let taille_mem prg = 
	let f l = function
		| (_, Eram(tAdr, tMot, _, _, _, _)) -> (tAdr, tMot)
		| (_, Erom(tAdr, tMot, _)) -> (tAdr, tMot)
	in
	List.fold_left f (0, 0) prg.p_eqs
;;


let rec print_var k = function 
	| VBit v -> Printf.printf ("%s %B") k v 
	| VBitArray v -> Array.iter (fun var -> Printf.printf("%s %B") k var) v 
;;	


let rec list_of_file f tMot = 
	try begin (*(match input_char f with "0" -> false |_ -> true)::(list_of_file f)*)
		let cur = Array.make tMot false in
		for i = 0 to (tMot-1) do 
			cur.(i) <- (match input_char f with
						| '1' -> true
						| '0' -> false)
		done;
		cur::(list_of_file f tMot)
	end
	with End_of_file -> []
;;


(*file, fichier_rom sont des noms de fichiers ; nbTour un entier*)
(*on lit tout sur l'entrée standard*)

let main file nbTour fichier_rom = 
	
	let prg_pasOrd = Netlist.read_file file in
	let prg = Scheduler.schedule prg_pasOrd in
	
	genHashVar prg;
	genHashReg prg;

	let (tAdr, tMot) = taille_mem prg in

	(*génère RAM*)
	let ram = Array.make_matrix (1 lsl tAdr) tMot false in
	
	(*mettre ROM dans tableau, donne ici tableau 2D*)
	let temp  = open_in fichier_rom in
	let rom = Array.of_list (list_of_file temp tMot) in

	(*mettre à jour la ram*)

	let ecrit_ram = function (*on considère que ram = tableau de tableau*)
		|(_, Eram(_, _, _, aut_ecr, adresse, data)) when Exec.value hshVar aut_ecr = VBit true -> 
			let v = match Exec.value hshVar data with
				| VBit b -> [|b|]
				| VBitArray b -> b
			in
			let ad = match Exec.value hshVar adresse with
				| VBit b -> Exec.b_to_i b
				| VBitArray b -> Exec.holder b
			in
			ram.(ad) <- v
		|_ -> ()
	in
	
	(*faire l'exécution du programme*)
	
	for i = 1 to nbTour do
		(*List.iter (fun (nom, valeur) -> Hashtbl.replace hshVar nom valeur) vEntree.(i);*)
		List.iter (fun nom -> (print_string (nom^" = ? "); 
							  Hashtbl.replace hshVar nom (Scanf.scanf "%d" (fun x -> match x with 1 -> VBit true | _ -> VBit false)))) 
			prg.p_inputs;

		List.iter (Exec.ex_eq hshVar hshReg rom ram) prg.p_eqs;

		(*maj ram*)
		List.iter ecrit_ram prg.p_eqs;		
		(*maj registres*)
		Hashtbl.iter (fun k _ -> Hashtbl.replace hshReg k (Hashtbl.find hshVar k)) hshReg;
		
		Hashtbl.iter print_var hshVar;
	done;
;;
let _ = main ()
