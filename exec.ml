open Printf
open Netlist_ast

(* exception pour les nappe de fils à la place de fils*)
exception Type_error of string

let b_to_i b = if b then 1 else 0;;

let holder b = Array.fold_left (fun n bit -> 2*n + (b_to_i bit)) 0 b;;


let value hshVar = function
	| Avar x -> Hashtbl.find hshVar x
	| Aconst c -> c
;;

let ex_eq hshVar hshReg rom ram = function

	(*Opérations sur un fil, généralisées sur les nappes*)

	| (a, Earg b) -> Hashtbl.replace hshVar a (value hshVar b)
	| (a, Ereg b) -> Hashtbl.replace hshVar a (Hashtbl.find hshReg b) 
	| (a, Enot b) -> Hashtbl.replace hshVar a (match value hshVar b with 
			| VBit b' -> VBit (not b') 
			| VBitArray b' -> VBitArray (Array.map (fun bit -> not bit) b')) 
	
	| (a, Ebinop (op, b, c)) -> begin match op, value hshVar b, value hshVar c with
			|Or, VBit b', VBit c' -> Hashtbl.replace hshVar a (VBit (b' || c'))
			|Xor, VBit b', VBit c' -> Hashtbl.replace hshVar a (VBit (b' <> c'))
			|And, VBit b', VBit c' ->Hashtbl.replace hshVar a (VBit (b' && c'))
			|Nand, VBit b', VBit c' -> Hashtbl.replace hshVar a (VBit (not((b' && c'))))
			|_, _, _ -> raise (Type_error "nappe à la place de fil simple")
			end
	
	| (a, Emux(b, c, _)) when value hshVar b = (VBit true) -> Hashtbl.replace hshVar a (value hshVar c)
	| (a, Emux(_,_, d)) -> Hashtbl.replace hshVar a (value hshVar d)


	(*gestion des opérations sur les nappes : fil considéré comme une nappe *)
		
	| (a, Econcat(b, c)) -> begin let concat = match value hshVar b, value hshVar c with
			|VBit b', VBit c' -> [|b'; c'|]
			|VBit b', VBitArray c' -> Array.append (Array.create 1 b') c'
			|VBitArray b', VBit c' -> Array.append b' (Array.create 1 c')
			|VBitArray b', VBitArray c' -> Array.append b' c'
			in
			Hashtbl.replace hshVar a (VBitArray concat)
			end

	| (a, Eslice(deb, fin, tab)) -> begin match value hshVar tab with 
												| VBitArray t -> Hashtbl.replace hshVar a (VBitArray (Array.sub t deb (fin-deb+1)))
												| VBit b when deb == 0 && fin == 0 -> Hashtbl.replace hshVar a (VBitArray [|b|])
														(*== ou simplement = ???*)
												| VBit _ -> raise (Type_error "fil à la place de nappe pour un slice")
									end

	| (a, Eselect(pos, tab)) -> Hashtbl.replace hshVar a (match value hshVar tab with 
																| VBitArray t -> VBit t.(pos) 
																| VBit b when pos = 0 -> VBit b
																| _ -> raise(Type_error "fil à la place de nappe pour un select"))
	(*mieux gérer toutes les erreurs possibles ici, accès hors tableau etc, peut être déjà rattrapées par Array ?*)
	
	(*gestion de la mémoire*)
		(*bit de poids fort en 0, A PRECISER DANS RAPPORT*)
	| (a, Erom(_, taille_mot, add)) -> 
						let pos = (match value hshVar add with  VBit b -> b_to_i b | VBitArray b -> holder b) in
						Hashtbl.replace hshVar a (match taille_mot with (*pour pas avoir de nappe de taille 1*)
													| 0 -> VBit (rom.(pos).(0))
													(*| k -> VBiArray (Array.sub rom pos k)*)
													| _ -> VBitArray (rom.(pos)) (*parce que rom / ram tableaux de tableaux ?*)
												 )
	
	(*même chose que rom à cet endroit, on aurait pu faire une fonction qui gérait ça*)
	| (a, Eram(_, taille_mot, add_lect, _, _, _)) -> 
						let pos = (match value hshVar add_lect with VBit b -> b_to_i b | VBitArray b -> holder b) in
						Hashtbl.replace hshVar a (match taille_mot with
													| 0 -> VBit (ram.(pos).(0))
													(*| k -> VBitArray (Array.sub ram pos k)*)
													| _ -> VBitArray (ram.(pos))
												 )

;;
