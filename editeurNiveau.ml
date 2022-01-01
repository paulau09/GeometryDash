#load "graphics.cma";;

open Graphics;;

open_graph " 1000x500+150+150";;
let hauteurNiveau = 20 
and longueurMax = 700
and tailleBloc = 25
and blocCourant = ref 'B'
and touches = [|'B';'b';'h';'D';'e'|];; (*e pour effacer*)

let blanc = 0xEEEEEE
and gris = 0x333333;;

let tr1 = [|10,250; 50,220; 50,280|];;
let tr2 = [|990,250; 950,220; 950,280|];;
fill_poly tr1;;
fill_poly tr2;;

let arr = Array.make 20 0;;

let affiche bloc x y = match bloc with
|	'B'-> (set_color gris; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	'b'-> (set_color gris; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc + 25;x-x mod tailleBloc +25 ,y-y mod tailleBloc + 25;x-x mod tailleBloc +13 ,y-y mod tailleBloc|])
|	'h'->	(set_color gris; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc;x-x mod tailleBloc +25 ,y-y mod tailleBloc ;x-x mod tailleBloc +13 ,y-y mod tailleBloc + 25|])
|	'D'-> (set_color gris; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc + 13) tailleBloc 13)
|	'e'-> (set_color blanc; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	_->();;

let grille = Array.make_matrix hauteurNiveau longueurMax 0 in 
	if key_pressed() = true then(
		let key = read_key() in
		if Array.memq key touches then blocCourant := key);

	if button_down() = true then (
		let a,b = mouse_pos() in 
			(*if (a<=50) then	*)						(*Gerer triangles*)
			if (a>=50 && a<=950) then(			(*Gerer blocs*)
				grille.(b/25).(a/25) <- !blocCourant;
				affiche !blocCourant a b );;


clear_graph();;
