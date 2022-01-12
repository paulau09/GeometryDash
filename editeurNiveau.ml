#load "graphics.cma";;

open Graphics;;
(*Enregistrement girlle dans fichier
deplacement de la grille
mettre l'image derriere
*)

open_graph " 1000x500+150+150";;
let hauteurNiveau = 20 
and longueurMax = 700
and tailleBloc = 25
and pos = ref 0
and blocCourant = ref 'B'
and touches = [|'B';'b';'h';'D';'e'|];; (*e pour effacer*)

let blanc = 0xEEEEEE
and gris = 0x333333;;

let tr1 = [|10,250; 50,220; 50,280|];;
let tr2 = [|990,250; 950,220; 950,280|];;
let c1 = 37, 250
and c2 = 963, 250;;
let u1 = -50, 0
and v1 = -40, -30
and u2 = -50,0
and v2 = 40,-30;;
fill_poly tr1;;
fill_poly tr2;;

type point = { x:int; y:int};;
let arr = Array.make 20 0;;

let pdt_scal v1 v2 = 
	let a,b = v1 
	and c,d = v2 in a*c+b*d;;

let dist pt1 pt2 = 
	let a,b = pt1
	and c,d = pt2 in
	sqrt ((float_of_int (c-a))**2. +. (float_of_int (d-b)));;

let affiche_niv grille = 
	fill_poly tr1;
	fill_poly tr2;
	let img = make_image grille in
	draw_image img 0 -!pos;;

let move_left = 
	if !pos != 0 then pos := !pos -50;;

let move_right = 
	if !pos != longueurMax*tailleBloc - size_x() then pos:= !pos +50;;

let affiche_bloc bloc x y = match bloc with
|	'B'-> (set_color gris; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	'b'-> (set_color gris; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc + 25;x-x mod tailleBloc +24 ,y-y mod tailleBloc + 25;x-x mod tailleBloc +13 ,y-y mod tailleBloc+1|])
|	'h'->	(set_color gris; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc;x-x mod tailleBloc +24 ,y-y mod tailleBloc ;x-x mod tailleBloc +13 ,y-y mod tailleBloc + 24|])
|	'D'-> (set_color gris; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc + 13) tailleBloc 13)
|	'e'-> (set_color transp; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	_->();;

let enregistre grille = 
	let fichier = open_out_bin "niv_perso.txt" in 
	for i=0 to Array.length grille -1 do 
		for j=0 to Array.length grille.(i) -1 do 
			(*ecrit dans fichier*)()
			done;
		done;
		;;

let grille = Array.make_matrix hauteurNiveau longueurMax ' ' in 
	while true do (
	if key_pressed() = true then(
		let key = read_key() in
		if Array.memq key touches then blocCourant := key);

	if button_down() = true then (
		let a,b = mouse_pos() in 
			(*if (a<=50) then	*)						(*Gerer triangles*)
			if (dist (a,b) c1) < 37. then (move_left; print_char 'g');
			if (dist (a,b) c2) < 37. then (move_right; print_char 'd');
			if (a>=50 && a<=950) then(			(*Gerer blocs*)
				grille.(b/25).(a/25) <- !blocCourant;
				affiche_bloc !blocCourant a b )
			))done;;

clear_graph();;
