#load "graphics.cma";;
#load "unix.cma";;

open Graphics;;

open_graph "1000x500+150+150";;

type point = {x: int; mutable y : int};;
type carre = {position : point; mutable vy :  int; couleur : color; taille : int};;

let joueur = {position = {x = 400; y = 25}; vy = 0; couleur = blue; taille = 25};;

set_color joueur.couleur;;

let hauteurNiveau = 25
and longueurNiveau = 100
and temps_saut = 24
and posx = 400;;


let file_to_byte_array fichier =
	let grille = Array.make_matrix hauteurNiveau longueurNiveau 0 in
	for i=0 to (hauteurNiveau-1) do
		for j=0 to (longueurNiveau) do
			let c = input_byte fichier in
			if (c <> int_of_char '\n') then
				grille.(i).(j) <- c
		done
	done;
	grille;;


let agrandir arr agrandissement =

	let taille1 = Array.length arr in
	let taille2 = Array.length arr.(0) in
	let nouvArr = Array.make_matrix (taille1*agrandissement) (taille2*agrandissement) 0 in
	
	for i=0 to (taille1-1) do
		
		for j=0 to (taille2-1) do
			
			let valeur = arr.(i).(j) in
			
			if (valeur = int_of_char ' ') then
				
				begin
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0xFFFFFF
						
					done
				done;
				end
			
			else if (valeur = int_of_char 'B') then
				
				begin
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0
						
					done
				done;
				end

		done
	done;
	nouvArr;;

let niv1 = open_in "../../niveau1.txt";;

let arr = file_to_byte_array niv1;;

let arr2 = agrandir arr 25;;

let img = make_image arr2;;

draw_image img 0 0;;

let h_bloc = ref 25.;;

let saut t = match t with
	| 	n when 14< n && n <=24 -> 7
	|	n when 0 < n && n<=14 -> -5
	|	_ -> 0;;

for i=0 to 600 do
	if (key_pressed()) then 
		if ((read_key()= ' ') && (joueur.position.y = 25)) then 
			joueur.vy <- temps_saut;
	if joueur.vy > 0 then 
		(joueur.position.y <- joueur.position.y+saut joueur.vy ; joueur.vy <- joueur.vy-1);
	draw_image img (-6*i) 0;
	fill_rect posx (joueur.position.y) joueur.taille joueur.taille;
	Unix.sleepf 0.025;
	done;;


close_in niv1;;

clear_graph();;
