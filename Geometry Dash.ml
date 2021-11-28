#load "graphics.cma";;
#load "unix.cma";;

open Graphics;;

open_graph "1000x500+150+150";;


let hauteurNiveau = 25
and longueurNiveau = 100;;


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

let arr2 = agrandir arr 20;;

let img = make_image arr2;;

draw_image img 0 0;;

(*Création joueur*)
let coul = rgb 0 0 255;;
set_color coul;;
fill_rect 400 20 20 20;;

(*hauteur du joueur pour le saut*)
let h_joueur = ref 20.;;
let h_bloc = ref 20.;;

for i=0 to 500 do
	if (key_pressed()) then 
		if ((read_key()= ' ') && (!h_joueur = !h_bloc )) then 
			(h_joueur := (!h_joueur)+. 50. );
	if !h_joueur <> !h_bloc then h_joueur := !h_joueur -. 0.625; (*bien faire attention a décrementer de sorte que ca puisse être égal*)
	draw_image img (-i) 0;
	fill_rect 400 (int_of_float !h_joueur) 20 20;
	Unix.sleepf 0.002;
	done;;
       
close_in niv1;;

clear_graph();;














