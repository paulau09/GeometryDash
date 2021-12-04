#load "graphics.cma";;
#load "unix.cma";;

open Graphics;;

open_graph "1000x500+150+150";;

type carre = {mutable x : int; mutable y : int; mutable vy : int; couleur : color; taille : int};;

let joueur = {x = 400; y = 25; vy = 0; couleur = blue; taille = 25};;

set_color joueur.couleur;;

let hauteurNiveau = 20
and longueurNiveau = 100
and temps_saut = 17
and posAffx = 400
and gravite = 6
and vitesse = 5;;


let file_to_byte_array fichier =
	let grille = Array.make_matrix hauteurNiveau longueurNiveau 0 in
	for i=0 to (hauteurNiveau-1) do
		for j=0 to (longueurNiveau+1) do
			let c = input_byte fichier in
			if ((c <> int_of_char '\n') && (c <> int_of_char '\r')) then
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

let niv1 = open_in_bin "../../DM Jeu/niveau1.txt";;

let arr = file_to_byte_array niv1;;

let arr2 = agrandir arr joueur.taille;;

let img = make_image arr2;;

draw_image img 0 0;;

let h_bloc = ref 25;;

let saut t = match t with
| 	x when 0 < x  -> t
|	_ -> 0;;

joueur.y <- 25;;
joueur.x <- 400;;
joueur.vy <- 0;;
set_color joueur.couleur;;

for i=0 to 400 do
	if (key_pressed()) then 
		if ((read_key()= ' ') && (joueur.y = !h_bloc)) then 
			joueur.vy <- temps_saut;
	if joueur.vy > 0 then 
		(joueur.y <- joueur.y + saut joueur.vy ; joueur.vy <- joueur.vy - 1);
	if joueur.y - !h_bloc > gravite then
		joueur.y <- joueur.y - gravite 
		else (if joueur.y - !h_bloc > 0 then
			joueur.y <- !h_bloc);
	for k=(-1) to 1 do
		if (arr.((joueur.y / joueur.taille) - 1).((joueur.x / joueur.taille) + k)) = int_of_char 'B' then
			h_bloc := joueur.y / joueur.taille * joueur.taille
	done;
	joueur.x <- joueur.x + vitesse;
	draw_image img (-vitesse*i) 0;
	fill_rect posAffx joueur.y joueur.taille joueur.taille;
	Unix.sleepf 0.016;
	done;;

for i=0 to 400 do
	if (joueur.y mod 25 = 0) then (
		let blocDessous = arr.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille)
		if (blocDessous = 66) then (
			if (key_pressed()) then 
				if ((read_key()= ' ') && (joueur.y = !h_bloc)) then 
					joueur.vy <- temps_saut;
		)
	) else (
		if (joueur.y mod 25 > gravite) (
			joueur.y <- joueur.y - gravite;
		) else ()
	)
	done;;


close_in niv1;;

clear_graph();;











