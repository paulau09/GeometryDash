#load "graphics.cma";;
#load "unix.cma";;

open Graphics;;

open_graph "1000x500+150+150";;

type carre = {mutable x : int; mutable y : int; mutable vy : int; couleur : color; taille : int};;

let joueur = {x = 400; y = 25; vy = 0; couleur = blue; taille = 25};;

set_color joueur.couleur;;

let hauteurNiveau = 20
and longueurNiveau = 130
and tempsSaut = 17
and posAffx = 400
and gravite = 6
and vitesse = 5
and bloc_ = int_of_char 'B'
and air_ = int_of_char ' ';;


let print_fichier fichier = 
	for i=0 to (hauteurNiveau-1) do
		for j=0 to (longueurNiveau+1) do
			let c = input_byte fichier in
			print_int c;
			print_newline ();
		done
	done;;


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
			if (valeur = air_) then (
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0xFFFFFF
						
					done
				done;
			
			) else if (valeur = bloc_) then (
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0
						
					done
				done;
				
			);
			
		done
	done;
	nouvArr;;


let decoupage arr =
	let arrDecoupee = Array.make longueurNiveau (Array.make_matrix (hauteurNiveau*joueur.taille) joueur.taille 0) in
	
	for i=0 to longueurNiveau-1 do
		
		for j=0 to joueur.taille-1 do
			
			for k=0 to (hauteurNiveau*joueur.taille)-1 do
				
				arrDecoupee.(i).(k).(j) <- arr.(k).(i*joueur.taille+j)
				
			done
			
		done
		
	done;
	arrDecoupee;;


let renverse arr = 
    let long = Array.length arr and
    tmp = ref arr.(0) in
    for i = 0 to (long/2 -1) do 
        tmp := arr.(i);
        arr.(i) <- arr.(long-i-1);
        arr.(long-i-1) <- !tmp;
    done;;


let niv1 = open_in_bin "../../Informatique/DM/Geometry Dash/niveau1.txt";;

let arr = file_to_byte_array niv1;;

 let arr2 = agrandir arr joueur.taille;;

let arr3 = decoupage arr2;;

let arr4 = Array.map make_image arr3;;

for i=0 to 1000/20 do
	draw_image arr4.(i) (i*20) 0;
done;;

renverse arr;;

let img = make_image arr2;;

let saut t = match t with
| 	x when 0 < x  -> t
|	_ -> 0;;


joueur.y <- 25;
joueur.x <- posAffx;
joueur.vy <- 0;
set_color joueur.couleur;
for i=0 to 400 do
	if (joueur.y mod 25 < gravite) then (
		
		let bloc2 = arr.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille)
		and bloc3 = arr.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille + 1) in
		
		if (bloc2 = bloc_ || bloc3 = bloc_) then (
			
			joueur.y <- joueur.y - (joueur.y mod 25);
			joueur.vy <- 0;
			
			if (key_pressed()) then (
				
				if (read_key() = ' ') then (
					
					joueur.vy <- tempsSaut;
					print_endline "AAAAAh";
					
				);
				while key_pressed() do
					let _ = read_key() in ()
				done;
			)
			
		) else if (bloc2 = air_ && bloc3 = air_) then (
			
			joueur.y <- joueur.y - gravite;
			
		)
		
	) else (
		
		joueur.y <- joueur.y - gravite;
		
	);
	
	if (joueur.vy > 0) then (
		joueur.y <- joueur.y + saut joueur.vy;
		joueur.vy <- joueur.vy - 1;
	);
	
	joueur.x <- joueur.x + vitesse;
	draw_image img (-vitesse*i) 0;
	fill_rect posAffx joueur.y joueur.taille joueur.taille;
	Unix.sleepf 0.016;
done;;


close_in niv1;;

clear_graph();;

sound 261 150;
sound 261 150;
sound 261 150;
sound 293 150;
sound 329 300;
sound 293 300;
sound 261 150;
sound 329 150;
sound 293 150;
sound 293 150;
sound 261 600;;





