(* ------------ Ouverture des fichiers externes ---------------- *)

#load "graphics.cma";;
#load "unix.cma";;

open Graphics;;

(* ----------- Ouverture de la fenêtre ---------------- *)

open_graph " 1000x500+150+150";;

(* ----------- Création du type carré pour le joueur ----------- *)

type carre = {mutable x : int; mutable y : int; mutable vy : int; couleur : color; taille : int; xc : float; mutable yc : float};;
type point = {mutable x : float; mutable y : float};;

let joueur = {x = 400; y = 25; vy = 0; couleur = blue; taille = 25; xc = 412.5; yc = 37.5};;

set_color joueur.couleur;;

(* ---------- Définition des constantes --------------- *)

let hauteurNiveau = 20
and longueurNiveau = 190
and tempsSaut = 17
and posAffx = 400
and gravite = 6
and vitesse = 6
and bloc_ = int_of_char 'B'
and air_ = int_of_char ' '
and picHaut_ = int_of_char 'h'
and conservationInput = 10
and rayon = sqrt (2. *. (float_of_int joueur.taille /. 2.)**2.)
and pi = 4. *. atan 1.;;

(* ---------- Définition des fonctions --------------- *)

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
				
			) else if (valeur = picHaut_) then (
				
				let centrey = agrandissement/2 + 1 in
				Printf.printf "%d \n" centrey;
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						if (abs(centrey-l) > k) then (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0xFFFFFF;
							
						) else (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- 0;
							
						)
						
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


let saut t = match t with
| 	x when 0 < x  -> t
|	_ -> 0;;

let foi pt = 	(* convertit point -> int*int *)
	let a,b = int_of_float pt.x, int_of_float pt.y in
	a,b;;

(* -------------- Ouverture du niveau ------------- *)

let niv1 = open_in_bin "niveau1.txt";;

let grille = file_to_byte_array niv1;;

let affichage = agrandir grille joueur.taille;;
(*
let arr3 = decoupage arr2;;

let arr4 = Array.map make_image arr3;;

for i=0 to 1000/20 do
	draw_image arr4.(i) (i*20) 0;
done;;*)

renverse grille;;

let img = make_image affichage;;

(* ------------- Boucle de jeu ------------- *)

joueur.y <- 25;;
joueur.x <- posAffx;;
joueur.vy <- 0;;
let angle = ref (5. *. pi /. 4.);;
let point1 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle};;
let point2 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle};;
let point3 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle};;
let point4 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle};;

set_color joueur.couleur;
	
let conserve = ref conservationInput in
	
let i = ref 0
and stop = ref 651 in
while (!i)<(!stop) do
	
	if (joueur.x mod joueur.taille > joueur.taille - vitesse) then (
		
		let bloc6 = grille.(joueur.y / joueur.taille).(joueur.x / joueur.taille + 1)
		and bloc9 = grille.(joueur.y / joueur.taille + 1).(joueur.x / joueur.taille + 1) in
		
		if ((bloc6 = bloc_)
				|| (joueur.y mod joueur.taille > 0 && bloc9 = bloc_)
				|| (bloc6 = picHaut_)
				|| (joueur.y mod joueur.taille > 0 && bloc9 = picHaut_)) then (
			
			i := !stop;
			
		);
		
	);
	
	
	if (joueur.y mod joueur.taille < gravite) then (
		
		let bloc2 = grille.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille)
		and bloc3 = grille.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille + 1) in
		
		if (bloc2 = bloc_ || bloc3 = bloc_) then (
			
			joueur.y <- joueur.y - (joueur.y mod joueur.taille);
			joueur.vy <- 0;
			
			if (key_pressed()) then (
				
				if (read_key() = ' ') then (
					
					joueur.vy <- tempsSaut;
					
				);
				while key_pressed() do
					let _ = read_key() in ()
				done;
			)
			
		) else if (bloc2 = air_ && bloc3 = air_) then (
			
			joueur.y <- joueur.y - gravite;
			
		) else if (bloc2 = picHaut_ || bloc3 = picHaut_) then (
			
			i := !stop;
			
		)
		
	) else (
		
		joueur.y <- joueur.y - gravite;
		
	);
	
	if (joueur.vy > 0) then (
		joueur.y <- joueur.y + saut joueur.vy;
		joueur.vy <- joueur.vy - 1;
		angle := !angle -. pi /. (2. *. (float_of_int tempsSaut)) ;
	);
	
	joueur.x <- joueur.x + vitesse;
	
	draw_image img (-vitesse*(!i)) 0;

(*-------------------Partie rotation--------------------- *)
	joueur.yc <- float_of_int joueur.y +. 12.5;	
	(*fill_rect posAffx joueur.y joueur.taille joueur.taille;*)


	point1.x <- joueur.xc +. rayon *. cos !angle;
	point1.y <- joueur.yc +. rayon *. sin !angle;
	point2.x <- joueur.xc +. rayon *. cos (!angle +. pi /. 2.);
	point2.y <- joueur.yc +. rayon *. sin (!angle +. pi /. 2.);
	point3.x <- joueur.xc +. rayon *. cos (!angle +. pi);
	point3.y <- joueur.yc +. rayon *. sin (!angle +. pi);
	point4.x <- joueur.xc +. rayon *. cos (!angle -. pi /. 2.);
	point4.y <- joueur.yc +. rayon *. sin (!angle -. pi /. 2.);

	let poly = [|foi point1; foi point2; foi point3; foi point4|] in

		fill_poly poly;


	if (key_pressed()) then (
		
		if ((!conserve) > 0) then (
			
			conserve := !conserve - 1;
			
		) else (
			
			conserve := conservationInput;
			let _ = read_key() in ();
			
		)
		
	);
	
	Unix.sleepf 0.016;
	i := (!i) + 1;
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




