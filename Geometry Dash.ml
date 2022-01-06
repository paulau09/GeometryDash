(* ------------ Ouverture des fichiers externes ---------------- *)

#use "topfind";;

#require "graphics";;
#load "unix.cma";;

open Graphics;;

(* ----------- Ouverture de la fenêtre ---------------- *)

open_graph " 1000x500+150+150";;

(* ----------- Création du type carré pour le joueur ----------- *)

type carre = {mutable x : int; mutable y : int; mutable vy : int; couleur : color; taille : int; xc : float; mutable yc : float};;
type point = {mutable x : float; mutable y : float};;

let joueur = {x = 400; y = 25; vy = 0; couleur = 0x3333FF; taille = 25; xc = 412.5; yc = 37.5};;

(* ---------- Définition des constantes --------------- *)

let hauteurNiveau = 20
and longueurNiveau = 591
	
and tempsSaut = 17
and posAffx = 400
	
and gravite = 6
and vitesse = 6
	
and conservationInput = 20
	
and rayon = sqrt(2.) *. (float_of_int joueur.taille /. 2.)
and pi = 4. *. atan 1.

and couleurFond = 0xEEEEEE
and couleurBloc = 0x333333
	
and bloc_ = int_of_char 'B'
and air_ = int_of_char ' '
and picHaut_ = int_of_char 'h'
and picBas_ = int_of_char 'b'
and demiBloc_ = int_of_char 'D';;

exception Mort;;

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

(* Permet de passer d'une grille lue dans le fichier à un array qui peut être utilisé ppour le make_image *)
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
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurFond
						
					done
				done;
			
			) else if (valeur = bloc_) then (
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurBloc
						
					done
				done;
				
			) else if (valeur = picHaut_) then (
				
				let centrey = agrandissement/2 + 1 in
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						if (abs(centrey-l) > k) then (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurFond;
							
						) else (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurBloc;
							
						)
						
					done
				done;
			) else if (valeur = picBas_) then (
				
				let centrey = agrandissement/2 + 1 in
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						if (abs(centrey-l) >= (agrandissement - k)) then (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurFond;
							
						) else (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurBloc;
							
						)
						
					done
				done;	
			) else if (valeur = demiBloc_) then (
				
				for k=0 to (agrandissement-1) do
					
					for l=0 to (agrandissement-1) do
						
						if (k < agrandissement/2) then (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurBloc;
							
						) else (
							
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- couleurFond;
							
						)
						
					done
				done;
			);
			
		done
		
	done;
	nouvArr;;


let renverse arr = 
	let long = Array.length arr and
	tmp = ref arr.(0) in
	for i = 0 to (long/2 -1) do 
			tmp := arr.(i);
			arr.(i) <- arr.(long-i-1);
			arr.(long-i-1) <- !tmp;
	done;;


let arrondi f = int_of_float (f +. 0.5);;


let foi pt = 	(* convertit point -> int*int *)
	let a,b = arrondi pt.x, arrondi pt.y in
	a,b;;


let checkBloc6 (j : carre) g =

	let bloc6 = g.(j.y / j.taille).(j.x / j.taille + 1) in

	if ((bloc6 <> air_)) then (
	
			raise Mort;
	
	);;


let checkBloc9 (j : carre) g = 

	let bloc9 = g.(j.y / j.taille + 1).(j.x / j.taille + 1) in

	if ((j.y mod j.taille > 0) && (bloc9 <> air_)) then (
	
		raise Mort;

	);;


let checkBloc8 (j : carre) g =

	let bloc8 = g.(j.y / j.taille + 1).(j.x / j.taille) in

	if ((j.y mod j.taille > 0) && (bloc8 <> air_)) then (
	
		raise Mort;

	);;


(* -------------- Ouverture du niveau ------------- *)

let niv1 = open_in_bin "niveau1.txt";;

let grille = file_to_byte_array niv1;;

close_in niv1;;

let affichage = agrandir grille joueur.taille;;

renverse grille;;

let img = make_image affichage;;

(* ------------- Boucle de jeu ------------- *)

let rec loop () =

	joueur.y <- 25;
	joueur.x <- posAffx;
	joueur.vy <- 0;
		
	let angle = ref (5. *. pi /. 4.) in
	let point1 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle}
	and point2 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle}
	and point3 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle}
	and point4 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle} in
		
	set_color joueur.couleur;
		
	let conserve = ref conservationInput in
		
	let i = ref 0
	and temps = ref (Sys.time())
	and stop = longueurNiveau*joueur.taille/vitesse in 
	while (!i)<stop do

		if (Sys.time() -. (!temps) >= 0.016) then ( (* 60 FPS *)

			try

				checkBloc8 joueur grille;
			
				if (joueur.x mod joueur.taille >= joueur.taille - vitesse) then (
					
					checkBloc6 joueur grille;
					checkBloc9 joueur grille;
					
				);
				
				if (joueur.y mod joueur.taille < gravite) then ( (* On considère le joueur alors sur le bloc *)
					
					let bloc2 = grille.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille)
					and bloc3 = grille.(joueur.y / joueur.taille - 1).(joueur.x / joueur.taille + 1) in
					
					if ((bloc2 = bloc_)
							|| (bloc3 = bloc_)
							|| (bloc2 = demiBloc_)
							|| (bloc3 = demiBloc_)) then (
						
						joueur.y <- joueur.y - (joueur.y mod joueur.taille);
						joueur.vy <- 0;
						angle := (5. *. pi /. 4.);
						
						if (key_pressed()) then (
							
							if (read_key() = ' ') then (
								
								joueur.vy <- tempsSaut;
								
							);
							while key_pressed() do   (* Suppression des input "en trop" qui ferait sauter le bloc plusieurs fois *)
								let _ = read_key() in ()
							done;
						)
						
					) else if (bloc2 = air_ && bloc3 = air_) then (
						
						joueur.y <- joueur.y - gravite;
						
					) else if (bloc2 = picHaut_ || bloc3 = picHaut_ ) then (
						
						raise Mort;
						
					)
					
				) else (
					
					joueur.y <- joueur.y - gravite;
					
				);
				
				if (joueur.vy > 0) then (

					joueur.y <- joueur.y + joueur.vy;
					joueur.vy <- joueur.vy - 1;

					if (joueur.vy = 0) then (

						angle := (5. *. pi /. 4.);

					) else (

						angle := !angle -. pi /. (2. *. (float_of_int tempsSaut));

					);
				);
				
				joueur.x <- joueur.x + vitesse;
				
				draw_image img (-vitesse*(!i)) 0; 
				
				(*-------------------Partie rotation--------------------- *)
				joueur.yc <- float_of_int joueur.y +. 12.5;
				
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
				
				if (key_pressed()) then ( (* Un input ne peut être conservé que pendant un nombre d'images égal à conservationInput (ne fonctionne que pour un seul input à la fois) *)
					
					if ((!conserve) > 0) then (
						
						conserve := !conserve - 1;
						
					) else (
						
						conserve := conservationInput;
						let _ = read_key() in ();
						
					)
					
				);
				temps := Sys.time();
				i := (!i) + 1;
			
			with
			| Mort -> (
					let choix = wait_next_event[Key_pressed] in
					if (choix.key <> '\r') then ( (* Appuyer sur ENTRÉE pour quitter après une mort *)
						loop();
					) else (
						i := stop;
					);
			);

		);

	done;;


(* Éxécution du code *)

loop();;

