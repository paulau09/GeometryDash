(* ------------ Ouverture des fichiers externes ---------------- *)

#use "topfind";;

#require "graphics";;

open Graphics;;

(* ----------- Ouverture de la fenêtre ---------------- *)

open_graph " 1000x500+150+150";;
set_window_title "Caml Dash";;
auto_synchronize false;;
display_mode false;;

(* ----------- Création du type carré pour le joueur ----------- *)

type carre = {mutable x : int; mutable y : int; mutable vy : int; couleur : color; taille : int; xc : float; mutable yc : float};;
type point = {mutable x : float; mutable y : float};;

let joueur = {x = 400; y = 25; vy = 0; couleur = 0xAAAA33; taille = 25; xc = 412.5; yc = 37.5};;

(* ---------- Définition des constantes --------------- *)

let hauteurNiveau = 20  (* Caractéristiques du niveau de base *)
and longueurNiveau = 610
and longueurNivPerso = 700
and tempsSaut = 17
and posAffx = 400   (* Position du joueur sur l'écran *)
and gravite = 6
and vitesse = 6
and conservationInput = 20  (* Nombre d'images pendant lesquelles un input est conservé *)
and rayon = sqrt(2.) *. (float_of_int joueur.taille /. 2.)  (* Rayon du cercle dans lequel est inscrit le joueur *)
and pi = 4. *. atan 1.
and couleurFond = transp
and couleurBloc = 0x111111
and bloc_ = int_of_char 'B'
and air_ = int_of_char ' '
and picHaut_ = int_of_char 'h'
and picBas_ = int_of_char 'b'
and demiBloc_ = int_of_char 'D';;

exception Mort;;
exception Fin;;
exception Quitte;;

(* ---------- Définition des fonctions --------------- *)


let file_to_byte_array fichier hauteur longueur base =
	
	let grille = Array.make_matrix hauteur longueur 0 in
	
	for i=0 to (hauteur-1) do
		
		for j=0 to (if (base) then (longueur+1) else longueur) do (* Le fichier niveau est écrit sous windows 
		et ne se lit donc pas de la même manière que le fichier perso écrit avec le programme *)
			let c = input_byte fichier in
			if ((c <> int_of_char '\n') && (c <> int_of_char '\r')) then
				grille.(i).(j) <- c
			
		done
		
	done;
	grille;;


(* Permet de lire les images stockées sous forme de texte comme l'arrière-plan *)
let text_image_to_image file =

    let hauteur = int_of_string (input_line file)
    and longueur = int_of_string (input_line file) in
    
    let arr = Array.make_matrix hauteur longueur 0 in

    for i=0 to longueur*hauteur-1 do
        arr.(i/longueur).(i mod longueur) <- int_of_string (input_line file);
    done;

    make_image arr;;


(* Permet de passer d'une grille lue dans le fichier à un tableau qui peut être utilisé dans make_image *)
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


let foi pt = 	(* convertit point -> int*int *)
    let arrondi f = int_of_float (f +. 0.5) in
	let a,b = arrondi pt.x, arrondi pt.y in
	a,b;;


(* les fonctions checkBloc regardent les blocs correspondants et lèvent une exception Mort le cas échéant *)
(* Les numérotations des blocs sont faites tout le long du programme selon un pavé numérique organisé de telle sorte
        789
        456
        123
    avec les coordonnées du joueur (son pixel en bas à gauche) qui se situent dans le bloc 5 *)
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


(* Permet d'afficher du texte à l'écran avec dx et dy qui représentent le décalage du centre du texte par rapport au centre de la fenêtre *)
let affiche_texte texte dx dy =
	let a,b = text_size texte in
	moveto ((size_x() - a)/2 + dx) ((size_y() - b)/2 + dy);
	draw_string texte;;


(*---------------Fonctions d'édition de niveau---------------*)


let fond = 0xEEEEEE
and noir = 0x333333
and decalage = 1;;


let tr1 = [|10,250; 50,220; 50,280|];;
let tr2 = [|990,250; 950,220; 950,280|];;
let c1 = 37, 250
and c2 = 963, 250;;

let dist pt1 pt2 = 
	let a,b = pt1
	and c,d = pt2 in
	sqrt ((float_of_int (c-a))**2. +. (float_of_int (d-b)));;


let file_to_char_array fichier =	(*Lit un fichier de jeu pour en extraire la grille correspondante et pouvoir écrire les nouveaux blocks*)
	
	let grille = Array.make_matrix hauteurNiveau longueurNivPerso ' ' in
	
	for i=0 to (hauteurNiveau-1) do
		
		for j=0 to (longueurNivPerso) do
			
			let c = char_of_int (input_byte fichier) in
			if ((c <> '\n') && (c <> '\r')) then
				grille.(i).(j) <- c
			
		done
		
	done;
	grille;;


let agrandir_char arr agrandissement =	(*Permet d'obtenir une grille à ensuite convertir en image*)
	
	let taille1 = Array.length arr in
	let taille2 = Array.length arr.(0) in
	let nouvArr = Array.make_matrix (taille1*agrandissement) (taille2*agrandissement) 0 in
	
	for i=0 to (taille1-1) do
		
		for j=0 to (taille2-1) do
			
			let valeur = int_of_char arr.(i).(j) in
			if (valeur = air_) then (
				
				for k=0 to (agrandissement-1) do
					for l=0 to (agrandissement-1) do
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- fond
					done
				done;
			
			) else if (valeur = bloc_) then (
				
				for k=0 to (agrandissement-1) do
					for l=0 to (agrandissement-1) do
						nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- noir
					done
				done;
				
			) else if (valeur = picHaut_) then (
				
				let centrey = agrandissement/2 + 1 in
				for k=0 to (agrandissement-1) do
					for l=0 to (agrandissement-1) do
						if (abs(centrey-l) > k) then (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- fond;
						) else (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- noir;
						)
					done
				done;

			) else if (valeur = picBas_) then (
				
				let centrey = agrandissement/2 + 1 in
				for k=0 to (agrandissement-1) do
					for l=0 to (agrandissement-1) do
						if (abs(centrey-l) >= (agrandissement - k)) then (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- fond;
						) else (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- noir;
						)
					done
				done;

			) else if (valeur = demiBloc_) then (
				
				for k=0 to (agrandissement-1) do
					for l=0 to (agrandissement-1) do
						if (k < agrandissement/2) then (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- noir;
						) else (
							nouvArr.(i*agrandissement+k).(j*agrandissement+l) <- fond;
						)
					done
				done;

			);
			
		done
		
	done;
	nouvArr;;


let decoupe grille longueur pos =	(*extrait de la grille de jeu la partie correspondant à ce qu'on veut afficher à l'écran*)
	let grille_d = Array.make_matrix hauteurNiveau longueur ' ' in
	for i=0 to hauteurNiveau - 1 do 
		for j=0 to longueur - 1 do 
			grille_d.(i).(j) <- grille.(i).(pos + j)
		done;
	done;
	grille_d;;


let deplacement grille pos = 	(*Permet d'afficher la grille déplacée vers la droite ou la gauche*)
	let grille_d = decoupe grille 36 pos in
	renverse grille_d;
	let grille_a = agrandir_char grille_d joueur.taille in
	let img = make_image grille_a in
	draw_image img 50 0;
	synchronize();;


let move_left grille pos = 	(*Gère l'action de la flèche de droite*)
	if !pos != 0 then (
		pos := !pos - decalage;
		deplacement grille (!pos);
	);;


let move_right grille pos = 	(*Gère l'action de la flèche de gauche*)
	if !pos != longueurNivPerso-37 then (
		pos := !pos + decalage;
		deplacement grille (!pos);
	);;


let rec affiche_bloc bloc x y = match bloc with		(*Remplit le bloc actuel correspondant dans la case cliquée*)
|	'B'-> (affiche_bloc 'e' x y; set_color noir; fill_rect (x - x mod joueur.taille) (y-y mod joueur.taille) joueur.taille joueur.taille)
|	'b'-> (affiche_bloc 'e' x y; set_color noir; fill_poly [|x-x mod joueur.taille,y-y mod joueur.taille + 25;x-x mod joueur.taille +24 ,y-y mod joueur.taille + 25;x-x mod joueur.taille +13 ,y-y mod joueur.taille+1|])
|	'h'-> (affiche_bloc 'e' x y; set_color noir; fill_poly [|x-x mod joueur.taille,y-y mod joueur.taille;x-x mod joueur.taille +24 ,y-y mod joueur.taille ;x-x mod joueur.taille +13 ,y-y mod joueur.taille + 24|])
|	'D'-> (affiche_bloc 'e' x y; set_color noir; fill_rect (x - x mod joueur.taille) (y-y mod joueur.taille + 13) joueur.taille 13)
|	'e'-> (set_color fond; fill_rect (x - x mod joueur.taille) (y-y mod joueur.taille) joueur.taille joueur.taille)
|	_ -> ();;
	
	
let enregistre grille = 	(*Enregistre la grille dans le fichier "niv_perso.txt" afin de jouer le niveau ou l'éditer plus tard*)
	let fichier = open_out_bin "niv_perso.txt" in 
	for i=0 to Array.length grille - 2 do 
		for j=0 to Array.length grille.(i) - 1 do 
			Printf.fprintf fichier "%c" grille.(i).(j)
		done;
		Printf.fprintf fichier "\r"
	done;
	for j=0 to Array.length grille.(hauteurNiveau-1) - 1 do 	(*Rajout de blocs sur la derniere ligne si la case était initialement vide afin de garder un niveau jouable*)
		if (grille.(hauteurNiveau-1).(j) <> ' ') then (Printf.fprintf fichier "%c" grille.(hauteurNiveau-1).(j)) else(Printf.fprintf fichier "B") 
	done;
	Printf.fprintf fichier "\r";
	close_out fichier;;


let chargeNiveau fichier = 		(*Ouvre le niveau dans le fichier et en extrait la grille de jeu*)
	let f = open_in_bin fichier in 
	let grille = file_to_char_array f in 
	grille;;


(* -------------- Ouverture du niveau ------------- *)


let niv1 = open_in_bin "niveau1.txt";;
let nivPerso = open_in_bin "niv_perso.txt";;
let background = open_in "background.txt";;

(*grilleB : niveau de base
grilleP : niveau personnalisable*)
let grilleB = file_to_byte_array niv1 hauteurNiveau longueurNiveau true;;
let grilleP = ref (file_to_byte_array nivPerso hauteurNiveau longueurNivPerso false);;

let bg = text_image_to_image background;;

close_in niv1;;
close_in nivPerso;;
close_in background;;

let affichageB = agrandir grilleB joueur.taille;;
let affichageP = ref (agrandir (!grilleP) joueur.taille);;

renverse grilleB;;   (* On renverse la grille pour qu'elle soit orientée de la même façon qu'un repère mathématique classique *)
renverse (!grilleP);;

let imgB = make_image affichageB;;
let imgP = ref (make_image (!affichageP));;


(* ------------- Boucle de jeu ------------- *)


let rec accueil () = 
	draw_image bg 0 0;
	set_color white;
	affiche_texte "Bienvenue sur" 0 70;
	affiche_texte "CAML DASH" 0 20;
	affiche_texte "A -> Niveau de base" 0 (-30);
	affiche_texte "Z -> Niveau personnalise" 0 (-60);
	affiche_texte "E -> Editeur de niveau" 0 (-90);
	synchronize();
	let doitChoisir = ref true in
	while (!doitChoisir) do
		let choix = wait_next_event[Key_pressed] in
		if (choix.key = '\r') then (
			doitChoisir := false;
		) else (							(*Gestion du choix entre les niveaux*)
			if (choix.key = 'a') then (
				loop grilleB imgB longueurNiveau;
				doitChoisir := false;
			) else if (choix.key = 'z') then (
				loop (!grilleP) (!imgP) longueurNivPerso;
				doitChoisir := false;
			) else if (choix.key = 'e') then (
				edition();
				doitChoisir := false;
			);
		)
	done;
    

and loop grille img longueur =

	joueur.y <- 25;     (* Réinitialisation des paramètres *)
	joueur.x <- posAffx;
	joueur.vy <- 0;
	
    (* Les 4 points représentent les 4 coins du carré du joueur que l'on utilisera pour la rotation du joueur lors de ses sauts *)
	let angle = ref (5. *. pi /. 4.) in
	let point1 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle}
	and point2 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc +. rayon *. sin !angle}
	and point3 = {x = joueur.xc -. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle}
	and point4 = {x = joueur.xc +. rayon *. cos !angle; y = joueur.yc -. rayon *. sin !angle} in
		
	set_color joueur.couleur;
		
	let conserve = ref conservationInput in
		
	let i = ref 0
	and temps = ref (Sys.time())
	and stop = (longueur*joueur.taille/vitesse) - (1000/vitesse) in 
	while (!i)<stop do

		if (Sys.time() -. (!temps) >= 0.016) then ( (* 60 FPS *)

			try

                (* Test pour voir si le joueur meurt ou s'il continue dans l'air *)
				checkBloc8 joueur grille;
				if (joueur.x mod joueur.taille >= joueur.taille - vitesse) then ( (* Si le joueur passe au bloc suivant *)
					checkBloc6 joueur grille;
					checkBloc9 joueur grille;
				);
				
				if (joueur.y mod joueur.taille < gravite) then ( (* On considère alors le joueur sur le bloc d'en dessous *)
					
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
							
							let key = read_key() in 
							if (key = ' ') then ( (* Le joueur appuie sur ESPACE pour sauter *)
								joueur.vy <- tempsSaut;
							) else if (key = '\r') then (
								raise Quitte;
							);
							while key_pressed() do   (* Suppression des input "en trop" qui feraient sauter le joueur plusieurs fois *)
								let _ = read_key() in ()
							done;

						)
						
					) else if (bloc2 = air_ && bloc3 = air_) then (     (* Le joueur peut alors tomber dans le bloc d'en dessous *)
						
						joueur.y <- joueur.y - gravite;
						
					) else if (bloc2 = picHaut_ || bloc3 = picHaut_ ) then (
						
						raise Mort;
						
					)
					
				) else ( (* Le joueur n'a alors pas d'appui et continue donc de chuter au sein du même bloc *)
					
					joueur.y <- joueur.y - gravite;
					
				);
				
				if (joueur.vy > 0) then ( (* Gestion des sauts *)

					joueur.y <- joueur.y + joueur.vy;
					joueur.vy <- joueur.vy - 1;

					if (joueur.vy = 0) then (
						angle := (5. *. pi /. 4.);
					) else (
						angle := !angle -. pi /. (2. *. (float_of_int tempsSaut));
					);
				);
				
				joueur.x <- joueur.x + vitesse;
				
                (* Affichage *)
                draw_image bg 0 0;
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
                    
                synchronize();

                (* Un input ne peut être conservé que pendant un nombre d'images égal à conservationInput (ne fonctionne que pour un seul input à la fois) *)
				if (key_pressed()) then (
					if ((!conserve) > 0) then (
						conserve := !conserve - 1;
					) else (
						conserve := conservationInput;
						let _ = read_key() in ();
					)
				);
				temps := Sys.time();
				i := (!i) + 1;
				if (!i = stop) then (
					raise Fin;
				);
			
			with
			| Mort -> (
				i := stop;
                let choix = wait_next_event[Key_pressed] in
                if (choix.key <> '\r') then ( (* Appuyer sur ENTRÉE pour quitter après une mort *)
                    loop grille img longueur;
                ) else (
                    accueil();
                );
			);
			| Fin -> (
				print_endline "fin";
                set_color joueur.couleur;
                affiche_texte "Bravo !" 0 60;
				affiche_texte "Vous avez fini le niveau" 0 20;
                affiche_texte "Appuyez sur ENTREE" 0 (-20);
				affiche_texte "pour retourner au menu" 0 (-60);
				synchronize();
                let choix = wait_next_event[Key_pressed] in
                if (choix.key <> '\r') then ( 	(* Appuyer sur ENTRÉE pour quitter après avoir fini le niveau *)
                    loop grille img longueur;
                ) else (
                    accueil();
                );
			);
			| Quitte -> (
				i := stop;
				accueil();
			);

		);

	done;


and edition () =
	set_color white;
	fill_rect 0 0 (2*joueur.taille) (size_y());
	fill_rect (size_x()-50) 0 (size_x()) (size_y());
	set_color black;
	fill_poly tr1;
	fill_poly tr2;
	let pos = ref 0
	and blocCourant = ref 'B'
	and touches = [|'B';'b';'h';'D';'e'|] in
	let grille = chargeNiveau "niv_perso.txt" in
	let grille_aff = agrandir_char (decoupe grille 36 (!pos)) joueur.taille in 
	let img = make_image grille_aff in
	draw_image img 50 0;
	synchronize();
	renverse grille;
	let continue = ref true in
	while (!continue) do		(*Boucle principale*)
		
		if (key_pressed()) then (		(*Gere le changement de bloc selectionné*)
			let key = read_key() in
			if Array.memq key touches then (
				blocCourant := key
			) else if (key = '\r') then (		(*Declenche l'enregistrement de la grille puis la fermeture de l'editeur de niveau*)
				renverse grille;
				enregistre grille;
				let nivPerso = open_in_bin "niv_perso.txt" in
				grilleP := file_to_byte_array nivPerso hauteurNiveau longueurNivPerso false;
				close_in nivPerso;
				affichageP := agrandir (!grilleP) joueur.taille;
				imgP := make_image (!affichageP);
				renverse (!grilleP);
				continue := false;
			);
		);

		if button_down() = true then (		
			let a,b = mouse_pos() in 
			if ((dist (a,b) c1) < 37.) then (move_left grille pos);		(*Detecte le clic des fleches de deplacement et deplace la grille*)
			if ((dist (a,b) c2) < 37.) then (move_right grille pos);
			if (a >= 50 && a < (size_x()-50) && b >= 0 && b < (size_y())) then (		(*Affiche le bloc correspondant à l'endroit cliqué et le stocke dans la grille*)
				if (!blocCourant <> 'e') then (grille.(b/25).(a/25 + (!pos) - 2) <- !blocCourant) else (grille.(b/25).(a/25 + (!pos) - 2) <- ' ');
				affiche_bloc !blocCourant a b;
				synchronize();
			);
		)

	done;
	accueil();;
	
	


(* Éxécution du code *)

accueil();;
close_graph();;
