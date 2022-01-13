#use "topfind";;
#require "graphics";;

open Graphics;;
(*
deplacement de la grille
mettre l'image derriere -> compromis
ouvrir un niveau
*)

open_graph " 1000x500+150+150";;

let hauteurNiveau = 20 
and longueurMax = 700
and tailleBloc = 25
and pos = ref 0
and blocCourant = ref 'B'
and touches = [|'B';'b';'h';'D';'e'|];; (*e pour effacer*)

let fond = 0xEEEEEE
and noir = 0x333333
and decalage = 1
and niveau = "niv_perso.txt";;

(*Création des fleches de déplacement *)
let tr1 = [|10,250; 50,220; 50,280|];;
let tr2 = [|990,250; 950,220; 950,280|];;
let c1 = 37, 250
and c2 = 963, 250;;
(*let u1 = -50, 0
and v1 = -40, -30
and u2 = -50,0
and v2 = 40,-30;;*)
let bloc_ = int_of_char 'B'
and air_ = int_of_char ' '
and picHaut_ = int_of_char 'h'
and picBas_ = int_of_char 'b'
and demiBloc_ = int_of_char 'D';;

fill_poly tr1;;
fill_poly tr2;;
(*
type point = { x:int; y:int};;
let pdt_scal v1 v2 = 
	let a,b = v1 
	and c,d = v2 in a*c+b*d;;
*)
let dist pt1 pt2 = 
	let a,b = pt1
	and c,d = pt2 in
	sqrt ((float_of_int (c-a))**2. +. (float_of_int (d-b)));;

let file_to_byte_array fichier =
	
	let grille = Array.make_matrix hauteurNiveau longueurMax ' ' in
	
	for i=0 to (hauteurNiveau-1) do
		
		for j=0 to (longueurMax) do
			
			let c = char_of_int (input_byte fichier) in
			if ((c <> '\n') && (c <> '\r')) then
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

let decoupe grille longueur =
	let grille_d = Array.make_matrix hauteurNiveau longueur ' ' in
	for i=0 to hauteurNiveau - 1 do 
		for j=0 to longueur - 1 do 
			grille_d.(i).(j) <- grille.(i).((!pos) + j)
		done;
	done;
	grille_d;;


let renverse arr = 
	let long = Array.length arr and
	tmp = ref arr.(0) in
	for i = 0 to (long/2 -1) do 
		tmp := arr.(i);
		arr.(i) <- arr.(long-i-1);
		arr.(long-i-1) <- !tmp;
	done;;


let deplacement grille = 
	let grille_d = decoupe grille 36 in
	renverse grille_d;
	let grille_a = agrandir grille_d tailleBloc in
	
	let img = make_image grille_a in
	draw_image img 50 0;;


let move_left grille  = 
	if !pos != 0 then (
		pos := !pos - decalage;
		deplacement grille
	);;


let move_right grille = 
	if !pos != longueurMax-37 then (
		pos := !pos + decalage;
		deplacement grille
	);;


let affiche_niv grille = 
	fill_poly tr1;
	fill_poly tr2;
	let img = make_image grille in
	draw_image img 0 (-(!pos));;



let rec affiche_bloc bloc x y = match bloc with
|	'B'-> (affiche_bloc 'e' x y; set_color noir; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	'b'-> (affiche_bloc 'e' x y; set_color noir; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc + 25;x-x mod tailleBloc +24 ,y-y mod tailleBloc + 25;x-x mod tailleBloc +13 ,y-y mod tailleBloc+1|])
|	'h'-> (affiche_bloc 'e' x y; set_color noir; fill_poly [|x-x mod tailleBloc,y-y mod tailleBloc;x-x mod tailleBloc +24 ,y-y mod tailleBloc ;x-x mod tailleBloc +13 ,y-y mod tailleBloc + 24|])
|	'D'-> (affiche_bloc 'e' x y; set_color noir; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc + 13) tailleBloc 13)
|	'e'-> (set_color fond; fill_rect (x - x mod tailleBloc) (y-y mod tailleBloc) tailleBloc tailleBloc)
|	_ -> ();;

let enregistre grille = 
	let fichier = open_out_bin niveau in 
	for i=0 to Array.length grille - 2 do 
		for j=0 to Array.length grille.(i) - 1 do 
			Printf.fprintf fichier "%c" grille.(i).(j)
        done;
		Printf.fprintf fichier "\r"
    done;
	for j=0 to Array.length grille.(hauteurNiveau-1) - 1 do (*Rajout de blocs sur la derniere ligne si la case était initialement vide afin de garder un niveau jouable*)
		if (grille.(hauteurNiveau-1).(j) <> ' ') then (Printf.fprintf fichier "%c" grille.(hauteurNiveau-1).(j)) else(Printf.fprintf fichier "B") 
	done;
	Printf.fprintf fichier "\r";
    close_out fichier;;

let chargeNiveau fichier = 
    let f = open_in_bin fichier in 
    let grille = file_to_byte_array f in 
    grille;;

let grille = chargeNiveau niveau in
let grille_aff = agrandir (decoupe grille 36) tailleBloc in 
let img = make_image grille_aff in 
draw_image img 50 0;
renverse grille;
let toucheALire = ref true in
while (!toucheALire) do
    let a = read_key () in
    Printf.printf "%d\n" (int_of_char a)
done;
let continue = ref true in
while (!continue) do
    
	if key_pressed() = true then (
		let key = read_key() in
		if Array.memq key touches then (
            blocCourant := key
		) else if (key = '\t') then (
            renverse grille;
            enregistre grille;
            Printf.printf "Out\n";
            continue := false;
        );
	);

	if button_down() = true then (
		let a,b = mouse_pos() in 
		(*if (a<=50) then	*)						(*Gerer triangles*)
		if ((dist (a,b) c1) < 37.) then (move_left grille);
		if ((dist (a,b) c2) < 37.) then (move_right grille);
		if (a >= 50 && a <= (size_x()-50) && b >= 0 && b < (size_y())) then (			(*Gerer blocs*)
			if (!blocCourant <> 'e') then (grille.(b/25).(a/25 + !pos) <- !blocCourant) else (grille.(b/25).(a/25 + !pos) <- ' ');
			affiche_bloc !blocCourant a b;
		);
	)

done;;
while (key_pressed()) do
    let _ = read_key() in ()
done;;
