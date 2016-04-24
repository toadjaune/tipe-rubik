(*paramètres*)
let hauteur = 200
and largeur = 200;;

(*Fin des paramètres*)

(*Variables et constantes principales *)

type facette = ((float array array)*float);;
let zoom = 0.28 (*1/2sqrt3*) *. float_of_int (min hauteur largeur);;
let cube_ini=[|1;1;1;1;1;1;1;1;1;
               2;2;2;2;2;2;2;2;2;
               3;3;3;3;3;3;3;3;3;
               4;4;4;4;4;4;4;4;4;
               5;5;5;5;5;5;5;5;5;
               6;6;6;6;6;6;6;6;6|]
and pi = 3.141592654
and zbuf_ini = -.10.*.zoom;;
let cube = Array.make 54 0
and ecran= Array.make_matrix largeur hauteur [|0.;zbuf_ini|] (*zbuffer*)
and echantillonnage_affichage = int_of_float (0.88*.zoom)
and contour = 0.03 (* Largeur du contour, entre 0 et 1 *)
and angles_euler_ini = [|0.;-.pi/.4.;pi*.5./.4.+.pi/.16.|]
and angles_euler     = Array.make 3 0.
and delta_angles = pi/.8. (* pas des déplacements autour du cube *)
and sequence = ref [] (* mouvements a effectuer *)
and prev = ref [] (* mouvements effectués *);;
let images_mouvement = 8
and duree_mouvement = 0.5;; (* secondes *)

(*fin des variables et constantes *)

let modulo x n =(((x mod n)+n) mod n);;

let prod_mat m c =
  let n= Array.length m
  and p= Array.length c in
  let res = Array.make n 0. in
  for i = 0 to n-1 do
    for k = 0 to p-1 do
      res.(i) <- (res.(i) +. m.(i).(k) *. c.(k))
    done
  done;
  res
and add_tab t1 t2 =
  let res = Array.make (Array.length t1) 0. in
  for i=0 to (Array.length t1 -1) do
    res.(i) <- t1.(i) +. t2.(i)
  done;
  res
and sub_tab t1 t2 =
  let res = Array.make (Array.length t1) 0. in
  for i=0 to (Array.length t1 -1) do
    res.(i) <- t1.(i) -. t2.(i)
  done;
  res
and mul_tab t k =
  let res = Array.make (Array.length t) 0. in
  for i = 0 to (Array.length t -1) do
    res.(i) <- k *. t.(i)
  done;
  res;;

let reinitialiser_cube () = 
  sequence := [];
  prev := [];
  for i = 0 to 53 do
    cube.(i) <- cube_ini.(i)
  done;;
reinitialiser_cube ();;

let reinitialiser_euler () =
  for i = 0 to 2 do
    angles_euler.(i) <- angles_euler_ini.(i)
  done;;
reinitialiser_euler ();;

(* cube test
let cube = [|1;7;2;7;1;7;3;7;4;
	     1;7;2;7;2;7;3;7;4;
	     1;7;2;7;3;7;3;7;4;
	     1;7;2;7;4;7;3;7;4;
	     1;7;2;7;5;7;3;7;4;
	     1;7;2;7;6;7;3;7;4|];;
*)

(* #load "graphics.cma";;    nécéssaire pour WinCaml     *)
open Graphics;;
open_graph (" "^(string_of_int largeur)^"x"^(string_of_int hauteur));;
auto_synchronize false;;
let effgraph ()=
  clear_graph ();
  for i=0 to largeur-1 do
    for j=0 to hauteur-1 do
      ecran.(i).(j) <- [|0.;zbuf_ini|]
    done
  done;;
effgraph ();;




(*Coordonnées des points des carrés à tracer (affichage 2D)*)


(*Fonction d'affichage du patron*)
(*let patron v =
  let x=[|190;239;288;190;239;288;190;239;288;
          190;239;288;190;239;288;190;239;288;
          362;411;460;362;411;460;362;411;460;
          534;583;632;534;583;632;534;583;632;
          18;67;116;18;67;116;18;67;116;
          190;239;288;190;239;288;190;239;288|]
  and y=[|436;436;436;387;387;387;338;338;338;
          274;274;274;225;225;225;176;176;176;
          274;274;274;225;225;225;176;176;176;
          274;274;274;225;225;225;176;176;176;
          274;274;274;225;225;225;176;176;176;
          106;106;106;57;57;57;8;8;8|]  in
    for i = 0 to 53 do
      ( match true with 
	|_ when v.(i)=1 -> set_color white
	|_ when v.(i)=2 -> set_color blue
	|_ when v.(i)=3 -> set_color (Graphics.rgb 255 128 0)
	|_ when v.(i)=4 -> set_color green
	|_ when v.(i)=5 -> set_color red    
	|_ when v.(i)=6 -> set_color yellow 
	|_ when v.(i)=7 -> set_color black
	|_ -> failwith "numero de couleur inexistant"); 
      fill_rect (x.(i)) (y.(i)) 44 44
    done;;


(*Lancement*)
let initialisation_patron ()=
  open_graph " 700x500";
   let grille x y =
   ( set_line_width 5;
     set_color black;
     moveto x y;
     lineto (x+147) y;
     lineto (x+147) (y+147);
     lineto x (y+147);
     lineto x y;
     moveto (x+49) y;
     lineto (x+49) (y+147);
     moveto (x+98) y;
     lineto (x+98) (y+147);
     moveto x (y+49);
     lineto (x+147) (y+49);
     moveto x (y+98) ;
     lineto (x+147) (y+98) ) in
   grille 15 174;
   grille 187 174;
   grille 359 174;
   grille 531 174;
   grille 187 6;
   grille 187 336;
   patron cube_ini;;
initialisation_patron ();;    *)
(* Fin de l'affichage 2D*)


(*Mouvements basiques (6)*)

        
let mouvement f n =
  let v = cube in
  for i = 1 to (modulo n 4) do
    match f with
    |1 -> (let t1=v.(36) and t2=v.(37) and t3=v.(38) 
    and s1=v.(1)  and s2=v.(2) in
	   v.(36)<-v.(9)  ; v.(37)<-v.(10) ; v.(38)<-v.(11);
	   v.(9)<-v.(18)  ; v.(10)<-v.(19) ; v.(11)<-v.(20);
	   v.(18)<-v.(27) ; v.(19)<-v.(28) ; v.(20)<-v.(29);
	   v.(27)<-t1     ; v.(28)<-t2     ; v.(29)<-t3; 
	   v.(2)<-v.(0)   ; v.(1)<-v.(3)   ; v.(0)<-v.(6)  ; 
	   v.(3)<-v.(7)   ; v.(6)<-v.(8)   ; 
	   v.(7)<-v.(5)   ;
	   v.(5)<-s1      ; v.(8)<-s2 )
      
      
    |2 -> (let t1=v.(6) and t2=v.(7) and t3=v.(8) 
    and s1=v.(10)  and s2=v.(11) in
	   v.(6)<-v.(44)  ; v.(7)<-v.(41)  ; v.(8)<-v.(38) ;
	   v.(44)<-v.(47) ; v.(41)<-v.(46) ; v.(38)<-v.(45);
	   v.(47)<-v.(18) ; v.(46)<-v.(21) ; v.(45)<-v.(24);
	   v.(18)<-t1     ; v.(21)<-t2     ; v.(24)<-t3; 
	   v.(11)<-v.(9)  ; v.(10)<-v.(12) ; v.(9)<-v.(15) ; 
	   v.(12)<-v.(16) ; v.(15)<-v.(17) ; 
	   v.(16)<-v.(14) ;
	   v.(14)<-s1     ; v.(17)<-s2 )
      
    |3 -> (let t1=v.(8) and t2=v.(5) and t3=v.(2) 
    and s1=v.(19)  and s2=v.(20) in
	   v.(8)<-v.(17)  ; v.(5)<-v.(14)  ; v.(2)<-v.(11) ;
	   v.(17)<-v.(53) ; v.(14)<-v.(50) ; v.(11)<-v.(47);
	   v.(53)<-v.(27) ; v.(50)<-v.(30) ; v.(47)<-v.(33);
	   v.(27)<-t1     ; v.(30)<-t2     ; v.(33)<-t3; 
	   v.(20)<-v.(18) ; v.(19)<-v.(21) ; v.(18)<-v.(24) ; 
	   v.(21)<-v.(25) ; v.(24)<-v.(26) ; 
	   v.(25)<-v.(23) ;
	   v.(23)<-s1     ; v.(26)<-s2 )
      
    |4 -> (let t1=v.(53) and t2=v.(52) and t3=v.(51) 
    and s1=v.(28)  and s2=v.(29) in
	   v.(53)<-v.(42) ; v.(52)<-v.(39) ; v.(51)<-v.(36);
	   v.(42)<-v.(0)  ; v.(39)<-v.(1)  ; v.(36)<-v.(2) ;
	   v.(0)<-v.(20)  ; v.(1)<-v.(23)  ; v.(2)<-v.(26) ;
	   v.(20)<-t1     ; v.(23)<-t2     ; v.(26)<-t3; 
	   v.(29)<-v.(27) ; v.(28)<-v.(30) ; v.(27)<-v.(33); 
	   v.(30)<-v.(34) ; v.(33)<-v.(35) ; 
	   v.(34)<-v.(32) ;
	   v.(32)<-s1     ; v.(35)<-s2 )
      
    |5 -> (let t1=v.(29) and t2=v.(32) and t3=v.(35) 
    and s1=v.(37)  and s2=v.(38) in
	   v.(29)<-v.(51) ; v.(32)<-v.(48) ; v.(35)<-v.(45);
	   v.(51)<-v.(15) ; v.(48)<-v.(12) ; v.(45)<-v.(9) ;
	   v.(15)<-v.(6)  ; v.(12)<-v.(3)  ; v.(9)<-v.(0)  ;
	   v.(6)<-t1      ; v.(3)<-t2      ; v.(0)<-t3; 
	   v.(38)<-v.(36) ; v.(37)<-v.(39) ; v.(36)<-v.(42); 
	   v.(39)<-v.(43) ; v.(42)<-v.(44) ; 
	   v.(43)<-v.(41) ;
	   v.(41)<-s1     ; v.(44)<-s2 )		
      
      
    |6 -> (let t1=v.(42) and t2=v.(43) and t3=v.(44) 
    and s1=v.(46)  and s2=v.(47) in
	   v.(42)<-v.(33) ; v.(43)<-v.(34) ; v.(44)<-v.(35);
	   v.(33)<-v.(24) ; v.(34)<-v.(25) ; v.(35)<-v.(26);
	   v.(24)<-v.(15) ; v.(25)<-v.(16) ; v.(26)<-v.(17);
	   v.(15)<-t1     ; v.(16)<-t2     ; v.(17)<-t3; 
	   v.(47)<-v.(45) ; v.(46)<-v.(48) ; v.(45)<-v.(51); 
	   v.(48)<-v.(52) ; v.(51)<-v.(53) ; 
	   v.(52)<-v.(50) ;
	   v.(50)<-s1     ; v.(53)<-s2 )
      
    |_ -> failwith "Erreur : numéro de face"
      
  done;;
  
let test_fin v=
  let res=ref true in
  for i=0 to 53 do
    if cube.(i)<> cube_ini.(i) then res:=false
  done;
  !res
and melange () = 
  sequence := [];
  prev := [];
  for i=0 to 500 do
    let n = Random.int 6
    and m = Random.int 3 in
    mouvement (1+n) (1+m)
  done;;


(* fonctions graphiques *)

let change_couleur c = match true with 
	|_ when c=1. -> set_color white
	|_ when c=2. -> set_color blue
	|_ when c=3. -> set_color (Graphics.rgb 255 128 0)
	|_ when c=4. -> set_color green
	|_ when c=5. -> set_color red    
	|_ when c=6. -> set_color yellow 
	|_ when c=7. -> set_color black
	|_ -> failwith "numero de couleur inexistant";;

let affiche_point x_f y_f z couleur =
  let x=int_of_float x_f
  and y=int_of_float y_f in
    if ecran.(x).(y).(1)<=z then
      (
	change_couleur couleur;
	ecran.(x).(y)<-[|couleur;z|];
	plot x y
      );;

let affiche_facette (facette:facette) = 
  let extraire_couleur = function |_,c->c
  and extraire_tableau = function |t,_->t in
  let tableau = extraire_tableau facette 
  and couleur = extraire_couleur facette in
  let ech = echantillonnage_affichage * (if couleur = 7. then 3 else 1) in (*Gestion de l'exception des deux facettes noires pendant l'animation *)
  let pas_i_x = zoom *. (tableau.(1).(0) -. tableau.(0).(0)) /. (float_of_int ech)
  and pas_i_y = zoom *. (tableau.(1).(1) -. tableau.(0).(1)) /. (float_of_int ech)
  and pas_i_z = zoom *. (tableau.(1).(2) -. tableau.(0).(2)) /. (float_of_int ech)
  and pas_j_x = zoom *. (tableau.(2).(0) -. tableau.(0).(0)) /. (float_of_int ech)
  and pas_j_y = zoom *. (tableau.(2).(1) -. tableau.(0).(1)) /. (float_of_int ech)
  and pas_j_z = zoom *. (tableau.(2).(2) -. tableau.(0).(2)) /. (float_of_int ech)
  and origine_x = tableau.(0).(0) *. zoom +. (float_of_int (largeur/2))
  and origine_y = tableau.(0).(1) *. zoom +. (float_of_int (hauteur/2))
  and origine_z = tableau.(0).(2) *. zoom 
  and cont = int_of_float (contour*.(float_of_int ech)) in
    for i=0 to ech do
      for j=0 to ech do
	affiche_point (origine_x +. (float_of_int i)*.pas_i_x +. (float_of_int j)*.pas_j_x)
	              (origine_y +. (float_of_int i)*.pas_i_y +. (float_of_int j)*.pas_j_y)
	              (origine_z +. (float_of_int i)*.pas_i_z +. (float_of_int j)*.pas_j_z)
	              (if i<=cont or i>=(ech-cont) or j<=cont or j>=(ech-cont) then 7. else couleur)
      done;
      done;;

let affiche_tableau tableau =
  for i=0 to (Array.length tableau -1) do
    affiche_facette tableau.(i)
  done;;

let calcule_euler ()=
  let t=angles_euler in
  let passage = 
    [|
      [|  (cos t.(0))*.(cos t.(2))-.(sin t.(0))*.(cos t.(1))*.(sin t.(2));
	-.(cos t.(0))*.(sin t.(2))-.(sin t.(0))*.(cos t.(1))*.(cos t.(2));
	  (sin t.(0))*.(sin t.(1))
      |];
      [|  (sin t.(0))*.(cos t.(2))+.(cos t.(0))*.(cos t.(1))*.(sin t.(2));
	-.(sin t.(0))*.(sin t.(2))+.(cos t.(0))*.(cos t.(1))*.(cos t.(2));
	-.(cos t.(0))*.(sin t.(1))|];
      [|  (sin t.(1))*.(sin t.(2));
	  (sin t.(1))*.(cos t.(2));
	  cos t.(1)
      |]
    |]in
  [|
    prod_mat passage [| 1.;-1.; 1.|];
    prod_mat passage [| 1.; 1.; 1.|];
    prod_mat passage [|-1.;-1.; 1.|];
    prod_mat passage [| 1.;-1.;-1.|];
  |]
;;

let calcule_cube () =
  let facette_nulle = (Array.make_matrix 3 3 0.),0. in
  let tableau = Array.make 54 facette_nulle
  and euler = calcule_euler () in
  let aux face points = (* Génère les 9 facettes d'une face *) (
    let pas_i_x = (points.(1).(0)-.points.(0).(0))/.3.
    and pas_i_y = (points.(1).(1)-.points.(0).(1))/.3.
    and pas_i_z = (points.(1).(2)-.points.(0).(2))/.3.
    and pas_j_x = (points.(2).(0)-.points.(0).(0))/.3.
    and pas_j_y = (points.(2).(1)-.points.(0).(1))/.3.
    and pas_j_z = (points.(2).(2)-.points.(0).(2))/.3. in
    for i=0 to 2 do
      for j=0 to 2 do 
	let num = 9*(face-1)+i+(6-j*3) in
	tableau.(num)<-
	  [|
	    [|points.(0).(0) +. (float_of_int i)*.pas_i_x +. (float_of_int j)*.pas_j_x;
	      points.(0).(1) +. (float_of_int i)*.pas_i_y +. (float_of_int j)*.pas_j_y;
	      points.(0).(2) +. (float_of_int i)*.pas_i_z +. (float_of_int j)*.pas_j_z
	    |];
	    [|points.(0).(0) +. (float_of_int (i+1))*.pas_i_x +. (float_of_int j)*.pas_j_x;
	      points.(0).(1) +. (float_of_int (i+1))*.pas_i_y +. (float_of_int j)*.pas_j_y;
	      points.(0).(2) +. (float_of_int (i+1))*.pas_i_z +. (float_of_int j)*.pas_j_z
	    |];
	    [|points.(0).(0) +. (float_of_int i)*.pas_i_x +. (float_of_int (j+1))*.pas_j_x;
	      points.(0).(1) +. (float_of_int i)*.pas_i_y +. (float_of_int (j+1))*.pas_j_y;
	      points.(0).(2) +. (float_of_int i)*.pas_i_z +. (float_of_int (j+1))*.pas_j_z
	    |]
	  |]
	  ,(float_of_int (cube.(num)))   
      done
    done 
  ) in 
  (
    aux 1 [|euler.(0);euler.(1);euler.(2)|];
    aux 2 [|euler.(3);
	    [|-.euler.(2).(0);-.euler.(2).(1);-.euler.(2).(2)|];
	    euler.(0)|];
    aux 3 [|
      [|-.euler.(2).(0);-.euler.(2).(1);-.euler.(2).(2)|];
      [|-.euler.(0).(0);-.euler.(0).(1);-.euler.(0).(2)|];
      euler.(1)|];
    aux 4[|
      [|-.euler.(0).(0);-.euler.(0).(1);-.euler.(0).(2)|];
      [|-.euler.(1).(0);-.euler.(1).(1);-.euler.(1).(2)|];
      [|-.euler.(3).(0);-.euler.(3).(1);-.euler.(3).(2)|]|];
    aux 5 [|
      [|-.euler.(1).(0);-.euler.(1).(1);-.euler.(1).(2)|];
      euler.(3);
      euler.(2)|];
    aux 6 [|
      [|-.euler.(1).(0);-.euler.(1).(1);-.euler.(1).(2)|];
      [|-.euler.(0).(0);-.euler.(0).(1);-.euler.(0).(2)|];
      euler.(3)|];
    tableau
  );;

let affiche_cube () =
  effgraph ();
  affiche_tableau (calcule_cube ());
  synchronize ();;

affiche_cube ();;


let rec animation face sens = 
  let aux face sens = 
    let pas = sens*.pi/.(float_of_int (2*images_mouvement))
    and tps_mvt = duree_mouvement/.(float_of_int images_mouvement)
    and t = calcule_cube () 
    and tab = function |t,_ -> t in 
    let statique = match face with 
      |1 -> [|t.(45);t.(46);t.(47);t.(48);t.(49);t.(50);t.(51);t.(52);t.(53);t.(12);t.(13);t.(14);t.(15);t.(16);t.(17);t.(21);t.(22);t.(23);t.(24);t.(25);t.(26);t.(30);t.(31);t.(32);t.(33);t.(34);t.(35);t.(39);t.(40);t.(41);t.(42);t.(43);t.(44);[|(tab t.(9)).(0);(tab t.(18)).(0);(tab t.(36)).(0)|],7.|]
      |2 -> [|t.(27);t.(28);t.(29);t.(30);t.(31);t.(32);t.(33);t.(34);t.(35);t.(0);t.(1);t.(2);t.(3);t.(4);t.(5);t.(19);t.(20);t.(22);t.(23);t.(25);t.(26);t.(48);t.(49);t.(50);t.(51);t.(52);t.(53);t.(36);t.(37);t.(39);t.(40);t.(42);t.(43);[|(tab t.(43)).(1);(tab t.(25)).(0);(tab t.(3)).(0)|],7.|]
      |3 -> [|t.(36);t.(37);t.(38);t.(39);t.(40);t.(41);t.(42);t.(43);t.(44);t.(0);t.(1);t.(3);t.(4);t.(6);t.(7);t.(9);t.(10);t.(12);t.(13);t.(15);t.(16);t.(45);t.(46);t.(48);t.(49);t.(51);t.(52);t.(28);t.(29);t.(31);t.(32);t.(34);t.(35);[|(tab t.(34)).(0);(tab t.(16)).(1);(tab t.(28)).(2)|],7.|]
      |4 -> [|t.(9);t.(10);t.(11);t.(12);t.(13);t.(14);t.(15);t.(16);t.(17);t.(3);t.(4);t.(5);t.(6);t.(7);t.(8);t.(18);t.(19);t.(21);t.(22);t.(24);t.(25);t.(45);t.(46);t.(47);t.(48);t.(49);t.(50);t.(37);t.(38);t.(40);t.(41);t.(43);t.(44);[|(tab t.(0)).(0);(tab t.(48)).(0);(tab t.(20)).(2)|],7.|]
      |5 -> [|t.(18);t.(19);t.(20);t.(21);t.(22);t.(23);t.(24);t.(25);t.(26);t.(1);t.(2);t.(4);t.(5);t.(7);t.(8);t.(10);t.(11);t.(13);t.(14);t.(16);t.(17);t.(46);t.(47);t.(49);t.(50);t.(52);t.(53);t.(27);t.(28);t.(30);t.(31);t.(33);t.(34);[|(tab t.(16)).(0);(tab t.(34)).(1);(tab t.(7)).(0)|],7.|]
      |6 -> [|t.(0);t.(1);t.(2);t.(3);t.(4);t.(5);t.(6);t.(7);t.(8);t.(9);t.(10);t.(11);t.(12);t.(13);t.(14);t.(18);t.(19);t.(20);t.(21);t.(22);t.(23);t.(27);t.(28);t.(29);t.(30);t.(31);t.(32);t.(36);t.(37);t.(38);t.(39);t.(40);t.(41);[|(tab t.(12)).(0);(tab t.(21)).(0);(tab t.(39)).(0)|],7.|]
      |_ -> failwith "Y'a que 6 faces --'"
    and mobile = match face with
      |1 -> [|t.(0);t.(1);t.(2);t.(3);t.(4);t.(5);t.(6);t.(7);t.(8);t.(9);t.(10);t.(11);t.(18);t.(19);t.(20);t.(27);t.(28);t.(29);t.(36);t.(37);t.(38);[|(tab t.(9)).(0);(tab t.(18)).(0);(tab t.(36)).(0)|],7.|]
      |2 -> [|t.(9);t.(10);t.(11);t.(12);t.(13);t.(14);t.(15);t.(16);t.(17);t.(6);t.(7);t.(8);t.(18);t.(21);t.(24);t.(45);t.(46);t.(47);t.(38);t.(41);t.(44);[|(tab t.(43)).(1);(tab t.(25)).(0);(tab t.(3)).(0)|],7.|]
      |3 -> [|t.(18);t.(19);t.(20);t.(21);t.(22);t.(23);t.(24);t.(25);t.(26);t.(2);t.(5);t.(8);t.(11);t.(14);t.(17);t.(47);t.(50);t.(53);t.(27);t.(30);t.(33);[|(tab t.(34)).(0);(tab t.(16)).(1);(tab t.(28)).(2)|],7.|]
      |4 -> [|t.(27);t.(28);t.(29);t.(30);t.(31);t.(32);t.(33);t.(34);t.(35);t.(0);t.(1);t.(2);t.(20);t.(23);t.(26);t.(51);t.(52);t.(53);t.(36);t.(39);t.(42);[|(tab t.(0)).(0);(tab t.(48)).(0);(tab t.(20)).(2)|],7.|]
      |5 -> [|t.(36);t.(37);t.(38);t.(39);t.(40);t.(41);t.(42);t.(43);t.(44);t.(0);t.(3);t.(6);t.(9);t.(12);t.(15);t.(45);t.(48);t.(51);t.(29);t.(32);t.(35);[|(tab t.(16)).(0);(tab t.(34)).(1);(tab t.(7)).(0)|],7.|]
      |6 -> [|t.(45);t.(46);t.(47);t.(48);t.(49);t.(50);t.(51);t.(52);t.(53);t.(15);t.(16);t.(17);t.(24);t.(25);t.(26);t.(33);t.(34);t.(35);t.(42);t.(43);t.(44);[|(tab t.(12)).(0);(tab t.(21)).(0);(tab t.(39)).(0)|],7.|]
      |_ -> failwith "Y'a que 6 faces --'" in
    let axe = (* produit vectoriel de deux vecteurs de la face mobile *)
      let t = tab mobile.(0) in
      let u = sub_tab t.(2) t.(0)
      and v = sub_tab t.(1) t.(0) in
      let axe2=[|u.(1)*.v.(2)-.u.(2)*.v.(1);
		 u.(2)*.v.(0)-.u.(0)*.v.(2);
		 u.(0)*.v.(1)-.u.(1)*.v.(0)|] in
      let invnorme = 1. /. (sqrt (axe2.(0)*.axe2.(0) +. axe2.(1)*.axe2.(1) +. axe2.(2)*.axe2.(2))) 
      in
      mul_tab axe2 invnorme 
    in 
    
    let mat_rot =
      let c  = cos pas
      and s  = sin pas in
      let c1 = 1.-.c in
      [|
	[|axe.(0)*.axe.(0)*.c1+.c;
	  axe.(0)*.axe.(1)*.c1-.axe.(2)*.s;
	  axe.(0)*.axe.(2)*.c1+.axe.(1)*.s|];
	[|axe.(0)*.axe.(1)*.c1+.axe.(2)*.s;
	  axe.(1)*.axe.(1)*.c1+.c;
	  axe.(1)*.axe.(2)*.c1-.axe.(0)*.s|];
	[|axe.(0)*.axe.(2)*.c1-.axe.(1)*.s;
	  axe.(1)*.axe.(2)*.c1+.axe.(0)*.s;
	  axe.(2)*.axe.(2)*.c1+.c|]
      |] 
    in
    
    for i = 1 to images_mouvement do
      let tps = (Sys.time ()) in
      (
	for j=0 to 21 do
	  mobile.(j) <- 
	    (
	      match mobile.(j) with 
	      |[|t1;t2;t3|],c -> [|prod_mat mat_rot t1;
				   prod_mat mat_rot t2;
				   prod_mat mat_rot t3|],c 
	      |_ -> failwith "rotation de la face"
	    )  ;
	done ;
	
	effgraph (); 
	affiche_tableau statique;
	affiche_tableau mobile; 
	synchronize ();
	while (Sys.time () < (tps +. tps_mvt)) do
	  ()
	done 
      )
    done 
  in 
  (match (modulo sens 4) with
  |0 -> ()
  |2 -> aux face 1.;
    mouvement face 1;
    animation face 1;
    mouvement face 2
  |1 -> aux face 1.
  |3 -> aux face (-1.)
  |_ -> failwith "modulo ...");
  mouvement face sens;
  affiche_cube () ;;

(* Fin des fonctions graphiques *)

















(* Algorithme de résolution *)

let resoudre () =

  if !sequence <> [] 
  then ()
  else 
    sequence := (

      let derniere_croix () =
	[0,0,0]


      in
      
      
      
      let deuxieme_couronne () =
	let algo i j = (* s=i-j *)
	  let s = modulo (i-j) 4 in
	  if s<>1 && s<>3 then failwith "faces non adjacentes"
	  else
	    [j,s,9;6,-s,9;j,-s,9;6,-s,9;i,-s,9;6,s,9;i,s,9]
	in
	match cube.(16),cube.(25),cube.(34),cube.(43),
	  cube.(46),cube.(50),cube.(52),cube.(48) with
	  |i,_,_,_, j,_,_,_ when i<>6 && j<>6 -> (6,j-4,9)::(algo i j)
	  |_,i,_,_, _,j,_,_ when i<>6 && j<>6 -> (6,j-5,9)::(algo i j)
	  |_,_,i,_, _,_,j,_ when i<>6 && j<>6 -> (6,j-6,9)::(algo i j)
	  |_,_,_,i, _,_,_,j when i<>6 && j<>6 -> (6,j-7,9)::(algo i j)
	  |_,_,_,_, _,_,_,_ -> match cube.(41),cube.(12),cube.(14),cube.(21),cube.(23),cube.(30),cube.(32),cube.(39) with
	    |a,b, _,_, _,_, _,_ when a<>5 or b<>2 -> algo 5 2
	    |_,_, a,b, _,_, _,_ when a<>2 or b<>3 -> algo 2 3
	    |_,_, _,_, a,b, _,_ when a<>3 or b<>4 -> algo 3 4
	    |_,_, _,_, _,_, a,b when a<>4 or b<>5 -> algo 4 5
	    |_,_, _,_, _,_, _,_ -> derniere_croix ()
      in



      let coins_premiere_face () =
    (* On teste les facettes en bas à droite et à gauche *)
	match cube.(17),cube.(26),cube.(35),cube.(44),cube.(15),cube.(24),cube.(33),cube.(42) with
	|1,_,_,_,_,p,_,_ -> [6,p  ,6;p,-1,6;6,1,6;p,1,6]
	|_,1,_,_,_,_,p,_ -> [6,p-1,6;p,-1,6;6,1,6;p,1,6]
	|_,_,1,_,_,_,_,p -> [6,p-2,6;p,-1,6;6,1,6;p,1,6]
	|_,_,_,1,p,_,_,_ -> [6,p-3,6;p,-1,6;6,1,6;p,1,6]
	|_,_,_,p,1,_,_,_ -> [6,p  ,6;p,1,6;6,-1,6;p,-1,6]
	|p,_,_,_,_,1,_,_ -> [6,p-1,6;p,1,6;6,-1,6;p,-1,6]
	|_,p,_,_,_,_,1,_ -> [6,p-2,6;p,1,6;6,-1,6;p,-1,6]
	|_,_,p,_,_,_,_,1 -> [6,p-3,6;p,1,6;6,-1,6;p,-1,6]
	|_,_,_,_,_,_,_,_ -> (* On teste les facettes en dessous *)
	  match cube.(47),cube.(53),cube.(51),cube.(45),cube.(24),cube.(33),cube.(42),cube.(15) with
	  |1,_,_,_,p,_,_,_ -> let q = 2 + (modulo (p-1) 4) in
			      [6,p-0-2,7;q,-1,7;6,2,7;q,1,7]
	  |_,1,_,_,_,p,_,_ -> let q = 2 + (modulo (p-1) 4) in
			      [6,p-1-2,7;q,-1,7;6,2,7;q,1,7]
	  |_,_,1,_,_,_,p,_ -> let q = 2 + (modulo (p-1) 4) in
			      [6,p-2-2,7;q,-1,7;6,2,7;q,1,7]
	  |_,_,_,1,_,_,_,p -> let q = 2 + (modulo (p-1) 4) in
			      [6,p-3-2,7;q,-1,7;6,2,7;q,1,7]
	  |_,_,_,_,_,_,_,_ -> (* On teste les emplacements des coins *)
	    match 
	      cube.(8),cube.(11),cube.(18),
	      cube.(2),cube.(20),cube.(27),
	      cube.(0),cube.(29),cube.(36),
	      cube.(6),cube.(38),cube.(9) with
	      |1,2,3, 1,3,4, 1,4,5, 1,5,2 -> deuxieme_couronne ()
	      |_,a,b, _,_,_, _,_,_, _,_,_ when a=1 or b=1 -> [2,1,8;6,1,8;2,-1,8]
	      |_,_,_, _,a,b, _,_,_, _,_,_ when a=1 or b=1 -> [3,1,8;6,1,8;3,-1,8]
	      |_,_,_, _,_,_, _,a,b, _,_,_ when a=1 or b=1 -> [4,1,8;6,1,8;4,-1,8]
	      |_,_,_, _,_,_, _,_,_, _,a,b when a=1 or b=1 -> [5,1,8;6,1,8;5,-1,8]
	      |_,_,_, _,_,_, _,_,_, _,_,_ -> failwith "coin defaillant"  
      in



      let couronner_premiere_croix () =
	match ( (* Nombre de facettes à la bonne place *)
	  (if cube.(10)=2 then 1 else 0)+
	    (if cube.(19)=3 then 1 else 0)+
	    (if cube.(28)=4 then 1 else 0)+
	    (if cube.(37)=5 then 1 else 0)) with
	|4 -> coins_premiere_face ()
	|0 -> [1,1,5]
	|1 -> [1,1,5]
	|2 -> let n=( (* numéros des 2 faces dont on doit échanger les facettes *)
		if      cube.(10)<>2 then 2
		else if cube.(19)<>3 then 3
		else                     4)in
	      let p=(
		if      cube.(37)<>5 then 5
		else if cube.(28)<>4 then 4
		else                     3)in
	      [n,2,5;6,p-n,5;p,2,5;6,n-p,5;n,2,5]
	|_ -> failwith "erreur: couronnage 1ere croix impossible"
      in



      let premiere_croix () = match cube.(7),cube.(5),cube.(1),cube.(3) with
	|1,1,1,1 -> couronner_premiere_croix ()
	|a,b,c,d -> let n=( 
		  (* numéro entre 0 et 3 qui désigne une face non en place *)
		      if      a <> 1 then 0
		      else if b <> 1 then 1
		      else if c <> 1 then 2 
		      else                3
		    ) 
		    in
		    match cube.(46),cube.(50),cube.(52),cube.(48) with 
                    (* on teste la face du bas *)
		    |1,_,_,_ -> [6,n  ,1;(n+2),2,1]
		    |_,1,_,_ -> [6,n-1,1;(n+2),2,1]
		    |_,_,1,_ -> [6,n-2,1;(n+2),2,1]
		    |_,_,_,1 -> [6,n-3,1;(n+2),2,1]
		    |_,_,_,_ -> (* On teste la 2ème couronne, facettes à droite de leur milieu *)
		      match cube.(14),cube.(23),cube.(32),cube.(41) with 
		      |1,_,_,_ -> [1,n-1,2;3,1,2]
		      |_,1,_,_ -> [1,n-2,2;4,1,2]
		      |_,_,1,_ -> [1,n-3,2;5,1,2]
		      |_,_,_,1 -> [1,n-4,2;2,1,2]
		      |_,_,_,_ -> (* On teste la 2ème couronne, facettes à gauche de leur milieu *)
			match cube.(12),cube.(21),cube.(30),cube.(39) with 
			|1,_,_,_ -> [1,n+1,3;5,-1,3]
			|_,1,_,_ -> [1,n  ,3;2,-1,3]
			|_,_,1,_ -> [1,n-1,3;3,-1,3]
			|_,_,_,1 -> [1,n-2,3;4,-1,3]
			|_,_,_,_ -> (* On teste 1ère et 3ème couronne *)
			  if      cube.(10)=1 or cube.(16)=1 then [2,1,4]
			  else if cube.(19)=1 or cube.(25)=1 then [3,1,4]
			  else if cube.(28)=1 or cube.(34)=1 then [4,1,4]
			  else if cube.(37)=1 or cube.(43)=1 then [5,1,4]
			  else failwith "erreur: 1ere croix impossible"
      in premiere_croix ()
    );;

(* Fin de l'algo de résolution *)


(* Boucle interactive *)

let pas_a_pas () = match !sequence with
  |(f,s,m)::q -> 
    (
      animation f s ;
      prev := (f,-s,m)::(!prev);
      sequence := q;


      print_int f;print_string ",";print_int s;print_string",";print_int m; print_newline ()



    ) 
  |_          -> failwith "sequence non valide"
and undo () = match !prev with
  |[] -> ()
  |(f,s,m)::q -> 
    (
      sequence := (f,-s,m)::(!sequence);
      prev     := q;
      animation f s
    );;
let resolution_complete ()=
  while 
    (resoudre ();
     match !sequence with
     |(_,_,0)::q -> false
     |_          -> true
    )do 
    pas_a_pas ()
  done
and tourne f =
  sequence := [f,(match read_key () with |'1'->1|'2'->2|'3'->3|_->0),-1];
  pas_a_pas ();;
    

let boucle () =
  while
    (
      match (read_key ()) with 
      |'\027' -> false (* Touche échap *)
      |c   -> 
	begin
	  match c with
	  |' '  -> resoudre (); pas_a_pas () (* espace *)
	  |'\r' -> resolution_complete ()    (* entrée *)
	  |'\b' -> undo ()                   (* backspace *)
	  |'l' -> reinitialiser_cube ()     
	  |'m' -> melange ()             
	  |'a'  -> reinitialiser_euler ()
	  |'1'  -> tourne 1
	  |'2'  -> tourne 2
	  |'3'  -> tourne 3
	  |'4'  -> tourne 4
	  |'5'  -> tourne 5
	  |'6'  -> tourne 6
	  |'d'  -> angles_euler.(2) <- angles_euler.(2) +. delta_angles
	  |'q'  -> angles_euler.(2) <- angles_euler.(2) -. delta_angles
	  |'z'  -> angles_euler.(1) <- angles_euler.(1) -. delta_angles
	  |'s'  -> angles_euler.(1) <- angles_euler.(1) +. delta_angles



	  |_    -> ()
	end;
	affiche_cube ();
	true
    ) do
    ()
  done ;;

boucle ();;
