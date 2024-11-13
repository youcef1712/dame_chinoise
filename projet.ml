type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)

type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let _ = assert(indice_valide 4 3 = true) ;;

let est_case ((i,j,k):case):bool=
  (i+j+k) = 0;;


let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool = 
  (i>= -dim && j >= -dim && k >= -dim) || (i <= dim && j <= dim && k <= dim);;

let _ = assert(est_dans_etoile (3,0,-3) 3 = true) ;;
let _ = assert(est_dans_etoile (0,-4,4) 3 = false) ;;


let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
  (i > dim || i < -dim) || ((i >= -dim && j >= -dim && k >= -dim) && (i  <= dim && j <= dim && k <= dim));;           

let _ = assert(est_dans_losange (3,0,-3) 3 = true) ;;
let _ = assert(est_dans_losange (1,3,-4) 3 = false) ;;


let rec tourner_case (m:int) ((i,j,k):case):case=
  if m = 0 then (i, j, k)
  else tourner_case (m-1) (k*(-1), i*(-1), j*(-1));;

let _ = assert(tourner_case 2 (-6,3,3) = (3,3,-6)) ;;
let _ = assert(tourner_case 5 (3,0,-3) = (0,3,-3)) ;;



let translate ((c1, c2, c3):case) ((v1, v2, v3) : vecteur) : case = 
  (c1+v1, c2+v2, c3+v3);;

let diff_case (x,y,z:case)(x1,y1,z1:case):vecteur=
  (x-x1,y-y1,z-z1);; 


let sont_cases_voisines (x,y,z:case) (x1,y1,z1:case):bool=
  (x1,y1,z1)=(x,y+1,z-1) || (x1,y1,z1)=(x+1,y,z-1) || (x1,y1,z1)=(x+1,y-1,z)|| 
  (x1,y1,z1)=(x-1,y,z+1) || (x1,y1,z1)=(x-1,y+1,z) || (x1,y1,z1)=(x,y-1,z+1);;

let _ = assert(sont_cases_voisines (1,0,-1) (1,-1,0) = true) ;;
let _ = assert(sont_cases_voisines (1,0,-1) (2,-2,0) = false) ;;



type case_option = None | Some of case ;;


let calcul_pivot (x1,y1,z1) (x2,y2,z2):case_option=
  if (not(x1 = x2 || y1 = y2 || z1 = z2))
  then
    None
  else
  if (not((x1+x2) mod 2 = 0 && (y1+y2) mod 2 = 0 && (z1+z2) mod 2 = 0))
  then
    None
  else
    Some ((x1+x2)/2, (y1+y2)/2, (z1+z2)/2) ;;

let _ = assert(calcul_pivot (1,-2,1) (-3,2,1) = Some (-1,0,1)) ;;
let _ = assert(calcul_pivot (1,-2,1) (2,-2,0) = None) ;;
let _ = assert(calcul_pivot (0,0,0) (0,1,-1) = None) ;;



let vec_et_dist (x,y,z:case) (x1,y1,z1:case):vecteur*int=
  let (x2,y2,z2) = diff_case (x,y,z) (x1,y1,z1) in
  if x2>0 && y2<0 
  then 
    ((1,-1,0),abs(x))
  else
    if x2<0 && y2>0 
    then
      ((-1,1,0),abs(x))
    else 
      if z2>0 && y2<0 
      then
        ((0,-1,1),abs(y))
      else 
        if z2<0 && y2>0
        then 
          ((0,1,-1),abs(y))
        else 
          if x2>0 && z2<0
          then ((1,0,-1),abs(x))
          else ((-1,0,1),abs(x));;


let _ = assert(vec_et_dist (0,2,-2) (0,0,0) = ((0,1,-1),2)) ;;



let tourner_liste (l:'a list):'a list=
  match l with
  |[] -> []
  |pr::fin -> fin @ [pr] ;;

let testList:couleur list = Vert::Jaune::Rouge::[] ;; 
let _ = assert(tourner_liste testList = Jaune::Rouge::Vert::[]) ;;


let der_liste (l:'a list): 'a =
  let x = List.length l-1  in
  List.nth l x ;;

let _ = assert(der_liste testList = Rouge) ;;



let rec remplir_segment (m:int)(x,y,z:case):case list =
  if (m = 0) then []
  else [(x,y,z)] @ remplir_segment (m-1) (x,y+1,z-1) ;;

let _ = assert(remplir_segment 1 (0,0,0) = [(0,0,0)]) ;;
let _ = assert(remplir_segment 3 (-4,1,3) = [(-4,1,3);(-4,2,2);(-4,3,1)]) ;;


let rec remplir_triangle_bas (m:int)(x,y,z:case):case list =
  if (m = 0) then []
  else remplir_segment m (x,y,z) @ remplir_triangle_bas (m-1) (x+1,y,z-1) ;;

let _ = assert(remplir_triangle_bas 1 (0,0,0) = [(0,0,0)]) ;;
let _ = assert(remplir_triangle_bas 3 (-3,4,-1) = [(-3,4,-1);(-3,5,-2);(-3,6,-3);(-2,4,-2);(-2,5,-3);(-1,4,-3)]) ;;



let rec remplir_triangle_haut (m:int)(x,y,z:case):case list =
  if (m = 0) then []
  else remplir_segment m (x,y,z) @ remplir_triangle_haut (m-1) (x-1,y+1,z) ;;

let _ = assert(remplir_triangle_haut 1 (0,0,0) = [(0,0,0)]) ;;
let _ = assert(remplir_triangle_haut 3 (-4,1,3) = [(-4,1,3);(-4,2,2);(-4,3,1);(-5,2,3);(-5,3,2);(-6,3,3)]) ;;


let rec colorie (c:couleur)(l:case list):case_coloree list=
  match l with
  |[] -> []
  |pr::fin -> (pr,c)::colorie c fin ;;


let rec tourner_config_helper (m:int) (caseColoreeList:case_coloree list):case_coloree list =
  match caseColoreeList with
  |[] -> []
  |pr::fin -> let (case,couleur):case_coloree = pr in
    (tourner_case m case,couleur) :: tourner_config_helper m fin ;;

let tourner_config (caseColoreeList,couleurList,dim:configuration):configuration=
  let m:int = List.length(couleurList) in
    (tourner_config_helper (6/m) caseColoreeList, tourner_liste couleurList, dim) ;;


let rec remplir_init_helper (joueurs:couleur list) ((lcc,lc,dim):configuration) (m:int):configuration=
  match joueurs with 
  |[] -> (lcc,lc,dim)
  |pr::fin -> remplir_init_helper fin (tourner_config ((((colorie pr (remplir_triangle_haut dim (-(1 + dim),1,dim))) @ lcc),lc,dim))) m ;;



let remplir_init (joueurs:couleur list)(dim:dimension):configuration=
  remplir_init_helper joueurs ([], joueurs, dim) (List.length joueurs) ;;


let rec associe a l defaut=
  match l with
  | [] -> defaut
  | (a2, b) :: suite -> if a = a2 then b else associe a suite defaut;;



(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1);;


affiche (remplir_init testList 3) ;;

let _ = assert(associe (0,0,0) [((-1,0,1),Jaune);((0,0,0),Vert)]Libre = Vert) ;;
let _ = assert(associe (1,-1,0) [((-1,0,1),Jaune);((0,0,0),Vert)]Libre = Libre) ;;



let quelle_couleur (x,y,z:case)(co,_,_:configuration):couleur =
  associe (x,y,z) co Libre ;;

let _ = assert(quelle_couleur (-6,3,3) (remplir_init testList 3) = Vert) ;;
let _ = assert(quelle_couleur (0,0,0) (remplir_init testList 3) = Libre) ;;


let supprime_dans_config (lcc,lc,d:configuration) (x,y,z:case):configuration=
  let yo=quelle_couleur (x,y,z)(lcc,lc,d) in
    if yo <> Libre 
    then
      ((List.filter(fun ((a,c,d),e)->(a,c,d)<>(x,y,z)) lcc) ,lc,d)
    else 
      (lcc,lc,d);;

affiche (supprime_dans_config (remplir_init testList 3) (-6,3,3)) ;;






let est_coup_valide (lcc,lc,dim:configuration) (Du(c1,c2):coup):bool=
  let currentCouleur = List.hd lc in
    est_dans_losange c2 dim = true && quelle_couleur c2 (lcc,lc,dim) = Libre && quelle_couleur c1 (lcc,lc,dim) = currentCouleur ;;

let _ = assert(est_coup_valide (remplir_init testList 3) (Du((-4, 3, 1), (-3, 3, 0))) = true) ;;


let appliquer_coup ((lcc,lc,d):configuration) (Du(c1,c2):coup):configuration=
  let (lcc2,_,_) = supprime_dans_config (lcc,lc,d) c1 in
    let currentCouleur = List.hd lc in
      (((c2,currentCouleur) :: lcc2),lc,d) ;;
      


let rec est_libre_seg (x,y,z:case)(x1,y1,z1:case)(c,lc,d:configuration):bool=
  let ((v1,v2,v3),d)=vec_et_dist(x,y,z)(x1,y1,z1) in
    let vbn= quelle_couleur(x-v1 ,y-v2,z-v3)(c,lc,d)=Libre in 
      if (x-(2*v1)=x1 && v1<>0 )||(y-(2*v2)=y1 && v2<>0)||(z-(2*v3)=z1 && v3<>0) =true then
        quelle_couleur(x-v1,y-v2,z-v3)(c,lc,d)=Libre 
      else if vbn=false
      then false
      else est_libre_seg (x-v1,y-v2,z-v3)(x1,y1,z1)(c,lc,d);;

let _ = assert(est_libre_seg (-5,3,2) (0,3,-3) (remplir_init testList 3) = false) ;;
let _ = assert(est_libre_seg (-4,3,1) (1,3,-4) (remplir_init testList 3) = true) ;;


let est_saut (c1:case) (c2:case) (conf:configuration):bool=
  match sont_cases_voisines c1 c2 with
  |true   ->  false
  |false  ->  match calcul_pivot c1 c2 with
              |None       -> false
              |Some pivot -> ((est_libre_seg c1 pivot conf) && (est_libre_seg pivot c2 conf)) ;;
                
    
    
let rec est_saut_multiple (lcase:case list) (conf:configuration):bool=
  match lcase with
  |pr::pr2::[]  ->  est_saut pr pr2 conf
  |pr::pr2::fin ->  match est_saut pr pr2 conf with
                    |true   -> est_saut_multiple (pr2::fin) conf
                    |false  -> false ;;
  

    

let mettre_a_jour_configuration ((lcc,lc,d):configuration) (currentCoup:coup):configuration=
  match currentCoup with
  |Du(c1,c2)  ->  (match (est_coup_valide (lcc,lc,d) (Du(c1,c2))) && (sont_cases_voisines c1 c2) with
                  |true   ->  appliquer_coup (lcc,lc,d) (Du(c1,c2))
                  |false  ->  failwith "Ce coup n’est pas valide, le joueur doit rejouer")

  |Sm(pr::fin) ->  let (lcc2,_,_) = supprime_dans_config (lcc,lc,d) pr in
                    match (est_saut_multiple (pr::fin) (lcc2,lc,d)) && (est_coup_valide (lcc,lc,d) (Du(pr,der_liste fin))) with
                    |true   ->  appliquer_coup (lcc,lc,d) (Du(pr,der_liste fin))
                    |false  ->  failwith "Cette serie de coup n’est pas valide, le joueur doit rejouer" ;;



let score ((lcc,lc,d):configuration):int=
  let fun1 (a:int) (((i,j,k),c):case_coloree):int=
    if c = (List.hd lc)
    then a + i else a in
      List.fold_left fun1 0 lcc ;;
  


let score_gagnant (d:dimension):int=
  let fun1 (a:int) ((i,j,k):case):int=
    a + i in 
    List.fold_left fun1 0 (remplir_triangle_bas d (d+1, -3, -(d-2))) ;;
  

let gagne ((lcc,lc,d):configuration):bool=
  score (lcc,lc,d) = score_gagnant d ;;


let rec est_partie ((lcc,lc,d):configuration) (lcoup:coup list):couleur=
  if not (gagne (lcc,lc,d)) then
    match lcoup with
    |[] -> Libre
    |pr::fin -> est_partie (tourner_config (mettre_a_jour_configuration (lcc,lc,d) pr)) fin
  else List.hd lc ;;


let conf = ref (remplir_init testList 3) in
  affiche !conf ;

  let (lcc,lc,d) = !conf in
    List.hd lc ;

  conf := tourner_config (mettre_a_jour_configuration !conf (Du((-4, 3, 1), (-3, 3, 0)))) ;
 
  conf := tourner_config (mettre_a_jour_configuration !conf (Du((-4, 3, 1), (-3, 3, 0)))) ;


  conf := tourner_config (mettre_a_jour_configuration !conf (Du((-4, 3, 1), (-3, 3, 0)))) ;
  affiche !conf ;
  
  

