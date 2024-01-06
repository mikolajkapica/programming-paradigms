(* 
„Na stole ustawiono w rzędzie losy na loterię. Każdy z losów można odkryć, zyskując w ten 
sposób ukrytą pod nim nagrodę. Losy mogą kryć w sobie nagrody lub kolejne losy. Próba 
odkrycia odkrytego losu nie zmienia stanu na stole.” 
*)

type 'a los = unit -> 'a nagroda
and 'a nagroda = Nagroda of 'a | ListaLosow of 'a los list
and 'a odkyta_nagroda_lub_los = OdkytaNagroda of 'a | Los of 'a los
and 'a table = 'a odkyta_nagroda_lub_los list

(* let buyTicket i (tab: 'a table): 'a table = 
  let rec aux tab i acc = 
    match tab with
    | [] -> List.rev acc
    | h::t -> (
      if i <> 0 then aux t (i-1) (h::acc)
      else
      match h with
      | OdkytaNagroda s -> List.rev acc @ tab
      | Los f -> match f () with
        | Nagroda s -> List.rev acc @ (OdkytaNagroda s :: t)
        | ListaLosow l -> List.rev acc @ (List.map (fun x -> Los x) l) @ t
    ) in
    aux tab i [] *)


let rec buyTicket i tab =
  match (i, tab) with
  | 0, (OdkytaNagroda _ :: t) -> tab
  | 0, (Los f :: t) -> (
    match f () with
    | Nagroda s -> OdkytaNagroda s :: t
    | ListaLosow l -> List.map (fun x -> Los x) l @ t
  )
  | _, [] -> []
  | _, (h::t) -> h :: buyTicket (i-1) t

let rec list_for_all f l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | (h1::t1), (h2::t2) -> f h1 h2 && list_for_all f t1 t2


let rec (<=>) f1 f2 =
  match f1 (), f2 () with
  | (Nagroda s1, Nagroda s2) -> s1 = s2
  | (ListaLosow l1, ListaLosow l2) -> list_for_all (<=>) l1 l2
  | _ -> false


let rec (=?=) tab1 tab2 = 
  match (tab1, tab2) with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | (OdkytaNagroda s1 :: t1), (OdkytaNagroda s2 :: t2) -> s1 = s2 && t1 =?= t2
  | (Los f1 :: t1), (Los f2 :: t2) -> (
    match (f1 (), f2 ()) with
    | (Nagroda s1, Nagroda s2) -> s1 = s2 && t1 =?= t2
    | (ListaLosow l1, ListaLosow l2) -> list_for_all (<=>) l1 l2 && t1 =?= t2
    | _ -> false
  )
  | _ -> false
  


let test = 
  let table: string table = [Los (fun () -> Nagroda "komputer"); 
                             Los (fun () -> Nagroda "telefon"); 
                             Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                         (fun () -> Nagroda "klawiatura")]);
                             Los (fun () -> Nagroda "myszka");
                             Los (fun () -> ListaLosow [(fun () -> Nagroda "karta graficzna")])
                            ]
  in 
  let test1 = (buyTicket 0 table) =?= [OdkytaNagroda "komputer"; 
                                   Los (fun () -> Nagroda "telefon"); 
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                               (fun () -> Nagroda "klawiatura")]);
                                   Los (fun () -> Nagroda "myszka");
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "karta graficzna")])
                                  ]
  and test2 = buyTicket 1 table =?= [Los (fun () -> Nagroda "komputer");
                                   OdkytaNagroda "telefon"; 
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                               (fun () -> Nagroda "klawiatura")]);
                                   Los (fun () -> Nagroda "myszka");
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "karta graficzna")])
                                  ]
  and test3 = buyTicket 2 table =?= [Los (fun () -> Nagroda "komputer"); 
                                    Los (fun () -> Nagroda "telefon"); 
                                    Los (fun () -> Nagroda "myszka");
                                    Los (fun () -> Nagroda "klawiatura");
                                    Los (fun () -> Nagroda "myszka");
                                    Los (fun () -> ListaLosow [(fun () -> Nagroda "karta graficzna")])
                                  ] 
  and test4 = buyTicket 3 table =?= [Los (fun () -> Nagroda "komputer"); 
                                   Los (fun () -> Nagroda "telefon"); 
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                               (fun () -> Nagroda "klawiatura")]);
                                   OdkytaNagroda "myszka";
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "karta graficzna")])
                                  ]
  and test5 = buyTicket 4 table =?= [Los (fun () -> Nagroda "komputer"); 
                                   Los (fun () -> Nagroda "telefon"); 
                                   Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                               (fun () -> Nagroda "klawiatura")]);
                                   Los (fun () -> Nagroda "myszka");
                                   Los (fun () -> Nagroda "karta graficzna")
                                  ]
  and test6 = buyTicket 4 [] =?= []
  and test7 = buyTicket 4 table |> buyTicket 4 =?= [Los (fun () -> Nagroda "komputer"); 
                                                  Los (fun () -> Nagroda "telefon"); 
                                                  Los (fun () -> ListaLosow [(fun () -> Nagroda "myszka"); 
                                                                              (fun () -> Nagroda "klawiatura")]);
                                                  Los (fun () -> Nagroda "myszka");
                                                  OdkytaNagroda "karta graficzna"
                                                 ]
  in
  List.iter (fun x -> print_endline @@ string_of_bool x) [test1; test2; test3; test4; test5; test6; test7];

  