type package_name = string

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type package_version = {
  date : string option;
  homepage : string option;
  keywords : StrSet.t;
  categories : StrSet.t;
  authors : StrSet.t;
  description : string;
  version : string;
  coq_supported : string list;
  suite : string;
}

module PkgVersionSet = Set.Make(struct
  type t = package_version
  let compare p1 p2 = String.compare p1.version p2.version
end)

let warning s = Printf.eprintf "W: %s\n%!" s 

let parse_args v =
  match Array.to_list v with
  | [] | [_] -> Printf.eprintf "archive2web template root1 root2..."; exit 1
  | template :: roots -> template, roots

let read_file fname =
  let ic = open_in fname in
  let b = Buffer.create 1024 in
  Buffer.add_channel b ic (in_channel_length ic);
  Buffer.contents b

let strip_prefix prefix fullname =
  let len_prefix = String.length prefix in
  String.sub fullname len_prefix (String.length fullname - len_prefix)

let forall_subdirs base f = Array.iter f (Sys.readdir base)

open OpamParserTypes

let has_tilde s = 
  let len = String.length s in
  if s.[len - 1] = '~' then String.sub s 0 (len-1), true
  else s, false

let digits v =
  let v, tilde = has_tilde v in
  try
    match String.split_on_char '.' v with
    | [] -> int_of_string v, 0, tilde
    | [x] -> int_of_string x, 0, tilde
    | x :: y :: _ -> int_of_string x, int_of_string y, tilde
  with Failure _ -> warning "incomplete version parsing"; 0,0,false

let coq_versions_between v1 (op : [ `Lt | `Leq ]) v2 =
  let open Printf in
  let d1, u1, _ = digits v1 in 
  let d2, u2, tilde = digits v2 in 
  let d2, u2 =
    if tilde then
      if u2 > 0 then d2, u2-1
      else if d2 > 0 then d2-1, u2
      else d2, u2
    else d2, u2 in
  let between = [] (* TODO *) in
  sprintf "%d.%d" d1 u1 :: between @ [sprintf "%d.%d" d2 u2]


let rec coq_version = function
  | Logop(_,`And, Prefix_relop(_,`Geq,String(_,v1)), 
                  Prefix_relop(_,((`Lt|`Leq) as op),String(_,v2))) ->
      coq_versions_between v1 op v2 
  | Logop(_,`Or,a,b) -> coq_version a @ coq_version b
  | Prefix_relop(_,`Eq,String(_,v)) -> [v]
  | Prefix_relop(_,`Geq,String(_,v)) -> [v ^ "+"]
  | _ -> []

let coq_range = function
  | Some (List(_,l)) ->
    let rec aux = function
      | Option(_, String(_,c), [Group(_,g)]) :: _ when c = "coq" ->
           List.(flatten (map coq_version g))
      | _ :: l -> aux l
      | [] -> []
    in aux l
  | _ -> warning "no depends variable in opam file"; [] 

let rec find_var_str name = function
  | [] -> None
  | Variable(_,n,String(_,v)) :: _ when name = n -> Some v 
  | _ :: xs -> find_var_str name xs

let rec find_var_str_list name = function
  | [] -> []
  | Variable(_,n,List(_,vl)) :: _ when name = n ->
       List.map (function String(_,s) -> s | _ -> assert false) vl
  | _ :: xs -> find_var_str_list name xs

let rec find_var name = function
  | [] -> None
  | Variable(_,n,v) :: _ when name = n -> Some v
  | _ :: xs -> find_var name xs

let has_prefix prefix s =
  let len_prefix = String.length prefix in
  let len = String.length s in
  if len > len_prefix && String.sub s 0 len_prefix = prefix then
    Some (String.sub s len_prefix (len - len_prefix))
  else None
 
let rec filtermap f = function
  | [] -> []
  | x :: xs ->
      match f x with Some x -> x :: filtermap f xs | _ -> filtermap f xs 

let extract_data opam_file =
  let homepage = find_var_str "homepage" opam_file in
  let tags = find_var_str_list "tags" opam_file in
  let date =
    match filtermap (has_prefix "date:") tags with
    | [] -> None
    | [x] -> Some x
    | x :: _ -> warning "multiple date tag"; Some x in
  let keywords = filtermap (has_prefix "keyword:") tags in
  let keywords = List.fold_right StrSet.add keywords StrSet.empty in
  let categories = filtermap (has_prefix "category:") tags in
  let categories = List.fold_right StrSet.add categories StrSet.empty in
  let authors = find_var_str_list "authors" opam_file in
  let authors = List.fold_right StrSet.add authors StrSet.empty in
  let coq_supported = coq_range (find_var "depends" opam_file) in
  date, homepage, keywords, categories, authors, coq_supported

let do_one_package_version pkgs root pname pdir =
  let version = strip_prefix (pname ^ ".") (Filename.basename pdir) in
  let { OpamParserTypes.file_contents } = OpamParser.file (pdir ^ "/opam") in
  let date, homepage, keywords, categories, authors, coq_supported =
    extract_data file_contents
  in
  let description = read_file (pdir ^ "/descr") in
  let p = {
    date;
    homepage;
    keywords;
    categories;
    authors;
    coq_supported;
    description;
    version;
    suite = root;
  } in
  let ps = try StrMap.find pname !pkgs with Not_found -> PkgVersionSet.empty in
  pkgs := StrMap.add pname (PkgVersionSet.add p ps) !pkgs
;;

let do_one_root pkgs root =
  forall_subdirs (root ^ "/packages/") (fun pname ->
    forall_subdirs (root ^ "/packages/" ^ pname ^ "/") (fun pdir ->
      do_one_package_version pkgs
        (Filename.basename root) (Filename.basename pname) pdir))
;;

let pkgversionset2html (pname, pvset) = assert false

let () =
  let template, roots = parse_args Sys.argv in
  let pkgs : PkgVersionSet.t StrMap.t ref = ref StrMap.empty in
  List.iter (do_one_root pkgs) roots;
  let output = 
    Str.global_replace (Str.regexp_string "@@PACKAGES@@")
      (String.concat "\n" (List.map pkgversionset2html (StrMap.bindings !pkgs)))
      (read_file template) in
  Printf.printf "%s" output
;;

