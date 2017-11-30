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
  coq_supported : string option;
  suite : string;
}

module PkgVersionSet = Set.Make(struct
  type t = package_version
  let compare p1 p2 = String.compare p1.version p2.version
end)

let parse_args v =
  match Array.to_list v with
  | [] | [_] -> Printf.eprintf "archive2web template root1 root2..."; exit 1
  | template :: roots -> template, roots

let read_file fname = assert false
let strip_prefix prefix fullname = assert false
let extract_data opam_file = assert false 
let forall_subdirs base f = assert false

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

