(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let descr_template =
  OpamFile.Descr.of_string "Short description\n\nLong\ndescription\n"

let () =
  OpamHTTP.register ()

let mkwarn () =
  let warnings = ref ([]: string list) in
  (fun s -> warnings := s::!warnings),
  (fun file -> match !warnings with
     | [] -> true
     | w ->
       OpamGlobals.error "In %s:\n  - %s\n"
         (OpamFilename.to_string file)
         (String.concat "\n  - " (List.rev w));
       false)

let check_opam file =
  let module OF = OpamFile.OPAM in
  try
    let _opam = OF.read file in
    (* let warn, warnings = mkwarn () in *)
    (* Add factored-out todo "opam-lint" here *)
    (* warnings file *)
    true
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false

let check_descr file =
  let module OF = OpamFile.Descr in
  try
    let descr = OF.read file in
    let warn, warnings = mkwarn () in
    if OF.synopsis descr = OF.synopsis descr_template ||
       OpamMisc.strip (OF.synopsis descr) = "" then
      warn "short description unspecified";
    if OF.body descr = OF.body descr_template ||
       OpamMisc.strip (OF.body descr) = "" then
      warn "long description unspecified";
    warnings file
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false

let check_url file =
  let module OF = OpamFile.URL in
  try
    let url = OF.read file in
    let warn, warnings = mkwarn () in
    let checksum = OF.checksum url in
    if checksum = None then warn "no checksum supplied";
    let check_url address =
      let addr,kind = OpamTypesBase.parse_url address in
      if snd address <> None || kind <> `http then
        warn (Printf.sprintf "%s is not a regular http or ftp address"
                (OpamTypesBase.string_of_address addr))
      else
        OpamFilename.with_tmp_dir @@ fun tmpdir ->
        let name =
          OpamPackage.of_string
            (Filename.basename (OpamTypesBase.string_of_address address))
        in
        let archive = OpamRepository.pull_url kind name tmpdir None [address] in
        match archive with
        | Not_available s ->
          warn (Printf.sprintf "%s couldn't be fetched (%s)"
                  (OpamTypesBase.string_of_address address)
                  s)
        | Result (F f) ->
          if checksum <> None && Some (OpamFilename.digest f) <> checksum then
            warn (Printf.sprintf "bad checksum for %s"
                    (OpamTypesBase.string_of_address address))
        | _ -> assert false
    in
    List.iter check_url (OF.url url :: OF.mirrors url);
    warnings file
  with e ->
    OpamMisc.fatal e;
    OpamGlobals.error "Couldn't read %s" (OpamFilename.to_string file);
    false


(* -- Prepare command -- *)

let prepare ?name ?version http_url =
  let open OpamFilename.OP in
  OpamFilename.with_tmp_dir (fun tmpdir ->
      (* Fetch the archive *)
      let url = (http_url,None) in
      let f =
        OpamRepository.pull_url `http
          (OpamPackage.of_string (Filename.basename http_url)) tmpdir None
          [url]
      in
      let archive = match f with
        | Not_available s ->
          OpamGlobals.error_and_exit "%s is not available: %s" http_url s
        | Result (F file) -> file
        | _ -> assert false
      in
      let checksum = List.hd (OpamFilename.checksum archive) in
      let srcdir = tmpdir / "src" in
      OpamFilename.extract archive srcdir;
      (* Gather metadata *)
      let meta_dir =
        if OpamFilename.exists_dir (srcdir / "opam")
        then srcdir / "opam"
        else srcdir
      in
      let src_opam =
        if OpamFilename.exists (meta_dir // "opam")
        then OpamFile.OPAM.read (meta_dir // "opam")
        else OpamGlobals.error_and_exit "No metadata found"
      in
      let name = match name, OpamFile.OPAM.name_opt src_opam with
        | None, None ->
          OpamGlobals.error_and_exit "Package name unspecified"
        | Some n1, Some n2 when n1 <> n2 ->
          OpamGlobals.warning
            "Publishing as package %s, while it refers to itself as %s"
            (OpamPackage.Name.to_string n1) (OpamPackage.Name.to_string n2);
          n1
        | Some n, _ | None, Some n -> n
      in
      let version = match version, OpamFile.OPAM.version_opt src_opam with
        | Some v, _ | None, Some v -> v
        | _ ->
          OpamGlobals.error_and_exit "Package version unspecified"
      in
      let package = OpamPackage.create name version in
      let src_descr =
        if OpamFilename.exists (meta_dir // "descr")
        then OpamFile.Descr.read (meta_dir // "descr")
        else descr_template
      in
      (* TODO: add data from the repo if found, take the best of the two.
         Use data from existing prepare_dir if specified instead of URL ?
         Just update url file in prepare_dir if both specified ? *)
      (* Fix and generate missing metadata *)
      let prep_url =
        OpamFile.URL.with_checksum (OpamFile.URL.create `http url) checksum
      in
      let prep_opam = OpamFile.OPAM.with_name_opt src_opam None in
      let prep_opam = OpamFile.OPAM.with_version_opt prep_opam None in
      (* Generate prepare dir *)
      let prepare_dir =
        OpamFilename.cwd () / OpamPackage.to_string package
      in
      if OpamFilename.exists_dir prepare_dir &&
         not (OpamGlobals.confirm
                "%s exists. Override contents \
                 (otherwise, only url will be updated) ?"
                (OpamFilename.Dir.to_string prepare_dir))
      then () else (
        OpamFile.OPAM.write (prepare_dir // "opam") prep_opam;
        OpamFile.Descr.write (prepare_dir // "descr") src_descr;
        if OpamFilename.exists_dir (meta_dir / "files") then
          OpamFilename.copy_dir ~src:(meta_dir / "files") ~dst:prepare_dir
      );
      OpamFile.URL.write (prepare_dir // "url") prep_url;

      OpamGlobals.msg
        "Template metadata for %s generated in %s.\n\
        \  * Check the 'opam' file\n\
        \  * Fill in or check the description of your package in 'descr'\n\
        \  * Check that there are no unneeded files under 'files/'\n\
        \  * Run 'opam publish submit %s' to submit your package\n"
        (OpamPackage.to_string package)
        (OpamFilename.prettify_dir prepare_dir)
        (OpamFilename.prettify_dir prepare_dir)
    )

(* -- Submit command -- *)

let (/) a b = String.concat "/" [a;b]

let git cmds = OpamSystem.command ("git" :: cmds)

let github_root = "git@github.com:"

type github_repo = { label: string; owner: string; name: string; }

let default_repo =
  { label = "default"; owner = "ocaml"; name = "opam-repository"; }

let opam_publish_root =
  OpamFilename.OP.(
    OpamFilename.Dir.of_string OpamGlobals.default_opam_dir /
    "plugins" / "opam-publish"
  )

let repo_dir label =
  OpamFilename.OP.(opam_publish_root / "repos" / label)

let user_branch package =
  "opam-publish" /
  String.map (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' as c -> c
      | _ -> '-'
    ) (OpamPackage.to_string package)

let repo_of_dir dir =
  let label = OpamFilename.Base.to_string (OpamFilename.basename_dir dir) in
  let remote =
    OpamFilename.in_dir dir (fun () ->
        OpamSystem.read_command_output ~verbose:false
          ["git"; "config"; "--get"; "remote.origin.url"]
        |> List.hd)
  in
  Scanf.sscanf remote "git@github.com:%s@/%s@."
    (fun owner name -> { label; owner; name })

let user_of_dir dir =
  let remote =
    OpamFilename.in_dir dir (fun () ->
        OpamSystem.read_command_output ~verbose:false
          ["git"; "config"; "--get"; "remote.user.url"]
        |> List.hd)
  in
  Scanf.sscanf remote "git@github.com:%s@/%s"
    (fun owner _ -> owner)

let get_user repo user_opt =
  let dir = repo_dir repo.label in
  match user_opt with
  | Some u ->
    if OpamFilename.exists_dir dir && user_of_dir dir <> u then
      OpamGlobals.error_and_exit
        "Repo %s already registered with github user %s"
        repo.label u
    else u
  | None ->
    if OpamFilename.exists_dir dir then user_of_dir dir else
    let rec get_u () =
      match OpamGlobals.read "Please enter your github name:" with
      | None -> get_u ()
      | Some u -> u
    in
    get_u ()

module Github = struct
  (* Quick & dirty binding. Temporary *)

  let api = "https://api.github.com/repos"
  let fork repo user =
    (* WARN: there may be a delay ! *)
    OpamSystem.command ~verbose:true
      ["curl"; "-u"; user; "-d"; ""; "-o"; "/dev/null";
       api/repo.owner/repo.name/"forks"]
  (* XXX: not checking return value ! *)

  let pull_request user package repo =
    let json_data =
      [ "title", "[opam-publish] "^OpamPackage.to_string package;
        "body", "Pull-request generated by opam-publish v."^Version.version;
        "head", user^":"^user_branch package;
        "base", "master" ]
      |> List.map (fun (label, value) -> Printf.sprintf "%S: %S" label value)
      |> String.concat ", "
      |> Printf.sprintf "{ %s }"
    in
    OpamSystem.command ~verbose:true
      ["curl"; "-u"; user; "-d"; json_data; "-o"; "/dev/null";
       api/repo.owner/repo.name/"pulls"]
    (* XXX: not checking return value ! *)
end



let init_mirror repo user =
  let dir = repo_dir repo.label in
  OpamFilename.mkdir dir;
  git ["clone"; github_root^repo.owner/repo.name^".git";
       OpamFilename.Dir.to_string dir];
  Github.fork repo user;
  OpamFilename.in_dir dir (fun () ->
      git ["remote"; "add"; "user"; github_root^user/repo.name]
    )

let update_mirror repo =
  OpamFilename.in_dir (repo_dir repo.label) (fun () ->
      git ["fetch"; "--multiple"; "origin"; "user"];
      git ["reset"; "origin/master"; "--hard"];
    )

let add_metadata repo user package user_meta_dir =
  let mirror = repo_dir repo.label in
  let meta_dir =
    OpamFilename.OP.(
      OpamFilename.Dir.of_string "packages" /
      OpamPackage.Name.to_string (OpamPackage.name package) /
      OpamPackage.to_string package
    )
  in
  OpamFilename.in_dir mirror (fun () ->
      if OpamFilename.exists_dir meta_dir then
        git ["rm"; "-r"; OpamFilename.Dir.to_string meta_dir];
      OpamFilename.mkdir (OpamFilename.dirname_dir meta_dir);
      OpamFilename.copy_dir
        ~src:user_meta_dir
        ~dst:meta_dir;
      git ["add"; OpamFilename.Dir.to_string meta_dir];
      git ["commit"; "-m";
           Printf.sprintf "[opam-publish] description for %s"
             (OpamPackage.to_string package)];
      git ["push"; "user"; "+HEAD:"^user_branch package]);
  Github.pull_request user package repo;
  let url = "https://github.com"/repo.owner/repo.name/"pulls" in
  (* TODO: extract real pull *)
  OpamGlobals.msg "Pull-requested: %s\n" url;
  if OpamSystem.command_exists "xdg-open" then
    OpamSystem.command ["xdg-open";url]

let sanity_checks meta_dir =
  let files = OpamFilename.files meta_dir in
  let dirs = OpamFilename.dirs meta_dir in
  let warns =
    files |> List.fold_left (fun warns f ->
        match OpamFilename.Base.to_string (OpamFilename.basename f) with
        | "opam" | "descr" | "url" -> warns
        | f -> Printf.sprintf "extra file %S" f :: warns
      ) []
  in
  let warns =
    dirs |> List.fold_left (fun warns d ->
        match OpamFilename.Base.to_string (OpamFilename.basename_dir d) with
        | "files" -> warns
        | d -> Printf.sprintf "extra dir %S" d :: warns
      ) warns
  in
  if warns <> [] then
    OpamGlobals.error "Bad contents in %s:\n  - %s\n"
      (OpamFilename.Dir.to_string meta_dir)
      (String.concat "\n  - " warns);
  let ok = warns = [] in
  let ok = check_opam OpamFilename.OP.(meta_dir // "opam") && ok in
  let ok = check_url OpamFilename.OP.(meta_dir // "url") && ok in
  let ok = check_descr OpamFilename.OP.(meta_dir // "descr") && ok in
  ok

let submit repo_label user_opt package meta_dir =
  if not (sanity_checks meta_dir) then
    OpamGlobals.error "Please correct the above errors and retry"
  else
  (* Prepare the repo *)
  let mirror_dir = repo_dir repo_label in
  let user, repo =
    if not (OpamFilename.exists_dir mirror_dir) then
      if repo_label = default_repo.label then
        let user = get_user default_repo user_opt in
        init_mirror default_repo user;
        user, default_repo
      else
        OpamGlobals.error_and_exit
          "Repository %S unknown, see `opam-publish repo'"
          repo_label
    else
    let mirror_dir = repo_dir repo_label in
    let repo = repo_of_dir mirror_dir in
    get_user repo user_opt, repo
  in
  (* pull-request processing *)
  update_mirror repo;
  add_metadata repo user package meta_dir


(* -- Command-line handling -- *)

open Cmdliner

(* name * version option *)
let package =
  let parse str =
    let name, version_opt =
      match OpamMisc.cut_at str '.' with
      | None -> str, None
      | Some (n,v) -> n, Some v
    in
    try
      `Ok
        (OpamPackage.Name.of_string name,
         OpamMisc.Option.map OpamPackage.Version.of_string version_opt)
    with Failure _ -> `Error (Printf.sprintf "bad package name %s" name)
  in
  let print ppf (name, version_opt) =
    match version_opt with
    | None -> Format.pp_print_string ppf (OpamPackage.Name.to_string name)
    | Some v -> Format.fprintf ppf "%s.%s"
                  (OpamPackage.Name.to_string name)
                  (OpamPackage.Version.to_string v)
  in
  parse, print

let github_user =
  Arg.(value & opt (some string) None & info ["n";"name"]
         ~docv:"NAME"
         ~doc:"github user name. This can only be set during initialisation \
               of a repo")

let prepare_cmd =
  let doc = "Gets a local metadatada directory from a given remote archive URL, \
             to let you edit locally before submitting." in
  let url = Arg.(required & pos ~rev:true 0 (some string) None & info
                   ~doc:"Public URL hosting the package source archive"
                   ~docv:"URL" [])
  in
  let pkg_opt = Arg.(value & pos ~rev:true 1 (some package) None & info
                       ~docv:"PKG" ~doc:"Package to release" [])
  in
  let prepare url pkg_opt =
    OpamMisc.Option.Op.(
      prepare ?name:(pkg_opt >>| fst) ?version:(pkg_opt >>= snd) url
    )
  in
  Term.(pure prepare $ url $ pkg_opt),
  Term.info "prepare" ~doc

let repos_cmd =
  let doc = "Sets up aliases for repositories you want to submit to." in
  let command =
    Arg.(value &
         pos 0 (enum ["add", `Add; "remove", `Remove; "list", `List]) `List &
         info [] ~docv:"SUBCOMMAND"
           ~doc:"One of $(b,add), $(b,remove) or $(b,list). Defaults to \
                 $(b,list).")
  in
  let label =
    Arg.(value & pos 1 string default_repo.label & info []
           ~docv:"NAME"
           ~doc:"Local name of the repository to use") in
  let gh_address =
    Arg.(value &
         opt (some (pair ~sep:'/' string string)) None &
         info ["a"; "github-address"]
           ~docv:"USER/REPO_NAME"
           ~doc:"Address of the github repo (github.com/USER/REPO_NAME)")
  in
  let repos command label gh_address user_opt =
    match command,gh_address with
    | `Add, Some (owner,name) ->
      if OpamFilename.exists_dir (repo_dir label) then
        `Error (false, "Repo "^label^" is already registered")
      else
      let repo = {label; owner; name} in
      let user = get_user repo user_opt in
      `Ok (init_mirror repo user)
    | `Add, _ -> `Error (true, "github address or user unspecified")
    | `Remove, _ -> `Ok (OpamFilename.rmdir (repo_dir label))
    | `List, _ ->
      `Ok (
        OpamFilename.dirs OpamFilename.OP.(opam_publish_root/"repos")
        |> List.iter @@ fun dir ->
        let repo = repo_of_dir dir in
        Printf.printf "%-20s  %s/%s (%s)" (OpamGlobals.colorise `bold repo.label)
          repo.owner repo.name (get_user repo None)
      );
  in
  Term.(ret (pure repos $ command $ label $ gh_address $ github_user)),
  Term.info "repo" ~doc

let submit_cmd =
  let doc = "submits or updates a pull-request to an OPAM repo." in
  let dir =
    Arg.(required & pos ~rev:true 0 (some string) None & info []
           ~docv:"DIR"
           ~doc:"Path to the metadata from opam-publish prepare") in
  let repo_name =
    Arg.(value & opt string default_repo.label & info ["r";"repo"]
           ~docv:"NAME"
           ~doc:"Local name of the repository to use (see the $(b,repo) \
                 subcommand") in
  let submit user dir repo_name =
    submit repo_name user
      (OpamPackage.of_string (Filename.basename dir))
      (OpamFilename.Dir.of_string dir)
  in
  Term.(pure submit $ github_user $ dir $ repo_name),
  Term.info "submit" ~doc

let cmds = [prepare_cmd; submit_cmd; repos_cmd]

let help_cmd =
  let usage () =
    OpamGlobals.msg "\
Opam-publish v.%s

Sub-commands:\n\
\      prepare URL   Prepares a local package definition directory from a\n\
\                    public URL pointing to a source archive.\n\
\      submit DIR    Submits or updates the request for integration of\n\
\                    the package defined by metadata at DIR.\n\
\      repo          Manage the repos you contribute to.\n\
\n\
See '%s COMMAND --help' for details on each command.\n\
"
      Version.version
      Sys.argv.(0)
  in
  Term.(pure usage $ pure ()),
  Term.info "opam-publish" ~version:(Version.version)

let () =
  Sys.catch_break true;
  let _ = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  try match Term.eval_choice ~catch:false help_cmd cmds with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | OpamGlobals.Exit i as e ->
    if !OpamGlobals.debug && i <> 0 then
      Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit i
  | OpamSystem.Internal_error _
  | OpamSystem.Process_error _ as e ->
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
  | Sys.Break ->
    exit 130
  | Failure msg as e ->
    Printf.eprintf "Fatal error: %s\n" msg;
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit 1
  | e ->
    Printf.eprintf "Fatal error:\n%s\n" (Printexc.to_string e);
    Printf.eprintf "%s" (OpamMisc.pretty_backtrace e);
    exit 1
