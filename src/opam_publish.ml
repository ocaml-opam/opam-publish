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
open OpamMisc.OP

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
    let opam = OF.read file in
    let warn, warnings = mkwarn () in
    List.iter warn (OF.validate opam);
    if OF.is_explicit file then
      warn "should not contain 'name' or 'version' fields";
    warnings file
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


(* -- Submit command -- *)

let (/) a b = String.concat "/" [a;b]

let git cmds = OpamSystem.command ("git" :: cmds)

let github_root = "git@github.com:"

type github_repo = { label: string; owner: string; name: string; }

let default_label = "default"

let default_repo =
  { label = default_label; owner = "ocaml"; name = "opam-repository"; }

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

let repo_rel_package_dir package =
  OpamFilename.OP.(
    OpamFilename.Dir.of_string "packages" /
    OpamPackage.Name.to_string (OpamPackage.name package) /
    OpamPackage.to_string package
  )

let add_metadata repo user package user_meta_dir =
  let mirror = repo_dir repo.label in
  let meta_dir = repo_rel_package_dir package in
  OpamFilename.in_dir mirror (fun () ->
      if OpamFilename.exists_dir meta_dir then
        git ["rm"; "-r"; OpamFilename.Dir.to_string meta_dir];
      OpamFilename.mkdir (OpamFilename.dirname_dir meta_dir);
      OpamFilename.copy_dir
        ~src:user_meta_dir
        ~dst:meta_dir;
      let setmode f mode =
        let file = OpamFilename.OP.(meta_dir // f) in
        if OpamFilename.exists file then OpamFilename.chmod file mode;
      in
      setmode "opam" 0o644;
      setmode "descr" 0o644;
      let () =
        let dir = OpamFilename.OP.(meta_dir / "files") in
        if OpamFilename.exists_dir dir then
          Unix.chmod (OpamFilename.Dir.to_string dir) 0o755
      in
      git ["add"; OpamFilename.Dir.to_string meta_dir];
      git ["commit"; "-m";
           Printf.sprintf "[opam-publish] description for %s"
             (OpamPackage.to_string package)];
      git ["push"; "user"; "+HEAD:"^user_branch package]);
  Github.pull_request user package repo;
  let url = "https://github.com"/repo.owner/repo.name/"pulls" in
  (* TODO: extract real pull *)
  OpamGlobals.msg "Pull-requested: %s\n" url;
  try OpamSystem.command ["xdg-open";url]
  with OpamSystem.Command_not_found _ -> ()

let get_git_metadata_dir package repo =
  let mirror = repo_dir repo.label in
  let meta_dir = repo_rel_package_dir package in
  update_mirror repo;
  OpamFilename.in_dir mirror (fun () ->
      try
        git ["reset"; "--hard"; "remotes"/"user"/user_branch package; "--"];
        if OpamFilename.exists_dir meta_dir then
          Some OpamFilename.OP.(mirror / OpamFilename.Dir.to_string meta_dir)
        else None
      with OpamSystem.Process_error _ -> None)

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
      if repo_label = default_label then
        let user = get_user default_repo user_opt in
        init_mirror default_repo user;
        user, default_repo
      else
        OpamGlobals.error_and_exit
          "Repository %S unknown, see `opam-publish repo'"
          repo_label
    else
    let repo = repo_of_dir mirror_dir in
    get_user repo user_opt, repo
  in
  (* pull-request processing *)
  update_mirror repo;
  add_metadata repo user package meta_dir


(* -- Prepare command -- *)

let prepare ?name ?version ?(repo_label=default_label) http_url =
  let open OpamFilename.OP in
  let open OpamMisc.Option.Op in (* Option monad *)
  OpamFilename.with_tmp_dir @@ fun tmpdir ->
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
  (* Utility functions *)
  let f_opt f = if OpamFilename.exists f then Some f else None in
  let dir_opt d = if OpamFilename.exists_dir d then Some d else None in
  let get_file name reader dir =
    dir >>= dir_opt >>= fun d ->
    f_opt (d // name) >>= fun f ->
    try Some (f, reader f)
    with OpamFormat.Bad_format _ -> None
  in
  let get_opam = get_file "opam" OpamFile.OPAM.read in
  let get_descr = get_file "descr" OpamFile.Descr.read in
  let get_files_dir dir = dir >>= dir_opt >>= fun d -> dir_opt (d / "files") in
  (* Get opam from the archive *)
  let src_meta_dir = dir_opt (srcdir / "opam") ++ dir_opt srcdir in
  let src_opam = get_opam src_meta_dir in
  (* Guess package name and version *)
  let name = match name, src_opam >>| snd >>= OpamFile.OPAM.name_opt with
    | None, None ->
      OpamGlobals.error_and_exit "Package name unspecified"
    | Some n1, Some n2 when n1 <> n2 ->
      OpamGlobals.warning
        "Publishing as package %s, while it refers to itself as %s"
        (OpamPackage.Name.to_string n1) (OpamPackage.Name.to_string n2);
      n1
    | Some n, _ | None, Some n -> n
  in
  let version =
    match version ++ (src_opam >>| snd >>= OpamFile.OPAM.version_opt) with
    | None ->
      OpamGlobals.error_and_exit "Package version unspecified"
    | Some v -> v
  in
  let package = OpamPackage.create name version in
  (* Metadata sources: from OPAM overlay, prepare dir, git mirror, archive.
     Could add: from highest existing version on the repo ? Better
     advise pinning at the moment to encourage some testing. *)
  let prepare_dir_name = OpamFilename.cwd () / OpamPackage.to_string package in
  let prepare_dir = dir_opt prepare_dir_name in
  let overlay_dir =
    let switch =
      match !OpamGlobals.switch with
      | `Command_line s | `Env s -> Some (OpamSwitch.of_string s)
      | `Not_set ->
        f_opt (OpamPath.config (OpamPath.root())) >>|
        OpamFile.Config.read >>|
        OpamFile.Config.switch
    in
    switch >>| fun sw ->
    OpamPath.Switch.Overlay.package (OpamPath.root ()) sw name
  in
  let pub_dir =
    dir_opt (repo_dir repo_label) >>| repo_of_dir >>=
    get_git_metadata_dir package
  in
  (* Choose metadata from the sources *)
  let prep_url =
    (* Todo: advise mirrors if existing in other versions ? *)
    OpamFile.URL.with_checksum (OpamFile.URL.create `http url) checksum
  in
  let chosen_opam_and_files =
    let get_opam_and_files dir =
      get_opam dir >>| fun o -> o, get_files_dir dir
    in
    get_opam_and_files overlay_dir  ++
    get_opam_and_files prepare_dir ++
    get_opam_and_files pub_dir ++
    get_opam_and_files src_meta_dir
  in
  let chosen_descr =
    get_descr overlay_dir ++
    get_descr prepare_dir ++
    get_descr pub_dir ++
    get_descr src_meta_dir
  in
  (* Choose and copy or write *)
  OpamFilename.mkdir prepare_dir_name;
  let prepare_dir = prepare_dir_name in
  match chosen_opam_and_files with
  | None ->
    OpamGlobals.error_and_exit
      "No metadata found. \
       Try pinning the package locally (`opam pin add %s %S`) beforehand."
      (OpamPackage.Name.to_string name) http_url
  | Some ((opam_file, opam), files_opt) ->
    let open OpamFile in
    if OPAM.name opam <> name || OPAM.version opam <> version ||
       OPAM.is_explicit opam_file
    then
      let opam = OPAM.with_name_opt opam None in
      let opam = OPAM.with_version_opt opam None in
      OPAM.write (prepare_dir // "opam") opam
    else
      OpamFilename.copy ~src:opam_file ~dst:(prepare_dir // "opam");
    (files_opt >>| fun src ->
     OpamFilename.copy_dir ~src ~dst:(prepare_dir / "files"))
    +! ();
    (match
       chosen_descr >>| fun (src, _descr) ->
       OpamFilename.copy ~src ~dst:(prepare_dir // "descr")
     with Some () -> ()
        | None -> OpamFile.Descr.write (prepare_dir // "descr") descr_template);
    OpamFile.URL.write (prepare_dir // "url") prep_url;
    (* Todo: add an option to get all the versions in prepare_dir and let
       the user merge *)

    OpamGlobals.msg
      "Template metadata for %s generated in %s.\n\
      \  * Check the 'opam' file\n\
      \  * Fill in or check the description of your package in 'descr'\n\
      \  * Check that there are no unneeded files under 'files/'\n\
      \  * Run 'opam publish submit %s' to submit your package\n"
      (OpamPackage.to_string package)
      OpamFilename.(remove_prefix (cwd ()) (prepare_dir // ""))
      OpamFilename.(remove_prefix (cwd ()) (prepare_dir // ""))


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

let repo_name =
  Arg.(value & opt string default_label & info ["r";"repo"]
         ~docv:"NAME"
         ~doc:"Local name of the repository to use (see the $(b,repo) \
               subcommand")

let prepare_cmd =
  let doc = "Provided a remote archive URL, gathers metadata for an OPAM \
             package suitable for editing and submitting to an OPAM repo. \
             A directory $(b,PACKAGE).$(b,VERSION) is generated, or updated \
             if it exists." in
  let url = Arg.(required & pos ~rev:true 0 (some string) None & info
                   ~doc:"Public URL hosting the package source archive"
                   ~docv:"URL" [])
  in
  let pkg_opt = Arg.(value & pos ~rev:true 1 (some package) None & info
                       ~docv:"PACKAGE"
                       ~doc:"Package to release, with optional version" [])
  in
  let prepare url pkg_opt repo_label =
    OpamMisc.Option.Op.(
      prepare ?name:(pkg_opt >>| fst) ?version:(pkg_opt >>= snd) ~repo_label url
    )
  in
  Term.(pure prepare $ url $ pkg_opt $ repo_name),
  Term.info "prepare" ~doc

let repo_cmd =
  let doc = "Sets up aliases for repositories you want to submit to." in
  let command =
    Arg.(value &
         pos 0 (enum ["add", `Add; "remove", `Remove; "list", `List]) `List &
         info [] ~docv:"SUBCOMMAND"
           ~doc:"One of $(b,add), $(b,remove) or $(b,list). Defaults to \
                 $(b,list).")
  in
  let label =
    Arg.(value & pos 1 string default_label & info []
           ~docv:"NAME"
           ~doc:"Local name of the repository to use") in
  let gh_address =
    Arg.(value &
         opt (some (pair ~sep:'/' string string)) None &
         info ["a"; "github-address"]
           ~docv:"USER/REPO_NAME"
           ~doc:"Address of the github repo (github.com/USER/REPO_NAME)")
  in
  let repo command label gh_address user_opt =
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
  Term.(ret (pure repo $ command $ label $ gh_address $ github_user)),
  Term.info "repo" ~doc

let submit_cmd =
  let doc = "submits or updates a pull-request to an OPAM repo." in
  let dir =
    Arg.(required & pos ~rev:true 0 (some string) None & info []
           ~docv:"DIR"
           ~doc:"Path to the metadata from opam-publish prepare") in
  let submit user dir repo_name =
    submit repo_name user
      (OpamPackage.of_string (Filename.basename dir))
      (OpamFilename.Dir.of_string dir)
  in
  Term.(pure submit $ github_user $ dir $ repo_name),
  Term.info "submit" ~doc

let cmds = [prepare_cmd; submit_cmd; repo_cmd]

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
