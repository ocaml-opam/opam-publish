(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op
open OpamStd.Option.Op
open PublishCommon

module Lint = Opam_ci_check_lint

type meta = {
  archive: OpamFilename.t option;
  opam: OpamFile.OPAM.t OpamFile.t option;
  checksum: OpamHash.t list option;
  dir: OpamFilename.Dir.t option;
  repo: (string * string) option;
  tag: string option;
  url: OpamUrl.t option;
  name: OpamPackage.Name.t option;
  version: OpamPackage.Version.t option;
}

let name = function
  | { name = Some n; _ } -> n
  | _ -> invalid_arg "name"
let version = function
  | { version = Some v; _ } -> v
  | _ -> invalid_arg "version"
let package = function
  | { name = Some n; version = Some v; _ } -> OpamPackage.create n v
  | _ -> invalid_arg "package"
let opam = function
  | { opam = Some o; _ } -> o
  | _ -> invalid_arg "opam"

let ( / ) a b = String.concat "/" [a;b]

let tmp_archive tmpdir url =
  OpamFilename.Op.(tmpdir / "archives" //
                   Printf.sprintf "%s-%s"
                     (Digest.to_hex (Digest.string (OpamUrl.to_string url)))
                     (OpamUrl.basename url))

let tmp_source tmpdir url =
  OpamFilename.Op.(tmpdir / "sources" /
                   Printf.sprintf "%s-%s"
                     (Digest.to_hex (Digest.string (OpamUrl.to_string url)))
                     (OpamUrl.basename url))

let upgrade_to_2_0 ?(local=true) opam0 =
  try
    Option.iter (fun opam ->
        let opam =
          OpamFileTools.add_aux_files ~dir:(OpamFilename.dirname (OpamFile.filename opam0))
            ~files_subdir_hashes:true opam
        in
        let opam2 = OpamFormatUpgrade.opam_file ~filename:opam0 opam in
        if not (OpamFile.OPAM.equal opam2 opam) then
          (OpamConsole.warning "%s has %s format, \
                                which is not accepted on 2.0 repository.%s"
             (if local then
                "Opam file " ^ OpamFilename.to_string (OpamFile.filename opam0)
              else "Downloaded opam file")
             (OpamConsole.colorise `underline
                (OpamVersion.to_string (OpamFile.OPAM.opam_version opam)))
             (if not local then
                OpamConsole.colorise `bold " Updating it."
              else "");
           if local &&
              not (OpamConsole.confirm " Update it inplace to %s format?"
                     (OpamConsole.colorise `bold "2.0"))
           then OpamStd.Sys.exit_because `Aborted;
           OpamFile.OPAM.write_with_preserved_format ~format_from:opam0 opam0 opam2);
        List.iter
          OpamFilename.(fun name ->
              remove Op.(dirname (OpamFile.filename opam0) // name))
          ["descr";"url"];
      ) (OpamFile.OPAM.read_opt opam0)
  with OpamPp.Bad_format _ | OpamPp.Bad_version _ ->
    OpamConsole.error_and_exit `Bad_arguments
      "Could not parse the file %S as an opam file"
      (OpamFile.to_string opam0)

let get_metas ~exclude force tmpdir dirs opams urls repos tag names version =
  let lopt = function [x] -> Some x | _ -> None in
  let base_meta = {
    archive = None;
    opam = lopt opams;
    checksum = None;
    dir = lopt dirs;
    repo = lopt repos;
    tag = tag;
    url = lopt urls;
    name = lopt names;
    version = version;
  } in
  let start_metas =
    match dirs, opams, urls, repos with
    | [], [], [], [] ->
      [{ base_meta with dir = Some (OpamFilename.cwd ()) }]
    | (_::_), ([] | [_]), ([] | [_]), ([] | [_]) ->
      dirs |> List.map (fun d -> { base_meta with dir = Some d })
    | ([] | [_]), (_::_), ([] | [_]), ([] | [_]) ->
      opams |> List.map (fun o -> { base_meta with opam = Some o })
    | ([] | [_]), ([] | [_]), (_::_), ([] | [_]) ->
      urls |> List.map (fun u -> { base_meta with url = Some u })
    | ([] | [_]), ([] | [_]), ([] | [_]), (_::_) ->
      repos |> List.map (fun r -> { base_meta with repo = Some r })
    | _ ->
      OpamConsole.error_and_exit `Bad_arguments
        "Only one of the DIR, OPAM, URL and PROJECT arguments can be repeated"
  in
  (* These rewriting rules fill all the unknown data. They return None when they
     don't apply *)
  let read_opam f =
    match OpamFileTools.lint_file f with
    | _, Some o -> o
    | _ -> OpamFile.OPAM.empty
  in
  let of_dir ?(local=true) m dir =
    let r =
      List.filter_map (fun {OpamStateTypes.pin_name = n; pin = {pin_file = o; _}} ->
          let sz =
            try Unix.((stat (OpamFile.to_string o)).st_size)
            with Unix.Unix_error _ -> -1
          in
          if sz < 10 || sz > 100_000 then None else
          match n, m.name with
          | Some n1, Some n2 when n1 = n2 ->
            upgrade_to_2_0 ~local o;
            Some { m with opam = Some o }
          | n, None | None, n ->
            upgrade_to_2_0 ~local  o;
            Some { m with opam = Some o; name = n }
          | _ -> None)
        (OpamPinned.files_in_source dir)
    in
    if r <> [] then Some r else None
  in
  let fill_rules = [
    (function (* URL (and checksum) from opam file *)
      | { opam = Some opam; url = None; _ } as m ->
        OpamFile.OPAM.url (read_opam opam) >>| fun u ->
        let checksum =
          m.checksum ++ match OpamFile.URL.checksum u with
          | [] -> None
          | l -> Some l
        in
        [{ m with url = Some (OpamFile.URL.url u); checksum }]
      | _ -> None);
    (function (* source dir from opam file *)
      | { opam = Some opam; dir = None; _ } as m ->
        let dir = OpamFilename.dirname (OpamFile.filename opam) in
        Some [{ m with dir = Some dir }]
      | _ -> None);
    (function (* opam files from source dir *)
      | { dir = Some dir; opam = None; _ } as m -> of_dir m dir
      | _ -> None);
    (function (* Github repo from source dir *)
      | { dir = Some dir; repo = None; _ } as m ->
        repo_of_dir dir >>| fun r -> [{ m with repo = Some r }]
      | _ -> None);
    (function (* tag from source dir *)
      | { dir = Some dir; tag = None; _ } as m ->
        tag_of_dir dir >>| fun t -> [{ m with tag = Some t }]
      | _ -> None);
    (function (* Github archive URL from repo and tag *)
      | { repo = Some repo; tag = Some tag; url = None; opam; _ } as m ->
        let no_url = match opam with
          | None -> false
          | Some opam ->
            List.mem OpamTypes.Pkgflag_Conf
              OpamFile.OPAM.(flags (safe_read opam))
        in
        if no_url then None else
        let u =
          Printf.sprintf "https://github.com/%s/%s/archive/refs/tags/%s.tar.gz"
            (fst repo) (snd repo) tag
        in
        Some [{ m with url = Some (OpamUrl.of_string u) }]
      | _ -> None);
    (function (* Archive from URL *)
      | { url = Some url; archive = None; opam = None; _ }
      | { url = Some url; archive = None; checksum = None; _ } as m ->
        let f = tmp_archive tmpdir url in
        if not (OpamFilename.exists f) then
          (try
             OpamProcess.Job.run
               (OpamDownload.download_as ~overwrite:false url f)
           with OpamDownload.Download_fail (msgopt, _) ->
             OpamConsole.error_and_exit `Sync_error
               "Could not download archive at %s%s.\n- make sure %s"
               (OpamConsole.colorise `underline (OpamUrl.to_string url))
               (OpamStd.Option.to_string (Printf.sprintf " (%s)") msgopt)
               (match m.repo with
                | Some _ -> "you have pushed your tag to Github."
                | None -> "your archive is uploaded and accessible."));
        Some [{ m with archive = Some f }]
      | _ -> None);
    (function (* checksum from archive *)
      | { archive = Some a; checksum = None; _ } as m ->
        let checksum = [
          OpamHash.compute ~kind:`MD5 (OpamFilename.to_string a);
          OpamHash.compute ~kind:`SHA512 (OpamFilename.to_string a);
        ] in
        Some [{ m with checksum = Some checksum }]
      | _ -> None);
    (function (* opams from archive *)
      | { url = Some url; archive = Some archive; opam = None; _ } as m ->
        let srcdir = tmp_source tmpdir url in
        OpamFilename.extract archive srcdir;
        of_dir ~local:false m srcdir
      | _ -> None);
    (function (* name from opam file contents *)
      | { opam = Some opam; name = None; _ } as m ->
        OpamFile.OPAM.name_opt (read_opam opam) >>| fun n ->
        [{ m with name = Some n }]
      | _ -> None);
    (function (* version from opam file *)
      | { opam = Some opam; version = None; _ } as m ->
        OpamFile.OPAM.version_opt (read_opam opam) >>| fun v ->
        [{ m with version = Some v }]
      | _ -> None);
    (function (* name from opam file name *)
      | { opam = Some opam; dir = Some dir; name = None; _ } as m ->
        OpamPinned.name_of_opam_filename dir (OpamFile.filename opam) >>| fun n ->
        [{ m with name = Some n }]
      | _ -> None);
    (function (* name from repo *)
      | { repo = Some (_, rname); name = None; _ } as m ->
        (try Some [{ m with name = Some (OpamPackage.Name.of_string rname) }]
         with Failure _ -> None)
      | _ -> None);
    (function (* name from dir *)
      | { dir = Some dir; name = None; _ } as m ->
        let b = OpamFilename.(Base.to_string (basename_dir dir)) in
        let b = match OpamStd.String.cut_at b '.' with
          | Some (n, _) -> n
          | None -> b
        in
        (try Some [{ m with name = Some (OpamPackage.Name.of_string b) }]
         with Failure _ -> None)
      | _ -> None);
    (function (* version from tag *)
      | { tag = Some tag; version = None; _ } as m ->
        (let v = OpamStd.String.remove_prefix ~prefix:"v" tag in
         try Some [{ m with version = Some (OpamPackage.Version.of_string v) }]
         with Failure _ -> None)
      | _ -> None);
  ] in
  let rec apply_rules_fix m0 =
    match OpamStd.List.find_map (fun rule -> rule m0) fill_rules with
    | m1 -> if m1 = [m0] then m1 else apply_rules_fix_list m1
    | exception Not_found -> [m0]
  and apply_rules_fix_list ms =
    List.fold_left
      (fun acc m -> List.rev_append (apply_rules_fix m) acc)
      [] ms
  in
  let metas =
    apply_rules_fix_list start_metas
  in
  let metas = (* Use single name arg for single pkg *)
    match metas, names with
    | [{ name = None; _ } as m], [n] -> [{ m with name = Some n }]
    | _ ->
      let missing_names =
        List.filter
          (fun n -> not (List.exists (fun m -> m.name = Some n) metas))
          names
      in
      if missing_names <> [] then
        OpamConsole.error_and_exit `Not_found
          "These packages' opam files couldn't be found: %s"
          (OpamStd.List.concat_map ", " OpamPackage.Name.to_string
             missing_names);
      metas
  in
  let metas =
    List.fold_left (fun metas name ->
        let name = OpamPackage.Name.of_string name in
        match
          List.partition (function
              | {name = Some name'; _} ->
                OpamPackage.Name.equal name name'
              | {name = None; _} -> false)
            metas
        with
        | [], _ ->
          OpamConsole.error_and_exit `Not_found
            "Excluded package '%s' not found"
            (OpamPackage.Name.to_string name)
        | _::_, metas -> metas)
      metas exclude
  in
  let is_incomplete =
    List.exists (fun m ->
        m.opam = None || m.name = None || m.version = None ||
        m.url = None && match m.opam with
        | Some o ->
          not force &&
          not OpamFile.OPAM.(has_flag OpamTypes.Pkgflag_Conf (safe_read o))
        | None -> true)
      metas
  in
  (if is_incomplete then
     (OpamConsole.error
        "Not enough information to publish. Here is what I could gather:";
      OpamConsole.errmsg "%s\n")
   else
     OpamConsole.msg "\nThe following will be published:\n%s\n") @@
  OpamStd.Format.itemize (fun m ->
      Printf.sprintf "%s version %s with opam file %s\n\
                      archive at %s"
        (OpamStd.Option.to_string ~none:(OpamConsole.colorise `red "UNKNOWN")
           (OpamConsole.colorise `bold @* OpamPackage.Name.to_string)
           m.name)
        (OpamStd.Option.to_string ~none:(OpamConsole.colorise `red "UNKNOWN")
           (OpamConsole.colorise `bold @* OpamPackage.Version.to_string)
           m.version)
        (match m with
         | { opam = None; _ } ->
           OpamConsole.colorise `red "NONE FOUND"
         | { opam = Some o; url = Some u; _ }
           when OpamFilename.starts_with
               (tmp_source tmpdir u) (OpamFile.filename o) ->
           "from the upstream archive"
         | { opam = Some o; _ } ->
           "at " ^ OpamConsole.colorise `bold (OpamFile.to_string o))
        (match m.url with
         | None ->
           OpamConsole.colorise `red "NONE FOUND"
         | Some u ->
           OpamConsole.colorise `bold @@
           OpamUrl.to_string u))
    metas;
  if is_incomplete then OpamStd.Sys.exit_because `Not_found;
  let rec has_dups = function
    | { name; _ } :: r -> List.exists (fun m -> m.name = name) r || has_dups r
    | [] -> false
  in
  if has_dups metas then
    OpamConsole.error_and_exit `Bad_arguments
      "Multiple publications with the same name are not allowed";
  let no_url = List.filter (fun m -> m.url = None) metas in
  if no_url <> [] then
    OpamConsole.warning
      "These will be virtual or conf packages: %s"
      (OpamStd.List.concat_map ", "
         (fun m -> OpamConsole.colorise `bold (OpamPackage.to_string (package m)))
         no_url);
  metas

let set_pre_release ~pre_release opam =
  if pre_release then
    let opam_version =
      OpamTypes.FIdent ([], OpamVariable.of_string "opam-version", None)
    in
    let add_to_available filter opam =
      match OpamFile.OPAM.available opam with
      | FBool true ->
        OpamFile.OPAM.with_available filter opam
      | available ->
        OpamFile.OPAM.with_available (OpamTypes.FAnd (filter, available)) opam
    in
    opam
    |> OpamFile.OPAM.add_flags [OpamTypes.Pkgflag_AvoidVersion]
    |> add_to_available (OpamTypes.FOp (opam_version, `Geq, FString "2.1.0"))
  else
    opam

let get_opam ?(force=false) ~pre_release meta =
  let f = opam meta in
  let warns, opam = OpamFileTools.lint_file f in
  let err = List.exists (fun (_,e,_) -> e = `Error) warns in
  if warns <> [] then
    OpamConsole.errmsg "Definition for %s didn't pass validation:\n%s\n"
      (OpamConsole.colorise `bold (OpamPackage.to_string (package meta)))
      (OpamFileTools.warns_to_string warns);
  if err && not force then None else
  match opam with
  | Some o ->
    let url =
      match meta with
      | { url = Some u; checksum = Some c; _ } ->
        Some (OpamFile.URL.create ~checksum:c u)
      | _ -> None
    in
    o
    |> OpamFile.OPAM.with_name_opt None
    |> OpamFile.OPAM.with_version_opt None
    |> OpamFile.OPAM.with_url_opt url
    |> set_pre_release ~pre_release
    |> Option.some
  | None -> None

let get_opams ~exclude ~pre_release force dirs opams urls repos tag names version =
  List.iter upgrade_to_2_0 opams;
  OpamFilename.with_tmp_dir @@ fun tmpdir ->
  get_metas ~exclude force tmpdir dirs opams urls repos tag names version |>
  List.map (fun m -> package m, (m, get_opam ~force ~pre_release m)) |>
  OpamPackage.Map.of_list |>
  OpamPackage.Map.map
    (function
      | m, Some o -> m, o
      | _, None -> OpamStd.Sys.exit_because `Not_found)

open Cmdliner

module Args = struct
  open Arg

  let wrap c (pa, pr) =
    conv ~docv:(conv_docv c) (
      (fun s -> match conv_parser c s with
         | Ok x -> (try Ok (pa x) with Failure e -> Error (`Msg e))
         | Error e -> Error e),
      (fun fmt x -> conv_printer c fmt (pr x))
    )

  let wrap_m (type t) c (module M: OpamStd.ABSTRACT with type t = t) =
    wrap c (M.of_string, M.to_string)

  let repo_conv =
    wrap string
      ((fun s ->
          match OpamStd.String.cut_at s '/' with
          | Some (owner,name) -> owner, name
          | None -> failwith "REPO must be of the form 'owner/name'"),
       (fun (owner, name) -> owner / name))

  let docs = "METADATA GATHERING"

  let src_args =
    let src_arg = (
      (fun s ->
         let u = OpamUrl.of_string s in
         let src_opt =
           (if s <> "opam" && not (String.contains s '/') &&
               not (OpamCompat.String.ends_with ~suffix:".opam" s)
            then
              try Some (`Name (OpamPackage.Name.of_string s))
              with Failure _ -> None
            else None)
           >>+ fun () ->
           (match u.OpamUrl.backend with `http -> Some (`Url u) | _ -> None)
           >>+ fun () ->
           (OpamUrl.local_dir u >>| fun d -> `Dir d)
           >>+ fun () ->
           (OpamUrl.local_file u >>| fun f ->
            let basename = OpamFilename.basename f in
            if OpamFilename.Base.check_suffix basename ".opam" ||
               OpamFilename.Base.to_string basename = "opam"
            then `Opam (OpamFile.make f: OpamFile.OPAM.t OpamFile.t)
            else failwith "This is neither a URL to a tarball or an opam file")
           >>+ fun () ->
           (match OpamStd.String.split s '/' with
            | [pkg] ->
              (try Some (`Name (OpamPackage.Name.of_string pkg))
               with Failure _ -> None)
            | [owner;name] -> Some (`Project (owner, name))
            | _ -> None)
         in
         match src_opt with
         | Some a -> a
         | None -> failwith "Invalid argument"),
      (function
        | `Url u -> OpamUrl.to_string u
        | `Dir d -> OpamFilename.Dir.to_string d
        | `Opam o -> OpamFile.to_string o
        | `Name n -> OpamPackage.Name.to_string n
        | `Project (owner,name) -> owner / name)
    ) in
    value & pos_all (wrap string src_arg) [] &
    info [] ~docs ~docv:"DIR|URL|OPAM|PROJECT|NAME" ~doc:
      "Specify the packages to submit (see the general $(b,DESCRIPTION))"

  let force =
    value & flag &
    info ["f";"force"] ~docs ~doc:
      "Don't stop on opam linting errors or opam-ci-check-lint errors."

  let tag =
    value & opt (some string) None &
    info ["tag"] ~docs ~docv:"TAG" ~doc:
      "The git tag at which to publish"

  let version =
    value & opt (some (wrap_m string (module OpamPackage.Version))) None &
    info ["v"] ~docs ~docv:"VERSION" ~doc:
      "The version of the package to publish"

  let docs = "SUBMITTING"

  let dry_run =
    value & flag &
    info ["n";"dry-run"] ~docs ~doc:
      "Show what would be submitted, but don't file a pull-request"

  let opam_ci_lint =
    value & flag &
    info ["opam-ci-lint"] ~docs ~doc:
      "Run the opam-ci-check-lint tool on the packages before submitting."

  let output_patch =
    value & opt (some string) None &
    info ["o";"output-patch"] ~docs ~doc:
      "Output a repository patch to the given file, don't do any uploads or \
       pull-requests"

  let no_browser =
    value & flag &
    info ["no-browser"] ~docs ~doc:
      "Disables opening the browser after submitting a pull-request"

  let repo =
    value & opt repo_conv ("ocaml", "opam-repository") &
    info ["repo"] ~docs ~docv:"REPO" ~doc:
      "The package repository to submit to, in the form $(b,owner/name).\
       For repositories with a subdirectory use with $(b,--packages-directory) option.\
       For example for Coq, use $(i,coq/opam-coq-archive) along with\
       $(i,--packages-directory=released/packages)."

  let target_branch =
    value & opt string "master" &
    info ["target-branch";"b"] ~docs ~docv:"BRANCH" ~doc:
      "The branch to submit the pull-requests to on the target package \
       repository"

  let packages_dir =
    value & opt string "packages" &
    info ["packages-directory"] ~docs ~docv:"DIR" ~doc:
      "The relative name of the directory (inside the GitHub \
       repository) where package descriptions are stored. For \
       instance, for Coq packages, use \"released/packages\"."

  let title =
    value & opt (some string) None &
    info ["t"; "title"] ~docs ~docv:"TXT" ~doc:
      "The title of the commit and pull request that will be submitted"

  let msg_file =
    value & opt (some file) None &
    info ["msg-file"] ~docs ~docv:"FILE" ~doc:
      "File containing a message to be appended to the pull request's body, \
       such as release notes."

  let split =
    value & flag &
    info ["split"] ~docs ~doc:
      "Split the URL and description of packages into separate files (`url` \
       and `descr`). This can be useful on legacy repositories, but is \
       deprecated."

  let no_confirmation =
    value & flag &
    info ["no-confirmation"] ~docs ~doc:
      "Skip all confirmations. This should only ever be used in very special circumstances, like automated publishing CI workflows where measures to ensure the submission has been reviewed have already taken place."

  let exclude =
    value & opt (list string) [] &
    info ["exclude"] ~docs ~doc:
      "Excludes the given list of packages from the publishing process"

  let pre_release =
    value & flag &
    info ["pre-release"] ~docs ~doc:
      "Add the necessary flags and availability formula (`flags: avoid-version` and `available: opam-version >= \"2.1.0\"`) for pre-releases (e.g. alpha, beta, rc, …) that are not meant to be used by default"

  let token =
    let doc = "Personal GitHub access token for authentication" in
    let env = Cmd.Env.info ~docs ~doc "OPAM_PUBLISH_GH_TOKEN" in
    value & opt (some string) None & info ["token"] ~env ~docs ~doc
end

let identical f = function
  | [] -> None
  | x::r ->
    let y = f x in
    if List.for_all (fun x -> f x = y) r then Some y
    else None

let pull_request_message ?msg meta_opams =
  let opams = OpamPackage.Map.(bindings @@ map snd meta_opams) in
  let packages, opams_l = List.split opams in
  let title =
    match packages with
    | [p] -> Printf.sprintf "Package %s" (OpamPackage.to_string p)
    | ps ->
      match
        identical
          (fun p -> OpamFile.OPAM.get_url (List.assoc p opams))
          ps
      with
      | Some (Some u) ->
        let path =
          try Scanf.sscanf u.OpamUrl.path "github.com/%s@/%s@/%_s"
                (fun owner name ->
                   Printf.sprintf "%s/%s%s" owner name
                     (match identical OpamPackage.version ps with
                      | Some v -> " at " ^ OpamPackage.Version.to_string v
                      | None -> ""))
          with Scanf.Scan_failure _ | End_of_file -> u.OpamUrl.path
        in
        Printf.sprintf "%d packages from %s" (List.length ps) path
      | _ ->
        let n = List.length ps in
        if n > 8 then match ps with
          | p1::p2::_ ->
            Printf.sprintf "%d packages (%s, %s, etc.)"
              n (OpamPackage.to_string p1) (OpamPackage.to_string p2)
          | _ -> assert false
        else
          Printf.sprintf "Packages %s"
            (OpamStd.Format.pretty_list
               (List.map OpamPackage.to_string ps))
  in
  let summary =
    match opams with
    | [p, o] ->
      Printf.sprintf "### `%s`\n%s%s\n"
        (OpamPackage.to_string p)
        (OpamStd.Option.to_string (fun s -> s^"\n")
           (OpamFile.OPAM.synopsis o))
        (OpamStd.Option.to_string (fun s -> s^"\n")
           (OpamFile.OPAM.descr_body o))
    | ps ->
      match identical OpamFile.OPAM.synopsis opams_l with
      | Some (Some s) ->
        Printf.sprintf "%s\n\nThis pull-request concerns:\n%s\n" s
          (OpamStd.Format.itemize ~bullet:"- "
             (fun (nv, _) -> Printf.sprintf "`%s`" (OpamPackage.to_string nv))
             ps)
      | _ ->
        Printf.sprintf "This pull-request concerns:\n%s\n"
          (OpamStd.Format.itemize ~bullet:"- "
             (fun (nv, o) ->
                match OpamFile.OPAM.synopsis o with
                | Some s ->
                  Printf.sprintf "`%s`: %s" (OpamPackage.to_string nv) s
                | None -> Printf.sprintf "`%s`" (OpamPackage.to_string nv))
             ps)
  in
  let homepage = match identical OpamFile.OPAM.homepage opams_l with
    | Some (h :: _) -> Printf.sprintf "* Homepage: %s\n" h
    | _ -> ""
  in
  let dev = match identical OpamFile.OPAM.dev_repo opams_l with
    | Some (Some d) ->
      Printf.sprintf "* Source repo: %s\n" (OpamUrl.to_string d)
    | _ -> ""
  in
  let bugs = match identical OpamFile.OPAM.bug_reports opams_l with
    | Some (h :: _) -> Printf.sprintf "* Bug tracker: %s\n" h
    | _ -> ""
  in
  let msg = match msg with
    | None -> ""
    | Some m -> OpamSystem.read m
  in
  let body =
    Printf.sprintf
      "%s\n\
       \n---\n\
       %s%s%s%s\
       %s%s\
       :camel: Pull-request generated by opam-publish v%s"
      summary
      homepage dev bugs
      (if summary <> "" || dev <> "" || bugs <> "" then "\n---\n" else "")
      msg
      (if msg <> "" then "\n\n---\n" else "")
      Version.version
  in
  title, body

let to_files ?(split=false) ~packages_dir meta_opams =
  OpamPackage.Map.fold (fun p (m, o) acc ->
      let dir =
        packages_dir (* default value: "packages" *)
        / OpamPackage.Name.to_string p.name
        / OpamPackage.to_string p
      in
      let acc = (dir, None) :: acc in
      let o, acc =
        match split, OpamFile.OPAM.url o with
        | true, Some u ->
          OpamFile.OPAM.with_url_opt None o,
          (dir / "url", Some (OpamFile.URL.write_to_string u, 0o644)) :: acc
        | _ -> o, acc
      in
      let o, acc =
        match split, OpamFile.OPAM.descr o with
        | true, Some d ->
          OpamFile.OPAM.with_descr_opt None o,
          (dir / "descr", Some (OpamFile.Descr.write_to_string d, 0o644)) :: acc
        | _ -> o, acc
      in
      (dir / "opam",
       (Some (OpamFile.OPAM.to_string_with_preserved_format (opam m) o, 0o644)))
      :: acc)
    meta_opams
    []
  |> List.rev

let opam_ci_check ~opam_repo_dir meta_opams =
  let opam_repo_dir = OpamFilename.Dir.to_string opam_repo_dir in
  let lint_metas = OpamPackage.Map.fold (fun pkg (m, o) acc ->
      let pkg_src_dir = Option.map OpamFilename.Dir.to_string m.dir in
      let newly_published = None in
      Lint.v ~pkg ~pkg_src_dir ~newly_published o :: acc
    ) meta_opams [] in
  Lint.lint_packages ~opam_repo_dir lint_metas

let main_term root =
  let run
      args force tag version dry_run opam_ci_lint output_patch no_browser repo
      target_branch packages_dir title msg split no_confirmation exclude
      pre_release token =
    let dirs, opams, urls, projects, names =
      List.fold_left (fun (dirs, opams, urls, projects, names) -> function
          | `Dir d -> (dirs @ [d], opams, urls, projects, names)
          | `Opam o -> (dirs, opams @ [o], urls, projects, names)
          | `Url u -> (dirs, opams, urls @ [u], projects, names)
          | `Project r -> (dirs, opams, urls, projects @ [r], names)
          | `Name n -> (dirs, opams, urls, projects, names @ [n]))
        ([], [], [], [], [])
        args
    in
    if no_confirmation then begin
      if OpamStd.Sys.tty_in then
        OpamConsole.error_and_exit `Bad_arguments
          "'--no-confirmation' is not allowed in interactive mode. Please \
           review before submitting"
      else
        OpamCoreConfig.update ~confirm_level:`unsafe_yes ()
    end;
    let meta_opams =
      get_opams ~exclude ~pre_release force dirs opams urls projects tag names version
    in
    if not (output_patch <> None ||
            OpamConsole.confirm
              "\nYou will be shown the patch before submitting.\n\
               Please confirm the above data. Continue ? ")
    then OpamStd.Sys.exit_because `Aborted;
    let pr_title, pr_body = pull_request_message ?msg meta_opams in
    let pr_title = title +! pr_title in
    let files = to_files ~split ~packages_dir meta_opams in
    let output_patch = Option.map OpamFilename.of_string output_patch in
    let opam_repo_dir =
      PublishSubmit.prepare_opam_repository ~token ~target_branch ~repo root in
    let lint_errors =
      match opam_ci_lint, repo with
      | true, _
      | _, ("ocaml", "opam-repository") ->
        opam_ci_check ~opam_repo_dir meta_opams |> (function
            | Ok [] -> Ok ()
            | Ok e -> Error (e |> List.map Lint.msg_of_error |> String.concat "\n")
            | Error _ as e -> e)
      | _ -> Ok ()
    in
    if Result.is_error lint_errors then
      OpamConsole.error "%s" (Result.get_error lint_errors);
    if Result.is_ok lint_errors || force then
      PublishSubmit.submit
        root
        ~token
        ~dry_run
        ~output_patch
        ~no_browser
        repo target_branch pr_title pr_body
        (OpamPackage.Map.keys meta_opams)
        files
  in
  let open Args in
  Term.(const run
        $ src_args $ force $ tag $ version $ dry_run $ opam_ci_lint
        $ output_patch $ no_browser $ repo $ target_branch $ packages_dir
        $ title $ msg_file $ split $ no_confirmation $ exclude $ pre_release
        $ token)

let main_info =
  Cmd.info "opam-publish"
    ~version:Version.version
    ~doc:"Helper for package publications on opam repositories"
    ~man:[
      `S "DESCRIPTION";
      `P "opam-publish submits new or updated package definitions as pull-requests to \
       opam package repositories hosted on Github.";
      `P "The packages to publish can be specified in one of the following \
          ways:";
      `I ("$(i,DIR)",
          "Directories, either bound to a Github repository, or containing \
           opam files. When nothing is specified, the default is to use the \
           current directory.");
      `I ("$(i,URL)",
          "URLs pointing to the source archives of the packages that you want \
           published.");
      `I ("$(i,OPAM)",
          "Paths to package definition ($(i,opam) or $(i,*.opam)) files.");
      `I ("$(i,PROJECT)",
          "Github repositories (in the form $(i,owner/name)).");
      `P "When using repositories, combine with the $(b,--tag) option to \
          select the version to publish. By default, the latest one is chosen. \
          Publishing multiple packages at once is allowed by repeating at most \
          one of the argument kinds above (the non-repeated ones will be \
          assumed to apply to all the packages to submit).";
      `P "Additionally, package $(i,NAME) arguments are allowed. If present, \
          they filter out packages with different names, and can also specify \
          the name of the package to submit when that is not known from its \
          metadata.";
      `P "The $(i,opam-ci-check-lint) tool is run on the packages by default \
          when submitting to $(i,ocaml/opam-repository). Use the \
          $(b,--opam-ci-lint) option to run the tool when submitting to \
          other repositories. Any lint errors will prevent the submission, \
          unless $(b,--force) is used.";
      `S "OPTIONS";
      `S "METADATA GATHERING";
      `S "SUBMITTING";
      `S "LIMITATIONS AND BUGS";
      `P "Packages with auxiliary files (i.e. with definitions containing a \
          $(i,files/) subdirectory) are not currently supported. Removing \
          packages is also not supported at the moment.";
      `P "Bugs can be reported at \
          $(i,https://github.com/ocaml/opam-publish/issues)";
      `S "AUTHORS";
      `P "Louis Gesbert https://github.com/AltGr";
      `P "David Sheets <sheets@alum.mit.edu>";
      `P "Jeremie Dimino <jdimino@janestreet.com>";
      `S "LICENSE";
      `P "Copyright (C) 2014-2017 OCamlPro.";
      `P "This is free software, released under the terms of the GNU Lesser \
          General Public License version 2.1, with a special exception on \
          linking. The text can be found in the file `LICENSE' distributed \
          with the sources."
    ]

let () =
  OpamSystem.init ();
  let opam_root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  let _ : OpamFile.Config.t option =
    OpamStateConfig.load_defaults ~lock_kind:`Lock_none opam_root
  in
  OpamCoreConfig.init ();
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ~root_dir:opam_root ();
  let publish_root = OpamFilename.Op.(opam_root / "plugins" / "opam-publish") in
  let main_command = Cmd.v main_info (main_term publish_root) in
  try
    match Cmd.eval_value ~catch:false main_command with
    | Ok (`Ok () | `Version | `Help) -> OpamStd.Sys.exit_because `Success
    | Error _ -> OpamStd.Sys.exit_because `Bad_arguments
  with
  | OpamStd.Sys.Exit i -> exit i
  | Failure e ->
    OpamConsole.error "%s" e;
    exit (List.assoc `Internal_error OpamStd.Sys.exit_codes)
  | e ->
    OpamConsole.error "Uncaught exception: %s" (Printexc.to_string e);
    exit (List.assoc `Internal_error OpamStd.Sys.exit_codes)
