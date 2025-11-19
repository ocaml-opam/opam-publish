(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014-2017 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open PublishCommon

let reset_terminal : (unit -> unit) option ref = ref None
let () =
  at_exit @@ fun () ->
  match !reset_terminal with None -> () | Some f -> f ()

(* -- Submit command -- *)

let (/) a b = String.concat "/" [a;b]

let github_root = "https://github.com/"

type github_repo = string * string (* owner, name *)

let repo_dir root (repo_owner, repo_name) =
  OpamFilename.Op.(root / "repos" / (repo_owner ^ "%" ^ repo_name))

let print_package_list sep packages =
  let packages =
    (* Order from smallest package name to largest *)
    List.sort OpamPackage.compare packages |>
    (* Then eliminate the packages that have the same version leaving only the smallest names *)
    List.sort_uniq (fun p1 p2 ->
        let v1 = OpamPackage.Version.to_string (OpamPackage.version p1) in
        let v2 = OpamPackage.Version.to_string (OpamPackage.version p2) in
        String.compare v1 v2)
  in
  String.concat sep (List.map OpamPackage.to_string packages)

let user_branch packages =
  String.map (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' as c -> c
      | _ -> '-'
    ) @@
  "opam-publish" / print_package_list "+" packages

module GH = struct
  open Lwt
  open Github

  type user = {
    name : string;
    email : string;
    login : string;
  }

  let token_note hostname = "opam-publish access token ("^hostname^")"

  let no_stdin_echo f =
    let reset_terminal : (unit -> unit) option ref = ref None in
    let () =
      at_exit @@ fun () ->
      match !reset_terminal with None -> () | Some f -> f ()
    in
    let open Unix in
    let attr = tcgetattr stdin in
    let reset () = tcsetattr stdin TCSAFLUSH attr in
    reset_terminal := Some reset;
    tcsetattr stdin TCSAFLUSH
      { attr with
        c_echo = false; c_echoe = false; c_echok = false; c_echonl = true; };
    let v = f () in
    reset ();
    reset_terminal := None;
    v

  let get_gh_scopes token =
    Lwt.catch
      (fun () ->
         let headers =
           Cohttp.Header.of_list [
             ("Authorization", "token "^Token.to_string token);
           ]
         in
         let api_uri = Uri.of_string "https://api.github.com" in
         Cohttp_lwt_unix.Client.get ~headers api_uri >>= fun (resp, body) ->
         Cohttp_lwt.Body.drain_body body >>= fun () ->
         let headers = Cohttp.Header.to_list (Cohttp.Response.headers resp) in
         let headers = List.map (fun (k, x) -> (String.lowercase_ascii k, x)) headers in
         Lwt.return (OpamStd.List.assoc_opt String.equal "x-oauth-scopes" headers))
      (fun _exn -> Lwt.return None)

  let get_user token =
    Lwt_main.run @@
    Lwt.catch (fun () ->
        Monad.run (
          let open Monad in
          let open OpamStd.Option.Op in
          User.current_info ~token () >>~ fun u ->
          let login = u.Github_t.user_info_login in
          let name = u.Github_t.user_info_name +! login in
          let email = u.Github_t.user_info_email +! login ^ "@opam-publish" in
          return { name; email; login }
        ) >>= fun r ->
        get_gh_scopes token >>= function
        | Some scopes ->
          let scopes = List.map String.trim (String.split_on_char ',' scopes) in
          let scopes = List.filter (fun x -> x <> "") scopes in
          if (List.mem "public_repo" scopes || List.mem "repo" scopes) &&
             List.mem "workflow" scopes then
            Lwt.return (Ok r)
          else
            Lwt.return (Error (`Missing_scope (r, scopes)))
        | None ->
          OpamConsole.warning "Unable to check the list of permissions of the given token. Ignoring.";
          Lwt.return (Ok r)
    ) (function
      | Message (`Unauthorized, _) -> Lwt.return (Error `Unauthorized)
      | exn -> Lwt.fail exn
    )

  let prompt_for_token expected_scopes =
    OpamConsole.msg
      "Please generate a Github token at \
       https://github.com/settings/tokens/new to allow access.\n\
       The %s scopes are required \
       (\"repo\" if submitting to a private opam repository).\n\n"
      (OpamStd.Format.pretty_list expected_scopes);
    let rec get_pass () =
      match
        OpamConsole.read "Please enter your GitHub personal access token:"
      with
      | Some p -> p
      | None -> get_pass ()
    in
    let input = no_stdin_echo get_pass in
    Token.of_string (OpamStd.String.strip input)

  let save_token_file root tok_file token =
    OpamConsole.msg
      "The token will be stored in %s.\n"
      (OpamFilename.prettify tok_file);
    let tok_file = OpamFilename.to_string tok_file in
    OpamFilename.mkdir root;
    let tok_fd = Unix.(openfile tok_file [O_CREAT; O_TRUNC; O_WRONLY] 0o600) in
    let tok_oc = Unix.out_channel_of_descr tok_fd in
    output_string tok_oc (Token.to_string token);
    close_out tok_oc;
    let { Unix.st_perm; _ } = Unix.stat tok_file in
    let safe_perm = 0o7770 land st_perm in
    if safe_perm <> st_perm then
      Unix.chmod tok_file safe_perm

  let get_user_token ~cli_token root (repo_owner, repo_name) =
    let expected_scopes = ["\"public_repo\""; "\"workflow\""] in
    let tok_file =
      OpamFilename.Op.(root // (repo_owner ^ "%" ^ repo_name ^ ".token"))
    in
    let max_attempts = 3 in
    let rec loop ~attempts_left ~cli_token =
      if attempts_left <= 0 then
        Format.ksprintf failwith
          "Could not obtain a valid GitHub token after %d attempts."
          max_attempts;
      let exists = OpamFilename.exists tok_file in
      let exists = (* Restore token from previous versions *)
        exists ||
        match
          List.filter (OpamFilename.ends_with ".token") (OpamFilename.files root)
        with
        | [f] when
            not (String.contains
                   (OpamFilename.(Base.to_string (basename f))) '%') ->
          OpamFilename.move ~src:f ~dst:tok_file; true
        | _ -> false
      in
      if exists then
        let token = Token.of_string (OpamFilename.read tok_file) in
        let user =
          match get_user token with
          | Ok u -> Ok u
          | Error `Unauthorized ->
            OpamConsole.msg "\nExisting Github token is no longer valid (%s).\n"
              (OpamFilename.prettify tok_file);
            OpamFilename.remove tok_file;
            Error ()
            (* get_user_token ~cli_token root (repo_owner, repo_name) *)
          | Error (`Missing_scope (u, scopes)) ->
            OpamConsole.msg "\nExisting Github token doesn't have the expected \
                             scopes (expected %s, but got %s).\n"
              (OpamStd.Format.pretty_list expected_scopes)
              (if scopes = [] then "none" else OpamStd.Format.pretty_list scopes);
            if OpamConsole.confirm ~default:false "Do you want to use it anyway?"
            then Ok u
            else (
              OpamFilename.remove tok_file;
              Error () )
        in
        match user with
        | Ok user -> user, token
        | Error () -> loop ~attempts_left:(attempts_left - 1) ~cli_token
      else
      let token =
        match cli_token with
        | Some token -> Token.of_string token
        | None -> prompt_for_token expected_scopes
      in
      let user =
        match get_user token with
        | Ok u -> Ok u
        | Error `Unauthorized ->
          OpamConsole.msg "Sorry, this token does not appear to be valid.\n";
          Error ()
        | Error (`Missing_scope (u, scopes)) ->
          OpamConsole.msg "Sorry, this token doesn't have the expected scopes \
                           (expected %s, but got %s).\n"
            (OpamStd.Format.pretty_list expected_scopes)
            (OpamStd.Format.pretty_list scopes);
          if OpamConsole.confirm ~default:false "Do you want to use it anyway?"
          then Ok u
          else Error ()
      in
      match user with
      | Ok user ->
        save_token_file root tok_file token;
        user, token
      | Error () ->
        (* If the user gave a bad cli token we cannot prompt/recurse *)
        if Option.is_some cli_token then
          failwith "Aborting due to invalid command-line token."
        else
          loop ~attempts_left:(attempts_left - 1) ~cli_token:None
    in
    loop ~attempts_left:max_attempts ~cli_token

  let fork token repo =
    let check uri =
      let not_found = API.code_handler ~expected_code:`Not_found (fun _ ->
        OpamConsole.log "PUBLISH" "Check for fork failed: not found";
        return_false
      ) in
      API.get ~fail_handlers:[not_found] ~expected_code:`OK ~token ~uri
        (fun _ -> return_true)
    in
    let rec until ?(n=0) f x () = Monad.(
      f x >>~ function
      | true ->
        if n > 0 then OpamConsole.msg "\n";
        return ()
      | false ->
        if n=0 then
          OpamConsole.msg "Waiting for GitHub to register the fork..."
        else if n<20 then
          OpamConsole.msg "."
        else
          failwith "GitHub fork timeout";
        embed (Lwt_unix.sleep 1.5) >>= until ~n:(n+1) f x
    ) in
    Lwt_main.run Monad.(run (
      Repo.fork ~token ~user:(fst repo) ~repo:(snd repo) ()
      >>~ fun { Github_t.repository_url = uri; _ } ->
      until check (Uri.of_string uri) ()
    ))

  let pull_request title user token repo ?text branch target_branch =
    let pull = {
      Github_t.
      new_pull_title = title;
      new_pull_base = target_branch;
      new_pull_head = user^":"^branch;
      new_pull_body = text;
    } in
    let update_pull = {
      Github_t.
      update_pull_title = Some pull.Github_t.new_pull_title;
      update_pull_body = pull.Github_t.new_pull_body;
      update_pull_state = None;
      update_pull_base = None;
    } in
    let open Github.Monad in
    let existing () =
      let pulls = Pull.for_repo ~token ~user:(fst repo) ~repo:(snd repo) () in
      Stream.find Github_t.(fun p ->
        (match p.pull_head.branch_user with
         | None -> false | Some u -> u.user_login = user) &&
        p.pull_head.branch_ref = branch &&
        p.pull_state = `Open
      ) pulls
    in
    let pr =
      Response.value @@ Lwt_main.run @@ Monad.run @@
      (existing () >>= function
        | None ->
          Pull.create ~token ~user:(fst repo) ~repo:(snd repo) ~pull ()
        | Some (p,_) ->
          let num = p.Github_t.pull_number in
          OpamConsole.msg "Updating existing pull-request #%d\n" num;
          Pull.update
            ~token ~user:(fst repo) ~repo:(snd repo) ~update_pull ~num
            ())
    in
    pr.Github_t.pull_html_url

end

let configure_user ~dir user =
  let open OpamStd.Option.Op in
  let name = git_query ~dir ["config"; "--get"; "user.name"] >>| String.trim in
  let email = git_query ~dir ["config"; "--get"; "user.email"] >>| String.trim in
  match (name, email) with
  | (None, None) | (Some "", Some "") ->
    git_command ~dir ["config"; "user.email"; user.GH.email];
    git_command ~dir ["config"; "user.name"; user.GH.name]
  | ((None | Some ""), Some _) ->
    git_command ~dir ["config"; "user.name"; user.GH.name]
  | (Some _, (None | Some "")) ->
    git_command ~dir ["config"; "user.email"; user.GH.email]
  | Some _, Some _ ->
    (* If both are set we probably want to use the configured name and email *)
    ()

(** Constructs a repository url for https push authentication.
    The format is: https://<token>@github.com/<user>/<repo_name> *)
let make_authenticated_https_url repo ~user ~token =
  let token = Github.Token.to_string token in
  Format.sprintf "https://%s@github.com/%s/%s" token user.GH.login (snd repo)

let init_mirror root repo user token =
  let dir = repo_dir root repo in
  if OpamFilename.exists_dir dir then
    OpamFilename.rmdir dir;
  OpamFilename.mkdir dir;
  OpamConsole.msg
    "Cloning the package repository, this may take a while...\n";
  git_command ~verbose:true
    ["clone";
     github_root^(fst repo)/(snd repo)^".git";
     OpamFilename.Dir.to_string dir];
  GH.fork token repo;
  configure_user ~dir user;
  let remote = make_authenticated_https_url repo ~user ~token in
  git_command ~dir ["remote"; "add"; "user"; remote]

let update_mirror root repo ~user ~token branch =
  let dir = repo_dir root repo in
  OpamConsole.msg "Fetching the package repository, this may take a while...\n";
  let aux () =
    git_command ~dir ["fetch"; "--multiple"; "origin"; "user"];
    git_command ~dir ["reset"; "origin"/branch; "--hard"];
  in
  try
    (* Updates user remote to make sure we use https token authentication. *)
    let https_url = make_authenticated_https_url repo ~user ~token in
    git_command ~dir ["remote"; "set-url"; "user"; https_url];
    aux ()
  with OpamSystem.Process_error _ ->
    OpamConsole.msg "Command failed. Trying one more time on a clean slate...\n";
    init_mirror root repo user token;
    aux ()

let add_files_and_pr
    root ~dry_run ~output_patch ~no_browser repo user token title message
    branch target_branch files =
  let mirror = repo_dir root repo in
  let () =
    List.iter (fun (rel_path, contents) ->
        match contents with
        | None ->
          if Sys.file_exists
              (Filename.concat (OpamFilename.Dir.to_string mirror) rel_path)
          then git_command ~dir:mirror ["rm"; "-r"; rel_path]
        | Some (contents, perms) ->
          let file = OpamFilename.Op.(mirror // rel_path) in
          OpamFilename.mkdir (OpamFilename.dirname file);
          OpamFilename.write file contents;
          OpamFilename.chmod file perms;
          git_command ~dir:mirror ["add"; rel_path])
      files;
    git_command ~dir:mirror
      ["commit"; "-m"; title];
    if not dry_run then
      git_command ~dir:mirror
        ["push"; "user"; "+HEAD:"^branch]
  in
  begin match output_patch with
    | None ->
      OpamFilename.in_dir mirror @@ fun () ->
      let _ : int = Sys.command "git show HEAD" in
      ()
    | Some out ->
      OpamFilename.in_dir mirror @@ fun () ->
      let cmd =
        Printf.sprintf "git format-patch HEAD^ --stdout > %S"
          (OpamFilename.to_string out)
      in
      let _ : int = Sys.command cmd in
      OpamConsole.msg
        "Patch file to be applied on %s/%s was written to %S\n"
        (fst repo) (snd repo) (OpamFilename.to_string out)
  end;
  if output_patch <> None || dry_run then OpamStd.Sys.exit_because `Success;
  if not (OpamConsole.confirm ~require_unsafe_yes:true
            "\nFile a pull-request for this patch ?") then
    OpamStd.Sys.exit_because `Aborted;
  let url =
    GH.pull_request title user.GH.login token repo ~text:message branch target_branch
  in
  OpamConsole.msg "Pull-requested: %s\n" url;
  OpamConsole.msg "You can re-run this command to update the pull-request.\n";
  if not no_browser then begin
    try
      let auto_open =
        if OpamStd.Sys.(os () = Darwin) then "open" else "xdg-open"
      in
      OpamSystem.command [auto_open; url]
    with OpamSystem.Command_not_found _ -> ()
  end

let submit
    root ~token ~dry_run ~output_patch ~no_browser
    repo target_branch title msg packages files =
  (* Prepare the repo *)
  let mirror_dir = repo_dir root repo in
  let user, token =
    if not OpamFilename.(exists_dir Op.(mirror_dir / ".git" )) then
      let user, token = GH.get_user_token ~cli_token:token root repo in
      init_mirror root repo user token;
      user, token
    else
    let user, token = GH.get_user_token ~cli_token:token root repo in
    user, token
  in
  (* pull-request processing *)
  update_mirror root repo ~user ~token target_branch;
  let branch = user_branch packages in
  add_files_and_pr root ~dry_run ~output_patch ~no_browser
    repo user token title msg branch target_branch files
