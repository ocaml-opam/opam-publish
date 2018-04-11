(**************************************************************************)
(*                                                                        *)
(*    Copyright 2014-2017 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op
open PublishCommon

let reset_terminal : (unit -> unit) option ref = ref None
let () =
  at_exit @@ fun () ->
  match !reset_terminal with None -> () | Some f -> f ()

(* -- Submit command -- *)

let (/) a b = String.concat "/" [a;b]

let github_root = "git@github.com:"

type github_repo = string * string (* owner, name *)

let repo_dir root (repo_owner, repo_name) =
  OpamFilename.Op.(root / "repos" / (repo_owner ^ "%" ^ repo_name))

let print_package_list sep = function
  | nv::r as packages when
      List.for_all (fun p -> OpamPackage.version p = OpamPackage.version nv) r
    ->
    String.concat sep (List.map OpamPackage.name_to_string packages) ^ "." ^
    OpamPackage.version_to_string nv
  | packages ->
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

  let recent_otp = ref None
  let complete_2fa user c =
    let rec try_again f = Monad.(f () >>~ function
    | Result auths -> return auths
    | Two_factor _ when !recent_otp <> None ->
      recent_otp := None;
      try_again f
    | Two_factor mode ->
      let otp = OpamConsole.read "%s 2FA code from '%s':" user mode in
      recent_otp := otp;
      try_again (c ?otp)
    ) in
    let otp = !recent_otp in
    try_again (c ?otp)

  let get_user token = Lwt_main.run @@ Monad.(
    Lwt.catch (fun () -> run (
      User.current_info ~token ()
      >>~ fun u -> return (Some u.Github_t.user_info_login)
    )) (function
      | Message (`Unauthorized, _) -> Lwt.return None
      | exn -> Lwt.fail exn
    )
  )

  let rec get_user_token root (repo_owner, repo_name) =
    let tok_file =
      OpamFilename.Op.(root // (repo_owner ^ "%" ^ repo_name ^ ".token"))
    in
    let exists = OpamFilename.exists tok_file in
    let exists = (* Restore token from previous versions *)
      exists ||
      match
        List.filter (OpamFilename.ends_with ".token") (OpamFilename.files root)
      with
      | [f] when
          not (OpamStd.String.contains_char
                 (OpamFilename.(Base.to_string (basename f))) '%') ->
        OpamFilename.move ~src:f ~dst:tok_file; true
      | _ -> false
    in
    if exists then
      let token = Token.of_string (OpamFilename.read tok_file) in
      match get_user token with
      | Some u -> u, token
      | None ->
        OpamConsole.msg "\nExisting Github token is no longer valid (%s).\n"
          (OpamFilename.prettify tok_file);
        OpamFilename.remove tok_file;
        get_user_token root (repo_owner, repo_name)
    else
    let user, token =
      if
        OpamConsole.confirm
          "No Github token found. Should I generate one ?\n\
           (your credentials will be required. If you do not have a Github \
           account, you can create one at https://github.com/join)"
      then
        let hostname = Unix.gethostname () in
        let token_note = token_note hostname in
        OpamConsole.msg
          "The obtained token will be stored in %s.\n\
           Your active tokens can be seen and revoked at \
           https://github.com/settings/tokens\n\n"
          (OpamFilename.prettify tok_file);
        let user =
          let rec get_u () =
            match OpamConsole.read "Please enter your Github name:" with
            | Some u -> u
            | None -> get_u ()
          in
          get_u ()
        in
        let pass =
          let rec get_pass () =
            match OpamConsole.read "%s password:" user with
            | Some p -> p
            | None -> get_pass ()
          in
          no_stdin_echo get_pass
        in
        let open Github.Monad in
        let create_token () =
          complete_2fa user
            (fun ?otp () ->
               Token.create ~scopes:[`Public_repo] ~user ~pass ~note:token_note
                 ?otp ())
        in
        Lwt_main.run @@ Monad.run @@
        (complete_2fa user (Token.get_all ~user ~pass)
         >>= fun auths ->
         (try
            let auth = List.find (fun a ->
                a.Github_t.auth_note = Some token_note)
                auths
            in
            OpamConsole.msg "Remote token for %s already exists. Resetting.\n\n"
              hostname;
            complete_2fa user
              (Token.delete ~user ~pass ~id:auth.Github_t.auth_id)
            >>= fun () ->
            create_token ()
          with Not_found -> create_token ())
         >>= fun auth ->
         (user, Token.of_auth auth) |> Monad.return)
      else
      match
        OpamConsole.read
          "Ok, then please generate a token from \
           https://github.com/settings/tokens\n\
           It needs access to the 'public_repo' scope ('repo' if you submit to \
           a private repository).\n\
           Auth token:"
      with
      | None -> OpamStd.Sys.exit_because `Aborted
      | Some stok ->
        let tok = Token.of_string (OpamStd.String.strip stok) in
        match get_user tok with
        | Some u -> u, tok
        | None ->
          OpamConsole.msg "Sorry, this token does not appear to be valid.\n";
          get_user_token root (repo_owner, repo_name)
    in
    let tok_file = OpamFilename.to_string tok_file in
    OpamFilename.mkdir root;
    let tok_fd = Unix.(openfile tok_file [O_CREAT; O_TRUNC; O_WRONLY] 0o600) in
    let tok_oc = Unix.out_channel_of_descr tok_fd in
    output_string tok_oc (Token.to_string token);
    close_out tok_oc;
    let { Unix.st_perm; _ } = Unix.stat tok_file in
    let safe_perm = 0o7770 land st_perm in
    begin if safe_perm <> st_perm
      then Unix.chmod tok_file safe_perm
    end;
    user, token

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

let init_mirror root repo user token =
  let dir = repo_dir root repo in
  OpamFilename.mkdir dir;
  OpamConsole.msg
    "Cloning the package repository, this may take a while...\n";
  git_command ~verbose:true
    ["clone";
     github_root^(fst repo)/(snd repo)^".git";
     OpamFilename.Dir.to_string dir];
  GH.fork token repo;
  git_command ~dir ["remote"; "add"; "user"; github_root^user/(snd repo)]

let update_mirror root repo branch =
  let dir = repo_dir root repo in
  OpamConsole.msg "Fetching the package repository, this may take a while...\n";
  git_command ~dir ["fetch"; "--multiple"; "origin"; "user"];
  git_command ~dir ["reset"; "origin"/branch; "--hard"]

let repo_package_dir repo_dir package =
  OpamFilename.Op.(
    repo_dir /
    "packages" /
    OpamPackage.Name.to_string (OpamPackage.name package) /
    OpamPackage.to_string package
  )

let repo_opam repo_dir package =
  OpamFilename.Op.(repo_package_dir repo_dir package // "opam")

let add_files_and_pr
    root ?(dry_run=false) repo user token title message
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
    git_command ~dir:mirror
      ["push"; "user"; "+HEAD:"^branch]
  in
  OpamFilename.in_dir mirror (fun () -> ignore (Sys.command "git show HEAD"));
  if dry_run then OpamStd.Sys.exit_because `Success;
  if not (OpamConsole.confirm "\nFile a pull-request for this patch ?") then
    OpamStd.Sys.exit_because `Aborted;
  let url =
    GH.pull_request title user token repo ~text:message branch target_branch
  in
  OpamConsole.msg "Pull-requested: %s\n" url;
  try
    let auto_open =
      if OpamStd.Sys.(os () = Darwin) then "open" else "xdg-open"
    in
    OpamSystem.command [auto_open; url]
  with OpamSystem.Command_not_found _ -> ()

let submit root ?dry_run repo target_branch title msg packages files =
  (* Prepare the repo *)
  let mirror_dir = repo_dir root repo in
  let user, token =
    if not (OpamFilename.exists_dir mirror_dir) then
      let user, token = GH.get_user_token root repo in
      init_mirror root repo user token;
      user, token
    else
    let user, token = GH.get_user_token root repo in
    user, token
  in
  (* pull-request processing *)
  update_mirror root repo target_branch;
  let branch = user_branch packages in
  add_files_and_pr root ?dry_run repo user token title msg branch target_branch files
