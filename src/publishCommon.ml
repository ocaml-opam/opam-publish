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
open OpamStd.Option.Op
open OpamProcess.Job.Op

let git_command ?dir ?(verbose=false) args =
  let dir = dir >>| OpamFilename.Dir.to_string in
  OpamProcess.Job.run @@
  OpamSystem.make_command ~verbose ?dir "git" args @@> fun r ->
  OpamSystem.raise_on_process_error r;
  Done ()

let git_query ?dir args =
  let dir = dir >>| OpamFilename.Dir.to_string in
  OpamProcess.Job.run @@
  OpamSystem.make_command ~verbose:false ?dir "git" args @@> fun r ->
  if OpamProcess.is_success r then
    match r.OpamProcess.r_stdout with
    | r :: _ -> Done (Some r)
    | [] -> Done None
  else Done None

let repo_of_dir ?(remote="origin") dir =
  let gh_re =
    Re.(compile @@
        seq [
          alt [str "git@github.com:";
               seq [str "http"; opt (char 's'); str "://"]];
          group (rep1 (diff any (char '/')));
          char '/';
          group (non_greedy (rep1 (diff any (char '/'))));
          opt (str ".git");
          eos;
        ])
  in
  git_query ~dir ["config"; "--get"; "remote."^remote^".url"] >>=
  Re.exec_opt gh_re >>= fun g ->
  Some (Re.Group.get g 1, Re.Group.get g 2)

(* let user_of_dir dir = fst (repo_of_dir ~remote:"user" dir) *)

let tag_of_dir dir =
  git_query ~dir ["describe"; "--tags"; "--abbrev=0"] >>| fun tag ->
  tag
