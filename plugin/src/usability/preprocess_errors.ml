open Utilities
open CErrors
open Names
open Himsg

(* 
 * Error messages
 *)

(* --- Error descriptions --- *)

let err_mutual_recursion = Pp.str "Mutual recursion is not supported."
let err_corecursion = Pp.str "Corecursion is not supported."
let err_functor = Pp.str "Functors are not supported."
                         
let err_opaque_not_constant qid =
  Pp.seq
    [Pp.str "The identifier ";
     Libnames.pr_qualid qid;
     Pp.str " that was passed to the { opaque ... } option is not a constant,";
     Pp.str " or does not exist."]

let err_type env sigma err =
  Pp.seq
    [Pp.str "Preprocess tried to produce a term that is not well-typed. ";
     Pp.str "Coq gave us this scary looking error:\n";
     Pp.fnl ();
     explain_pretype_error env sigma err;
     Pp.fnl ();
     Pp.fnl ();
     Pp.str "This is often due to one of two issues:\n";
     Pp.str "1. the term refers to an earlier term that is opaque, or\n";
     Pp.str "2. you need to generate a different induction principle."]

(* --- Possible workaround suggestions --- *)

let try_opaque = Pp.str "skipping this term using the { opaque ... } option"
let try_change_ind = Pp.str "generating a different induction principle"
let try_defs_only = Pp.str "preprocessing only the definitions you need"
let try_check_typos = Pp.str "checking for typos"
let try_fully_qualify = Pp.str "fully qualifying the identifier"
let try_alias = Pp.str "defining an alias for this term if it is your code"

let workaround suggestions =
  Pp.seq
    [Pp.str "To get around this, consider ";
     Pp.prlist_with_sep (fun _ -> Pp.str ", or ") id suggestions;
     Pp.str "."]

(* --- Suggestion to read the FAQ --- *)

let read_faq =
  Pp.str "Please see the README in uwplse/fix-to-elim for more information."

(* --- Reasons to cut an issue --- *)

let cool_feature = Pp.str "you really want this feature"
let problematic = Pp.str "this continues to cause you trouble"
         
let cut_issue reasons =
  Pp.seq
    [Pp.str "If ";
     Pp.prlist_with_sep (fun _ -> Pp.str ", or if ") id reasons;
     Pp.str ", then please cut an issue in the uwplse/fix-to-elim repository."]

(* --- Putting these together --- *)
    
(*
 * Our own user_err function to make it easier to present nice information
 * to the user
 *)
let user_err hdr err suggestions reasons =
  user_err
    ~hdr:hdr
    (Pp.prlist_with_sep
       Pp.spc
       id
       [err; workaround suggestions; read_faq; cut_issue reasons])
       
