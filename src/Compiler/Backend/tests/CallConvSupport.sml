(* Minimal identities/logging for testing the production CallConv module. *)
structure Lvars = struct type lvar = int val pr_lvar = Int.toString end
structure BackendInfo = struct end
structure Flags = struct val log = ref TextIO.stdOut end
structure Crash = struct fun impossible s = raise Fail s end
